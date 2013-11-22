module Network.HTTP.Encoding (decodeBody
                             ,decode
                             ,encodeBody
                             ,encode
                             ,withDecodedBody
                             ,withDecodedBodyM
                             ,HasBody(..)
                             ,EncodingError
                             ,DecodingResult(..)) where

import Network.HTTP.Encoding.Content
import Network.HTTP.Encoding.Character
import Network.HTTP.Encoding.Error
import Network.HTTP
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Control.Applicative
import Data.Text.ICU.Detect
import Data.Text.ICU.Convert
import Data.Text

targetEncoding = "UTF-8"

class HasBody a where
  getBody :: a b -> b
  setBody :: c -> a b -> a c

instance HasBody Request where
  getBody = rqBody
  setBody body rq = rq {rqBody = body}

instance HasBody Response where
  getBody = rspBody
  setBody body rsp = rsp {rspBody = body}

-- | The result of decoding a message body
data DecodingResult = DecodingResult {decodedBody :: Text
                                     ,originalEncoding :: String
                                     }

-- | Decodes and decompresses the response or request body using the
-- information in the headers and content and possibly returns the
-- body in UTF8
decodeBody :: (HasHeaders (r LBS.ByteString), HasBody r)
           => r LBS.ByteString 
           -> Either EncodingError DecodingResult
decodeBody r =
  let headers    = getHeaders r
      body       = getBody r
      contentEnc = getContentEncoding headers
      decodeBody2 :: String -> IO (Either EncodingError DecodingResult)
      decodeBody2 enc =
        do dbody <- lift $ decompress contentEnc body
           conv  <- open enc Nothing
           decBody <- toUnicode conv $ LBS.toStrict dbody
           return $ Right $ DecodingResult {decodedBody = decBody
                                           ,originalEncoding = enc}
  in case detect body of
    Nothing -> Left CannotDetermineCharacterEncoding
    Just charEnc -> decodeBody2 $ show charEnc
   -- case snd $ getContentTypeAndCharacterEncoding headers of
   --   Nothing -> case detect body of
   --     Nothing -> Left CannotDetermineCharacterEncoding
   --     Just charEnc -> decodeBody2 $ show charEnc
   --   Just charEnc -> decodeBody2 charEnc
    
flipEither :: Either a b -> Either b a
flipEither (Left  x) = Right x
flipEither (Right x) = Left  x

decode :: (HasHeaders (r LBS.ByteString), HasHeaders (r String), HasBody r) 
       => r LBS.ByteString
       -> Either EncodingError (String, r String)
decode r = do res <- decodeBody r
              let hdrs = updateContentEncoding IdentityCompression (getHeaders r)
                  hdrs2= setCharacterEncoding (originalEncoding res) hdrs 
              return (originalEncoding res
                     ,flip setHeaders hdrs $ setBody (decodedBody res) r)
       
-- |Takes a haskell UTF8-encoded string and produces a stream, encoded
-- and compressed
encodeBody :: String 
           -> ContentEncoding
           -> Text -- ^ Decoded body
           -> Either EncodingError LBS.ByteString
encodeBody source_enc ce dbody =
  do conv  <- open source_enc Nothing
     body <- fromUnicode conv dbody
     compress ce body

encode :: (HasHeaders (r String), HasBody r)
       => String -> r String -> Either EncodingError (r LBS.ByteString)
encode ch_enc r = 
  let headers = getHeaders r
      body    = getBody r
  in  let ce = getContentEncoding headers in
      do ebody <- encodeBody ch_enc ce body
         return $ setBody ebody r

either2Maybe (Left x) = Just x
either2Maybe (Right _) = Nothing

withDecodedBody :: (HasHeaders (r String), HasHeaders (r LBS.ByteString), HasBody r) 
                => (String -> String)
                -> r LBS.ByteString
                -> Either EncodingError (r LBS.ByteString)
withDecodedBody f r = 
  do (enc, dr) <- decode r
     let mdr = setBody (f $ getBody dr) dr
     encode enc mdr
     
withDecodedBodyM :: (Monad m, HasHeaders (r String), HasHeaders (r LBS.ByteString), 
                     HasBody r)
                 => (String -> m String)
                 -> r LBS.ByteString
                 -> m (Either EncodingError (r LBS.ByteString))
withDecodedBodyM f r =
  case decode r of
    Left err -> return $ Left err
    Right (enc, dr) -> f (getBody dr) >>= \mbody ->
      case encode enc $ setBody mbody dr of
        Left err -> return $ Left err
        Right mr -> return $ Right mr
