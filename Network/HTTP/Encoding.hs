-- | Encoding and decoding of bodies and complete HTTP messages. See
-- package 'jespresso' for an example of usage.
module Network.HTTP.Encoding (-- * Operations on HTTP messages
                              decode
                             ,encode
                              -- * Operations on the bodies of HTTP messages
                             ,withDecodedBody
                             ,withDecodedBodyM                              
                             ,decodeBody
                             ,encodeBody
                              -- * Types
                             ,HasBody(..)
                             ,EncodingError
                             ,DecodingResult(..)) where

import Network.HTTP.Encoding.Content
import Network.HTTP.Encoding.Character
import Network.HTTP.Encoding.Error
import Network.HTTP
import Data.ByteString.Lazy
import Codec.Text.IConv
import Control.Applicative
import Data.ByteString.Lazy.UTF8 (fromString, toString)

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
data DecodingResult = DecodingResult {decodedBody :: String
                                     ,originalEncoding :: EncodingName
                                     }

-- | Decodes and decompresses the response or request body using the
-- information in the headers and content and possibly returns the
-- body in UTF8
decodeBody :: (HasHeaders (r ByteString), HasBody r)
           => r ByteString 
           -> Either EncodingError DecodingResult
decodeBody r =
  let headers    = getHeaders r
      body       = getBody r
      contentEnc = getContentEncoding headers
      decodeBody2 :: String -> Either EncodingError DecodingResult
      decodeBody2 enc =
        do dbody <- decompress contentEnc body
           x <- either (Right) (Left . IConvError)
                       (convertStrictly enc targetEncoding dbody)

           return $ DecodingResult {decodedBody = toString x
                                   ,originalEncoding = enc}
  in case snd $ getContentTypeAndCharacterEncoding headers of
    Nothing -> decodeBody2 "utf-8"
    Just charEnc -> decodeBody2 charEnc
    
flipEither :: Either a b -> Either b a
flipEither (Left  x) = Right x
flipEither (Right x) = Left  x

-- | Decode the body of an HTTP message and return the original
-- encoding name and the same message with decoded body (as
-- UTF8-encoded string) and updated character and content encoding
-- headers.
decode :: (HasHeaders (m ByteString), HasHeaders (m String), HasBody m) 
       => m ByteString
       -> Either EncodingError (String, m String)
decode r = do res <- decodeBody r
              let hdrs = updateContentEncoding IdentityCompression (getHeaders r)
                  hdrs2= setCharacterEncoding (originalEncoding res) hdrs 
              return (originalEncoding res
                     ,flip setHeaders hdrs $ setBody (decodedBody res) r)
       
-- |Takes a haskell UTF8-encoded string and produces a stream, encoded
-- and compressed
encodeBody :: EncodingName 
           -> ContentEncoding
           -> String
           -> Either EncodingError ByteString
encodeBody source_enc ce str =
  do body <- either Right
                    (Left . IConvError)
                    (convertStrictly targetEncoding source_enc (fromString str))
     compress ce body

-- | Encode the UTF8-encoded body of an HTTP message with the provided
-- encoding.
encode :: (HasHeaders (m String), HasBody m)
       => EncodingName -> m String -> Either EncodingError (m ByteString)
encode ch_enc r = 
  let headers = getHeaders r
      body    = getBody r
  in  let ce = getContentEncoding headers in
      do ebody <- encodeBody ch_enc ce body
         return $ setBody ebody r

either2Maybe (Left x) = Just x
either2Maybe (Right _) = Nothing

-- | Allows to lift a transformation function operating on decoded
-- (UTF-8) bodies to bodies of requests with encoded (and compressed)
-- bodies.
withDecodedBody :: (HasHeaders (r String), HasHeaders (r ByteString), HasBody r) 
                => (String -> String)
                -> r ByteString
                -> Either EncodingError (r ByteString)
withDecodedBody f r = 
  do (enc, dr) <- decode r
     let mdr = setBody (f $ getBody dr) dr
     encode enc mdr
     
-- | A monadic version of 'withDecodeBody'
withDecodedBodyM :: (Monad m, HasHeaders (r String), HasHeaders (r ByteString), 
                     HasBody r)
                 => (String -> m String)
                 -> r ByteString
                 -> m (Either EncodingError (r ByteString))
withDecodedBodyM f r =
  case decode r of
    Left err -> return $ Left err
    Right (enc, dr) -> f (getBody dr) >>= \mbody ->
      case encode enc $ setBody mbody dr of
        Left err -> return $ Left err
        Right mr -> return $ Right mr
