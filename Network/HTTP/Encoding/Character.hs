{-# LANGUAGE OverloadedStrings #-}

-- | Detection and of character encodings of HTTP message bodies
module Network.HTTP.Encoding.Character 
       (getContentTypeAndCharacterEncoding
       ,setCharacterEncoding
       ,tryAsUTF8) where

import Network.HTTP
import Codec.Text.IConv
import Codec.MIME.Parse
import Codec.MIME.Type
import Control.Applicative hiding (many, (<|>))
import Data.Char (toLower, ord)
import Data.ByteString.Lazy.UTF8 (uncons)
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Data.Text (Text, unpack, pack)
import qualified Data.Text as Text
import Data.Maybe (listToMaybe)

-- | Looks for and parses the ContentType header. Returns the
-- (optional) content-type and (optional) the character encoding name.
getContentTypeAndCharacterEncoding :: [Header] -> 
                                      (Maybe Type, Maybe EncodingName)
getContentTypeAndCharacterEncoding [] = (Nothing, Nothing)
getContentTypeAndCharacterEncoding (Header HdrContentType str:_) = 
  parseContentTypeHdr str
getContentTypeAndCharacterEncoding (_:hs) = getContentTypeAndCharacterEncoding hs

-- | Sets the given character encoding name in the given header. If
-- there is no content type header in the header list, it defaults to
-- the text/plain content type
setCharacterEncoding :: EncodingName -> [Header] -> [Header]
setCharacterEncoding enc [] = [plainText enc]
setCharacterEncoding enc ((Header HdrContentType str):rest) =
  let (mtype, _) = parseContentTypeHdr str
      newhdr = case mtype of
        Nothing -> plainText enc
        Just ty -> Header HdrContentType $ unpack $ showType $ setMIMEEncoding ty enc
  in newhdr:rest

plainText :: EncodingName -> Header
plainText enc = Header HdrContentType $ unpack $ showType $
                Type {mimeType = (Text "plain")
                     ,mimeParams = [MIMEParam {paramName = "charset"
                                              ,paramValue = pack enc}]}

setMIMEEncoding :: Type -> EncodingName -> Type
setMIMEEncoding ty enc = ty {mimeParams = map replaceEnc $ mimeParams ty}
  where replaceEnc param | paramName param == "charset" =
          param {paramValue = pack enc}
        replaceEnc x = x

parseContentTypeHdr :: String -> (Maybe Type, Maybe EncodingName)
parseContentTypeHdr str =
  case parseContentType (pack str) of
    Nothing    -> (Nothing, Nothing)
    Just ctype -> (Just ctype, 
                   (unpack . Text.map toLower . paramValue) <$> 
                   listToMaybe (filter (\p -> paramName p == "charset")
                                (mimeParams ctype)))

-- | Tries to decode a bytestring as UTF-8. Returns nothing if any
-- illegal characters are encountered
tryAsUTF8 :: ByteString -> Maybe String
tryAsUTF8 bs = uncons bs >>= \(c, bs') ->
  if (ord c == 0xFFFD) then mzero
  else tryAsUTF8 bs' >>= \s -> return (c:s)
                                                                   
