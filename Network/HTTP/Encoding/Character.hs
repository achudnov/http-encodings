module Network.HTTP.Encoding.Character 
       (getContentTypeAndCharacterEncoding
       ,setCharacterEncoding) where

import Network.HTTP
--import Codec.Text.IConv
import Codec.MIME.Parse
import Codec.MIME.Type
import Control.Applicative hiding (many, (<|>))
import Data.Char (toLower)

getContentTypeAndCharacterEncoding :: [Header] -> 
                                      (Maybe Type, Maybe String)
getContentTypeAndCharacterEncoding [] = (Nothing, Nothing)
getContentTypeAndCharacterEncoding (Header HdrContentType str:_) = 
  parseContentTypeHdr str
getContentTypeAndCharacterEncoding (_:hs) = getContentTypeAndCharacterEncoding hs

setCharacterEncoding :: String -> [Header] -> [Header]
setCharacterEncoding enc [] = [plainText enc]
setCharacterEncoding enc ((Header HdrContentType str):rest) =
  let (mtype, _) = parseContentTypeHdr str
      newhdr = case mtype of
        Nothing -> plainText enc
        Just ty -> Header HdrContentType $ showType $ setMIMEEncoding ty enc
  in newhdr:rest

plainText :: String -> Header
plainText enc = Header HdrContentType $ showType $
                Type {mimeType = (Text "plain")
                     ,mimeParams = [("charset", enc)]}

setMIMEEncoding :: Type -> String -> Type
setMIMEEncoding ty enc = ty {mimeParams = map replaceEnc $ mimeParams ty}
  where replaceEnc (pname, _) | pname == "charset" = (pname, enc)
        replaceEnc x = x

parseContentTypeHdr :: String -> (Maybe Type, Maybe String)
parseContentTypeHdr str = 
  case parseContentType str of
    Nothing    -> (Nothing, Nothing)
    Just ctype -> (Just ctype, 
                   (map toLower) <$> lookup "charset" (mimeParams ctype))
