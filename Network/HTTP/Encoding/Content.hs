module Network.HTTP.Encoding.Content(getContentEncoding, 
                                     updateContentEncoding, 
                                     decompress, 
                                     compress, 
                                     ContentEncoding(..)) where

import Network.HTTP
import Network.HTTP.Encoding.Error
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString.Lazy
import Data.Maybe

data ContentEncoding = GZip | Compress | Deflate | IdentityCompression

instance Show ContentEncoding where
    show GZip = "gzip"
    show Compress = "compress"
    show Deflate = "deflate"
    show IdentityCompression = ""

getContentEncoding :: [Header] -> ContentEncoding
getContentEncoding [] = IdentityCompression
getContentEncoding (Header HdrContentEncoding "gzip":hs) = GZip
getContentEncoding (Header HdrContentEncoding "x-gzip":hs) = GZip
getContentEncoding (Header HdrContentEncoding "compress":hs) = Compress
getContentEncoding (Header HdrContentEncoding "x-compress":hs) = Compress
getContentEncoding (Header HdrContentEncoding "deflate":hs) = Deflate
getContentEncoding (h:hs) = getContentEncoding hs

updateContentEncoding :: ContentEncoding -> [Header] -> [Header]
updateContentEncoding ce [] = maybeToList $ contentEncodingHeader ce
updateContentEncoding ce (Header HdrContentEncoding _:hs) = 
  hs ++ maybeToList (contentEncodingHeader ce)
updateContentEncoding ce (h:hs) = h:updateContentEncoding ce hs

contentEncodingHeader :: ContentEncoding -> Maybe Header
contentEncodingHeader IdentityCompression = Nothing
contentEncodingHeader ce = Just $ Header HdrContentEncoding (show ce)

decompress :: ContentEncoding -> ByteString -> Either EncodingError ByteString
decompress GZip bs = Right $ GZip.decompress bs
decompress Compress body = Left UnsupportedCompressionAlgorithm
decompress Deflate bs = Right $ Zlib.decompress bs
decompress IdentityCompression bs  = Right bs

compress :: ContentEncoding -> ByteString -> Either EncodingError ByteString
compress GZip body = Right $ GZip.compress body
compress Compress body = Left UnsupportedCompressionAlgorithm
compress Deflate body = Right $ Zlib.compress body
compress IdentityCompression rsp = Right rsp
