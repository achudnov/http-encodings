-- | Deals with content encoding (compression) of message bodies:
-- detection, update and compression/decompression
module Network.HTTP.Encoding.Content(ContentEncoding(..)
                                    ,getContentEncoding
                                    ,updateContentEncoding
                                    ,decompress
                                    ,compress
                                    ) where

import Network.HTTP
import Network.HTTP.Encoding.Error
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Data.ByteString.Lazy
import Data.Maybe

-- | Represents the content encoding, per the HTTP/1.1 standard.
data ContentEncoding = GZip | Compress | Deflate | IdentityCompression

instance Show ContentEncoding where
    show GZip = "gzip"
    show Compress = "compress"
    show Deflate = "deflate"
    show IdentityCompression = ""

-- | Determines the content encoding from a list of headers. Defaults
-- to 'IdentityCompression'
getContentEncoding :: [Header] -> ContentEncoding
getContentEncoding [] = IdentityCompression
getContentEncoding (Header HdrContentEncoding "gzip":hs) = GZip
getContentEncoding (Header HdrContentEncoding "x-gzip":hs) = GZip
getContentEncoding (Header HdrContentEncoding "compress":hs) = Compress
getContentEncoding (Header HdrContentEncoding "x-compress":hs) = Compress
getContentEncoding (Header HdrContentEncoding "deflate":hs) = Deflate
getContentEncoding (h:hs) = getContentEncoding hs

-- | Given the list of headers, updates content encoding to the
-- specified.
updateContentEncoding :: ContentEncoding -> [Header] -> [Header]
updateContentEncoding ce [] = maybeToList $ contentEncodingHeader ce
updateContentEncoding ce (Header HdrContentEncoding _:hs) = 
  hs ++ maybeToList (contentEncodingHeader ce)
updateContentEncoding ce (h:hs) = h:updateContentEncoding ce hs

contentEncodingHeader :: ContentEncoding -> Maybe Header
contentEncodingHeader IdentityCompression = Nothing
contentEncodingHeader ce = Just $ Header HdrContentEncoding (show ce)

-- | Decompresses a 'Bytestring' assuming a given content encoding. The
-- Compress encoding (LZW algorithm) is not supported at this time.
decompress :: ContentEncoding -> ByteString -> Either EncodingError ByteString
decompress GZip bs = Right $ GZip.decompress bs
decompress Compress body = Left UnsupportedCompressionAlgorithm
decompress Deflate bs = Right $ Zlib.decompress bs
decompress IdentityCompression bs  = Right bs

-- | Compresses a 'Bytestring' assuming a given content encoding. The
-- Compress encoding (LZW algorithm) is not supported at this time.
compress :: ContentEncoding -> ByteString -> Either EncodingError ByteString
compress GZip body = Right $ GZip.compress body
compress Compress body = Left UnsupportedCompressionAlgorithm
compress Deflate body = Right $ Zlib.compress body
compress IdentityCompression rsp = Right rsp
