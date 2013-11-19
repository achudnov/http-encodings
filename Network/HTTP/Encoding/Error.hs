-- | Errors that may occur during decoding/encoding of HTTP message bodies
module Network.HTTP.Encoding.Error (EncodingError (..)
                                   ,ConversionError (..)) where

import Codec.Text.IConv (ConversionError
                        ,reportConversionError)

-- | Encoding/Decoding error message
data EncodingError = CannotDetermineCharacterEncoding
                     -- ^ Character decoding is not specified and
                     -- cannot be guessed
                   | UnsupportedCompressionAlgorithm
                     -- ^ A compression algorithm is not supported (LZW)
                   | IConvError ConversionError
                     -- ^ IConv conversion error
                   | GenericError String
                     -- ^ Other error
                     
instance Show EncodingError where
  show err = case err of 
    CannotDetermineCharacterEncoding ->
      "No character encoding was specified in message headers \
      \and the body character encoding cannot be determined"
    UnsupportedCompressionAlgorithm ->  
      "Sorry, the 'compress' algorithm is not supported at this time"
    IConvError conv_err ->   
      "Charset conversion error in iconv: " ++ 
      show (reportConversionError conv_err)
    GenericError err -> "Generic error: " ++ err
                     
