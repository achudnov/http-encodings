module Network.HTTP.Encoding.Error where

data EncodingError = CannotDetermineCharacterEncoding
                   | UnsupportedCompressionAlgorithm
                   | CharsetConversionError
                     
instance Show EncodingError where
  show err = case err of 
    CannotDetermineCharacterEncoding ->
      "No character encoding was specified in message headers \
      \and the body character encoding cannot be determined"
    UnsupportedCompressionAlgorithm ->  
      "Sorry, the 'compress' algorithm is not supported at this time"
    CharsetConversionError ->   
      "Charset conversion error in text-icu"
     
                     
