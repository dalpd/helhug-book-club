module SNP.Chapter3 where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.String (fromString)

-- | Exercise 10
-- A character encoding bug
-- >>> greet $ T.encodeUtf8 $ T.pack "Yiğit"
-- "Hello, Yi\USit"
greet :: ByteString -> ByteString
greet nameBS = case T.decodeUtf8' nameBS of
  Left _ -> fromString "Invalid bytestring"
  Right nameText -> fromString $ "Hello, " <> T.unpack nameText

-- | Exercise 11
-- >>> import Data.String (fromString)
-- >>> asciiUpper $ fromString "Hello! 123"
-- "HELLO! 123"
asciiUpper :: ByteString -> ByteString
asciiUpper = BS.map shiftToUpper
  where
    shiftToUpper w =
      if w >= 97 && w <= 122
      then w - 32
      else w
