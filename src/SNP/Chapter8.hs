{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE TypeApplications   #-}

module SNP.Chapter8 where

import SNP.Chapter6
import qualified ASCII as A
import qualified ASCII.Char as A (Char (CarriageReturn), Char (Space), Char (LineFeed))
import qualified ASCII.Decimal as A
import GHC.Natural (Natural)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import Network.Socket (Socket)
import qualified Network.Simple.TCP as Net
import Data.Foldable
import Data.Word

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

countHelloAscii :: Natural -> A.ASCII LBS.ByteString
countHelloAscii count =
  [A.string|Hello!|]
    <> A.fromCharList crlf
    <> case count of
      0 -> [A.string|This page has never been viewed.|]
      1 -> [A.string|This page has been viewed 1 time.|]
      _ -> [A.string|This page has been viewed |] <> A.showIntegralDecimal count <> [A.string| times.|]

data Status = Status StatusCode (Maybe ReasonPhrase)

status :: Status -> StatusLine
status (Status code phrase) = StatusLine http_1_1 code phrase

ok :: Status
ok = Status (StatusCode A.Digit2 A.Digit0 A.Digit0) (Just (ReasonPhrase $ [A.string|OK|]))

http_1_1 :: Version
http_1_1 = Version A.Digit1 A.Digit1

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode enc (Just x) = enc x
optionallyEncode _ Nothing = mempty

encodeLineEnd :: BSB.Builder
encodeLineEnd = A.fromCharList crlf

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase x) = BSB.byteString (A.lift x)

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) = A.fromDigitList [x, y, z]

encodeVersion :: Version -> BSB.Builder
encodeVersion (Version x y) = [A.string|HTTP/|] <> A.fromDigitList [x] <> [A.string|.|] <> A.fromDigitList [y]

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine version code reason)
  = encodeVersion version
  <> A.fromCharList [A.Space]
  <> encodeStatusCode code
  <> A.fromCharList [A.Space]
  <> optionallyEncode encodeReasonPhrase reason
  <> encodeLineEnd

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
repeatedlyEncode enc xs = fold (map enc xs)

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine fields bodyMaybe)
  = encodeStatusLine statusLine
  <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
  <> encodeLineEnd <> optionallyEncode encodeBody bodyMaybe

encodeRequest :: Request -> BSB.Builder
encodeRequest (Request requestLine fields bodyMaybe)
  = encodeRequestLine requestLine
  <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
  <> encodeLineEnd
  <> optionallyEncode encodeBody bodyMaybe

encodeMethod :: Method -> BSB.Builder
encodeMethod (Method x) = BSB.byteString (A.lift x)

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget (RequestTarget x) = BSB.byteString (A.lift x)

encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method target version)
  = encodeMethod method <> A.fromCharList [A.Space]
  <> encodeRequestTarget target <> A.fromCharList [A.Space]
  <> encodeVersion version <> encodeLineEnd

contentType :: FieldName
contentType = FieldName [A.string|Content-Type|]

plainAscii :: FieldValue
plainAscii = FieldValue [A.string|text/plain; charset=us-ascii|]

contentLength :: FieldName
contentLength = FieldName [A.string|Content-Length|]

bodyLengthValue :: Body -> FieldValue
bodyLengthValue (Body x) = FieldValue (A.showIntegralDecimal (LBS.length x))

asciiOk :: A.ASCII LBS.LazyByteString -> Response
asciiOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType plainAscii
    len = Field contentLength (bodyLengthValue body)
    body = Body (A.lift str)

sendResponse :: Socket -> Response -> IO ()
sendResponse s r = Net.sendLazy s $ BSB.toLazyByteString (encodeResponse r)

stuckCountingServer :: IO ()
stuckCountingServer = Net.serve @IO Net.HostAny "8000" \(s, _) -> do
  let count = 0 -- to-do!
  sendResponse s (asciiOk (countHelloAscii count))

mid :: Word8 -> Word8 -> Word8
mid x y = div (x + y) 2

mid' :: Word8 -> Word8 -> Word8
mid' x y = fromInteger $ div (toInteger x + toInteger y) 2
