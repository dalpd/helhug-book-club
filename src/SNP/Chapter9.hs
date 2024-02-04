{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE InstanceSigs       #-}

module SNP.Chapter9 where

import SNP.Chapter6
import SNP.Chapter8
import qualified ASCII as A
import GHC.Natural
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Network.Simple.TCP as Net
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as HTML
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as J.Key
import qualified Data.Aeson.KeyMap as J.KeyMap
import Data.Aeson (ToJSON (toJSON))
import qualified Data.ByteString.Builder as BSB
import Data.Int
import qualified  Data.Text.Encoding as T
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

plainUtf8 :: FieldValue
plainUtf8 = FieldValue [A.string|text/plain; charset=utf-8|]

htmlUtf8 :: FieldValue
htmlUtf8 = FieldValue [A.string|text/html; charset=utf-8|]

json :: FieldValue
json = FieldValue [A.string|application/json|]

countHelloText :: Natural -> LT.Text
countHelloText count =
  let countMsg =
        case count of
          0 -> TB.fromString "This page has never been viewed."
          1 -> TB.fromString "This page has been viewed 1 time."
          _ -> TB.fromString "This page has been viewed " <> TB.decimal count <> TB.fromString " times."
  in TB.toLazyText $ TB.fromString "Hello! \9835\r\n" <> countMsg

textOk :: LT.Text -> Response
textOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType plainUtf8
    len = Field contentLength (bodyLengthValue body)
    body = Body (LT.encodeUtf8 str)

stuckCountingServerText :: IO ()
stuckCountingServerText = Net.serve @IO Net.HostAny "8000" \(s, _) -> do
  let count = 0 -- to-do!
  sendResponse s (textOk (countHelloText count))

countHelloHtml :: Natural -> Html
countHelloHtml count = HTML.docType <> htmlDocument
  where
    htmlDocument = HTML.html $ documentMetadata <> documentBody
    documentMetadata = HTML.head titleHtml
    titleHtml = HTML.title (toHtml "My great web page")
    documentBody = HTML.body $ greetingHtml <> HTML.hr <> hitCounterHtml
    greetingHtml = HTML.p (toHtml "Hello! \9835")
    hitCounterHtml = HTML.p $ case count of
      0 -> toHtml "This page has never been viewed."
      1 -> toHtml "This page has been viewed 1 time."
      _ -> toHtml "This page has been viewed " <> toHtml @Natural count <> toHtml " times."

countHelloJson :: Natural -> J.Value
countHelloJson count = toJSON (J.KeyMap.fromList [greetingJson, hitsJson])
  where
    greetingJson = (J.Key.fromString "greeting", toJSON "Hello! \9835")
    hitsJson = (J.Key.fromString "hits",
                toJSON (J.KeyMap.fromList [numberJson, messageJson]))
    numberJson = (J.Key.fromString "count", toJSON count)
    messageJson = (J.Key.fromString "message", toJSON (countHelloText count))

countHelloJson' :: Natural -> J.Value
countHelloJson' count =
  J.object [
    J.Key.fromString "greeting" J..= J.Key.fromString "Hello! \9835",
    J.Key.fromString "hits" J..= J.object [
    J.Key.fromString "count" J..= count,
    J.Key.fromString "message" J..= countHelloText count ]
    ]

jsonOk :: J.Value -> Response
jsonOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType json
    len = Field contentLength (bodyLengthValue body)
    body = Body (J.encode str)

htmlOk :: Html -> Response
htmlOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType htmlUtf8
    len = Field contentLength $ bodyLengthValue body
    body = Body $ renderHtml str

stuckCountingServerHtml :: IO ()
stuckCountingServerHtml = Net.serve @IO Net.HostAny "8000" \(s, _) -> do
  let count = 0 -- to-do!
  sendResponse s (htmlOk (countHelloHtml count))

class Encode a where
  encode :: a -> BSB.Builder

-- Write Encode instances for the following types: Request, Response, Integer, Int64, Text,
-- LText, ByteString, LByteString, and BSB.Builder.
-- Have to have some of these types as orphans so might as well define all here.
instance Encode Request where
  encode :: Request -> BSB.Builder
  encode = encodeRequest

instance Encode Response where
  encode :: Response -> BSB.Builder
  encode = encodeResponse

instance Encode Integer where
  encode :: Integer -> BSB.Builder
  encode = BSB.integerDec

instance Encode Int64 where
  encode :: Int64 -> BSB.Builder
  encode = BSB.int64Dec

instance Encode Text where encode x = BSB.byteString (T.encodeUtf8 x)
instance Encode LT.Text where encode x = BSB.lazyByteString (LT.encodeUtf8 x)
instance Encode ByteString where encode = BSB.byteString
instance Encode LBS.ByteString where encode = BSB.lazyByteString
instance Encode BSB.Builder where encode x = x
