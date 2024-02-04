{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

module SNP.Chapter6 where

import ASCII
import ASCII.Char
import qualified ASCII.Decimal as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB

-- Common
--newtype ASCII string = ASCII string
newtype Body = Body LBS.ByteString
data Version = Version Digit Digit

newtype FieldName = FieldName (ASCII ByteString)
newtype FieldValue = FieldValue (ASCII ByteString)
data Field = Field FieldName FieldValue

-- Request
newtype Method = Method (ASCII ByteString)
newtype RequestTarget = RequestTarget (ASCII ByteString)
data RequestLine = RequestLine Method RequestTarget Version
data Request = Request RequestLine [Field] (Maybe Body)

-- Response
data StatusCode = StatusCode Digit Digit Digit
newtype ReasonPhrase = ReasonPhrase (ASCII ByteString)
data StatusLine = StatusLine Version StatusCode (Maybe ReasonPhrase)
data Response = Response StatusLine [Field] (Maybe Body)

-- GET /hello.txt HTTP/1.1
-- Host: www.example.com
-- Accept-Language: en, mi

-- HTTP/1.1 200 OK
-- Content-Type: text/plain; charset=us-ascii
-- Content-Length: 6
--
-- Hello!

helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
  where
    start =
      RequestLine
        (Method $ [string|GET|])
        (RequestTarget $ [string|hello.txt|])
        (Version A.Digit1 A.Digit1)
    host =
      Field
        (FieldName $ [string|Host|])
        (FieldValue $ [string|www.example.com|])
    lang =
      Field
        (FieldName $ [string|Accept-Language|])
        (FieldValue $ [string|en, mi|])

helloResponse :: Response
helloResponse =
  Response
    (StatusLine version code (Just phrase))
    [Field
     (FieldName $ [string|Content-Type|])
     (FieldValue $ [string|text/plain; charset=us-ascii|])
    ]
     (Just $ Body "Hello!")
  where
    version = Version A.Digit1 A.Digit1
    code = StatusCode A.Digit2 A.Digit0 A.Digit0
    phrase = ReasonPhrase $ [string|OK|]

santaSample :: LBS.ByteString
santaSample = LBS.cycle "ho"

santaTrademark :: LBS.ByteString
santaTrademark = LBS.take 6 $ santaSample

encodeField :: Field -> BSB.Builder
encodeField (Field (FieldName x) (FieldValue y)) =
  BSB.byteString (lift x) <> fromCharList [Colon, Space] <>
  BSB.byteString (lift y)

encodeBody :: Body -> BSB.Builder
encodeBody (Body x) = BSB.lazyByteString x
