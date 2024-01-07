{-# LANGUAGE NumericUnderscores #-}
module SNP.Chapter5 where

import SNP.Chapter2 (repeatUntil)
import Data.Maybe (isJust)

import qualified Network.Simple.TCP as Net
import SNP.Chapter4 (openThenConnect, resolve)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) func = func x
whenJust Nothing _  = pure ()

-- | Exercise 15
repeatUntilNothing :: Monad m => m (Maybe chunk) -> (chunk -> m ()) -> m ()
repeatUntilNothing getChunk action = proceed
  where
    proceed = do
      chunk <- getChunk
      whenJust chunk action
      proceed

repeatUntil' ::
  (Monad m) =>
  m chunk ->
  (chunk -> Bool) ->
  (chunk -> m ()) ->
  m ()
repeatUntil' getChunk isEnd = repeatUntilNothing $ do
  x <- getChunk
  return (if isEnd x then Nothing else Just x)

repeatUntilNothing'' :: Monad m => m (Maybe chunk) -> (Maybe chunk -> m ()) -> m ()
repeatUntilNothing'' getChunk =
  repeatUntil getChunk isJust

line :: ByteString -> ByteString
line x = x <> fromString "\r\n"

haskellOrgCall :: IO ()
haskellOrgCall = runResourceT $ do
  addressInfo <- liftIO (resolve "http" "haskell.org")
  (_, s) <- openThenConnect addressInfo
  liftIO $ Net.send s $
    line (fromString "GET / HTTP /1.1")
      <> line (fromString "Host: haskell.org")
      <> line (fromString "Connection: close")
      <> line (fromString "")
  liftIO $ repeatUntilNothing (Net.recv s 1_024) BS.putStr
