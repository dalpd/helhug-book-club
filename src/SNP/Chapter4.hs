{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}


module SNP.Chapter4 where
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import Network.Socket (Socket)
import qualified Network.Socket as S
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as S
import SNP.Chapter2 ( repeatUntil )

-- | Exercise 12
openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect addressInfo = allocate setup S.close
  where
    setup = do
      s <- S.openSocket addressInfo
      S.setSocketOption s S.UserTimeout 1_000
      S.connect s (S.addrAddress addressInfo)
      return s

openThenConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openThenConnect addressInfo = do
  returnPair@(_releaseKey, socket) <- allocate safeSetup S.close
  liftIO $ S.connect socket (S.addrAddress addressInfo)
  pure returnPair
  where
    safeSetup = do
      s <- S.openSocket addressInfo
      S.setSocketOption s S.UserTimeout 1_000
      pure s

-- | Exercise 13
findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite =
  let host = "quux.org"
      protocol = "gopher"
  in S.getAddrInfo
    (Just S.defaultHints { S.addrSocketType = S.Stream })
    (Just host)
    (Just protocol) >>= \case
      [] -> fail "getAddrInfo returned an empty list"
      x : _ -> pure x

-- | >>> findHaskellWebsite >>= communicateThroughGopher
communicateThroughGopher :: S.AddrInfo -> IO ()
communicateThroughGopher addressInfo = runResourceT $ do
  (_, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO $ do
      S.setSocketOption s S.UserTimeout 1_000
      S.connect s (S.addrAddress addressInfo)
      S.sendAll s $ T.encodeUtf8 $ T.pack "\r\n"
      repeatUntil (S.recv s 1_024) BS.null BS.putStr
      S.gracefulClose s 1_000

-- | Exercise 14
-- Resolver returning the first match
resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve service host =  S.getAddrInfo
    (Just S.defaultHints { S.addrSocketType = S.Stream })
    (Just host)
    (Just service) >>= \case
      [] -> fail "getAddrInfo returned an empty list"
      x : _ -> pure x
