{-# LANGUAGE LambdaCase #-}

module SNP.Chapter1 where

------------------------------------------------------------------------------

import Control.Exception (Exception (displayException))
import Control.Exception.Safe (tryAny)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
import Data.Time (getCurrentTime)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..))
import qualified System.IO as IO

------------------------------------------------------------------------------
fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose

------------------------------------------------------------------------------
myWriteGreetingSafe :: IO ()
myWriteGreetingSafe = runResourceT $ do
  dir <- liftIO getCurrentDirectory
  (_releaseKey, handle) <-
    fileResource (dir </> "greeting.txt") WriteMode
  liftIO (IO.hPutStrLn handle "hello")
  liftIO (IO.hPutStrLn handle "world")

------------------------------------------------------------------------------
handlePrintTest :: IO ()
handlePrintTest = runResourceT $ do
  dir <- liftIO getCurrentDirectory
  (_releaseKey, handle) <-
    fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ print handle
  liftIO $ putStrLn =<< IO.hShow handle

------------------------------------------------------------------------------
howManyHandles :: IO ()
howManyHandles = runResourceT $ do
  hs <- openManyHandles
  liftIO $ putStrLn $ "Opened " <> show (length hs) <> " handles"

------------------------------------------------------------------------------
openManyHandles :: ResourceT IO [Handle]
openManyHandles = go mempty
  where
    go handles =
      fileResourceMaybe >>= \case
        Nothing -> pure handles
        Just handle -> go (handle : handles)

------------------------------------------------------------------------------
fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
  timestamp <- liftIO getCurrentTime
  result <-
    tryAny $ do
      (_releaseKey, handler) <-
        fileResource ("/tmp/" </> show timestamp) WriteMode
      pure $ Just handler
  case result of
    Right handle -> pure handle
    Left e -> do
      liftIO $ print $ displayException e
      pure Nothing

------------------------------------------------------------------------------
checks :: IO ()
checks = do
  putStrLn "Exercise 2:"
  myWriteGreetingSafe
  handlePrintTest
  putStrLn "Exercise 3:"
  howManyHandles
