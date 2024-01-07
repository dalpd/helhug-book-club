{-# LANGUAGE OverloadedStrings #-}

module SNP.Chapter2 where

------------------------------------------------------------------------------

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Char (isDigit, toUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import SNP.Chapter1
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (..))

------------------------------------------------------------------------------
digitsOnly :: Text -> Text
digitsOnly = Text.filter isDigit

------------------------------------------------------------------------------
capitalizeLast :: Text -> Text
capitalizeLast = go
  where
    go txt =
      case Text.unsnoc txt of
        Nothing -> Text.empty
        Just (front, back) ->
          Text.snoc front $ toUpper back

------------------------------------------------------------------------------
unParen :: Text -> Maybe Text
unParen t = case Text.stripPrefix (Text.pack "(") t of
  Just t' -> Text.stripSuffix (Text.pack ")") t'
  Nothing -> Nothing

------------------------------------------------------------------------------
characterCount :: FilePath -> IO Int
characterCount fp = runResourceT $ do
  dir <- liftIO getCurrentDirectory
  (_, h) <- fileResource (dir </> fp) ReadMode
  liftIO (hCharacterCount h)

------------------------------------------------------------------------------
hCharacterCount :: Handle -> IO Int
hCharacterCount h = proceed 0
  where
    proceed n = do
      x <- Text.hGetChunk h
      case Text.null x of
        True -> return n
        False -> proceed (n + Text.length x)

------------------------------------------------------------------------------
repeatUntil ::
  (Monad m) =>
  m chunk ->
  (chunk -> Bool) ->
  (chunk -> m ()) ->
  m ()
repeatUntil getChunk isEnd f = proceed
  where
    proceed = do
      chunk <- getChunk
      unless (isEnd chunk) $ f chunk
      proceed

------------------------------------------------------------------------------
checks :: IO ()
checks = do
  putStrLn "Exercise 4:"
  print $ digitsOnly $ Text.pack "ab c123 def4"
  putStrLn "Exercise 5:"
  print $ capitalizeLast $ Text.pack "dog"
  putStrLn "Exercise 6:"
  print $ unParen $ Text.pack "(cat)"
  print $ unParen $ Text.pack "cat"
  putStrLn "Exercise 7:"
  print =<< characterCount "greeting.txt"
