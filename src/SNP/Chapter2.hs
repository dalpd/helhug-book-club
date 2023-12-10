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
checks :: IO ()
checks = do
  putStrLn "Exercise 4:"
  print $ digitsOnly $ Text.pack "ab c123 def4"
  putStrLn "Exercise 5:"
  print $ capitalizeLast $ Text.pack "dog"
