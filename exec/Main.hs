module Main
  ( main
  )
where

------------------------------------------------------------------------------

import qualified SNP.Chapter1 as Chapter1
import qualified SNP.Chapter2 as Chapter2

------------------------------------------------------------------------------
main :: IO()
main = do
  Chapter1.checks
  Chapter2.checks
