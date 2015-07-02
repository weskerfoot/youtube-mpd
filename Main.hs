module Main where

import Search
import Types
import Database
import Data.List
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO (putStrLn)

main = do
  term <- getArgs
  result <- firstResult $ intercalate " " term
  addSingle result
