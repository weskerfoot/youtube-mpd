module Main where

import Search
import Types
import qualified Data.Text.IO as TIO (putStrLn)

main = do
  term <- getContents
  results <- search term
  TIO.putStrLn $ url $ head $ results
