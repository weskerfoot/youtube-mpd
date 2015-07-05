module Main where

import Search
import Network.MPD.Applicative.CurrentPlaylist
import Types
import Database
import Data.List
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO (putStrLn)

main = do
  term <- getArgs
  result <- search $ intercalate " " term
  mapM_ addSingle result
