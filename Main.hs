module Main where

import Search
import Network.MPD.Applicative.CurrentPlaylist
import Types
import Database
import Data.List
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO (putStrLn)
import qualified Control.Monad as M

main = do
  args <- getArgs
  let mode = head args
  let searchTerm = intercalate " " $ tail args
  case mode of
    "single" -> M.join (addSingle <$> firstResult searchTerm) >> return ()
    "all" -> M.join (mapM_ addSingle <$> search searchTerm) >> return ()
    _ -> error "unknown mode"
