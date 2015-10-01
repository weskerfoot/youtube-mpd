module Main where

import Search
import Network.MPD.Applicative.CurrentPlaylist
import Types
import Database
import Data.List
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO (putStrLn, putStr, writeFile)
import qualified Control.Monad as M
import M3U

main = do
  args <- getArgs
  let mode = head args
  let searchTerm = intercalate " " $ tail args
  case mode of
    --"single" -> M.join (addSingle <$> firstResult searchTerm) >> return ()
    "all" -> do
              m3ulist <- (M.join (genm3u <$> search searchTerm))
              let (M3U filecontents) = m3ulist
              TIO.putStr filecontents
    _ -> error "unknown mode"
