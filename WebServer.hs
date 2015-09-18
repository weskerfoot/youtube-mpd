module WebServer where

import Search
import Network.MPD.Applicative.CurrentPlaylist
import Types
import Database
import Data.List
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO (putStrLn, putStr)
import qualified Control.Monad as M
import M3U

getPlaylist args = do
  let mode = head args
  let searchTerm = intercalate " " $ tail args
  case mode of
    --"single" -> M.join (addSingle <$> firstResult searchTerm) >> return ()
    "all" -> do
              m3ulist <- (M.join (genm3u <$> search searchTerm))
              let (M3U filecontents) = m3ulist
              return filecontents
    _ -> error "unknown mode"
