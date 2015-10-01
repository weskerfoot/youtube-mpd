module Playlists where

import qualified Data.Text as TIO
import System.Process (readProcess)
import Control.Concurrent.Async
import Control.Exception
import Utils
import Control.Monad

getUrls = mapConcurrently getUrl

getUrl :: TIO.Text -> IO (Maybe String)
-- Gets a direct url using youtube-dl
-- (if it is installed, otherwise we might fallback to some shitty code)

getUrl yourl =
  let url = catch (Just <$> (readProcess "youtube-dl"
              ["-g", "-f", "bestaudio", TIO.unpack yourl, "--no-cache-dir"] "")) ((\e -> return Nothing) :: SomeException -> IO (Maybe String))
    in url

downUrl yourl = readProcess "youtube-dl"
                  ["-f",
                  "bestaudio",
                  TIO.unpack yourl,
                  "--no-cache-dir"] ""
