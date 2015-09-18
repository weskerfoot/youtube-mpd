module Playlists where

import qualified Data.Text as TIO
import System.Process (readProcess)
import Utils

getUrl :: TIO.Text -> IO String
-- Gets a direct url using youtube-dl
-- (if it is installed, otherwise we might fallback to some shitty code)

getUrl yourl = readProcess "youtube-dl"
                ["-g",
                "-f",
                "bestaudio",
                TIO.unpack yourl,
                "--no-cache-dir"] ""
downUrl yourl = readProcess "youtube-dl"
                  ["-f",
                  "bestaudio",
                  TIO.unpack yourl,
                  "--no-cache-dir"] ""
