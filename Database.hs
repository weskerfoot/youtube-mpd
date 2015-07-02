{-# LANGUAGE OverloadedStrings #-}
module Database where

import qualified Network.MPD as MP
import Types
import Search
import qualified Unsafe.Coerce as C
import qualified Data.Text as TIO
import Data.Text.Encoding
import System.Process
import qualified Data.ByteString.Char8 as B
import Data.String.Utils

toPath :: String -> MP.Path
{-
 - unsafeCoerce is needed because the "Path"
 - type is a newtype that represents a ByteString type.
 - In the MPD library the Types module is a hidden
 - module, so we can't use the constructor for it.
 - Therefore we just coerce to that type, which
 - is the exact same type anyway
 -}
toPath = C.unsafeCoerce . B.pack . strip . head . split "\n"

addSingle url = do
  fullUrl <- readProcess "youtube-dl" ["-g", "-f", "bestaudio", (TIO.unpack url)] ""
  MP.withMPD $ MP.add $ toPath fullUrl
