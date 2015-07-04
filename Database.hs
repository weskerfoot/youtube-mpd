{-# LANGUAGE OverloadedStrings #-}
module Database where

import qualified Network.MPD as MP
import Types
import Search
import qualified Unsafe.Coerce as C
import qualified Data.Text as TIO
import Data.Text.Encoding
import System.Process
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.String.Utils
import Data.Maybe
import Control.Applicative

{-
 - unsafeCoerce is needed because the "Path"
 - type is a newtype that represents a ByteString type.
 - In the MPD library the Types module is a hidden
 - module, so we can't use the constructor for it.
 - Therefore we just coerce to that type, which
 - is the exact same type anyway
 -}
toPath :: String -> MP.Path
toPath = C.unsafeCoerce . BC.pack . strip . head . split "\n"

toValue :: B.ByteString -> MP.Value
toValue = C.unsafeCoerce

changeTitle :: TIO.Text -> Maybe MP.Song -> IO (MP.Response ())
changeTitle _ Nothing = error "empty playlist"

changeTitle title' (Just song) = do
  let title = encodeUtf8 title'
  case (MP.sgId song) of
    Nothing -> error "tried to modify a non-existent track"
    (Just sid) -> do
      MP.withMPD $
        MP.addTagId sid MP.Title (toValue title)

addSingle :: SearchResult -> IO (MP.Response ())
addSingle track = do
  let trackUrl = url track
  let trackTitle = title track
  fullUrl <- readProcess "youtube-dl" ["-g", "-f", "bestaudio", (TIO.unpack trackUrl)] ""
  MP.withMPD $ MP.add $ toPath fullUrl
  (Right pl) <- (MP.withMPD $ MP.playlistInfo Nothing)
  changeTitle trackTitle (listToMaybe $ reverse pl)
