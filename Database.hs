{-# LANGUAGE OverloadedStrings #-}
module Database where

import qualified Network.MPD as MP
import Network.MPD.Applicative.Status
import Network.MPD.Core
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
import Utils

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

fromId :: MP.Id -> Int
fromId = C.unsafeCoerce

changeTag :: MP.Metadata -> TIO.Text -> Maybe MP.Song -> IO (MP.Response [B.ByteString])
changeTag _ _ Nothing = error "empty playlist"

changeTag tag tagval' (Just song) = do
  let tagval = encodeUtf8 tagval'
  case (MP.sgId song) of
    Nothing -> error "tried to modify a non-existent track"
    (Just sid) -> addTagId sid tag (BC.unpack tagval)

changeArtist = changeTag MP.Artist
changeTitle = changeTag MP.Title

changeBoth track (artist, title) =
  changeArtist artist track >>
  changeTitle title track

addTagId :: MP.Id -> MP.Metadata -> String -> IO (Response [B.ByteString])
addTagId sid tag tagVal = MP.withMPD $
                              getResponse $
                                "addtagid " ++
                                (show $ fromId sid) ++
                                " " ++
                                (show tag) ++
                                " \"" ++
                                tagVal ++
                                " \""

addSingle :: SearchResult -> IO (Response [B.ByteString])
addSingle track = do
  let trackUrl = url track
  let trackDesc = title track
  fullUrl <- readProcess "youtube-dl" ["-g", "-f", "bestaudio", (TIO.unpack trackUrl)] ""
  MP.withMPD $ MP.add $ toPath fullUrl
  (Right pl) <- (MP.withMPD $ MP.playlistInfo Nothing)
  let lastTrack = listToMaybe $ reverse pl
  either (const $ changeTitle trackDesc lastTrack)
         (changeBoth lastTrack)
         (parseTrack trackDesc)
