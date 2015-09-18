{-# LANGUAGE OverloadedStrings #-}
module Database where

import qualified Network.MPD as MP
import Network.MPD.Applicative.Status
import Network.MPD.Core
import Types
import Search
import qualified Unsafe.Coerce as C
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.String.Utils
import Data.Maybe
import Control.Applicative
import Utils
import Playlists

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

--changeTag :: MP.Metadata -> TIO.Text -> Either MP.Id -> IO (MP.Response [B.ByteString])

changeTag tag tagval' (Right sid) = do
  let tagval = encodeUtf8 tagval'
    in addTagId sid tag (BC.unpack tagval)
changeTag _ _ (Left _) = error "error changing tag"

changeArtist = changeTag MP.Artist
changeTitle = changeTag MP.Title

changeBoth trackid (artist, title) =
  changeArtist artist trackid >>
  changeTitle title trackid

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
  fullUrl <- getUrl trackUrl
  newId <- MP.withMPD $ MP.addId (toPath fullUrl) Nothing
  --downUrl trackUrl
  either (const $ changeTitle trackDesc newId)
         (changeBoth newId)
         (parseTrack trackDesc)
