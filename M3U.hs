{-# LANGUAGE OverloadedStrings #-}
module M3U where

import Types
import qualified Data.Text as TIO
import qualified Control.Monad as M (join)
import Data.Text.Encoding
import Data.Maybe
import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import Utils
import Playlists
import Search
import Control.Concurrent.Async

m3uHeader = "#EXTM3U"
trackHeader = "#EXTINF"

makeHeader title =
  mconcat [trackHeader,
          ":-1,",
          title]

--toExtinf :: SearchResult -> TIO.Text
toExtinf track =
  let trackTitle = title track
      in case parseTrack trackTitle of
        Right (artist, title) ->
          makeHeader (artist `mappend` " - " `mappend` title `mappend` "\n")
        Left trackname -> makeHeader $ TIO.pack (trackname `mappend` "\n")

singleTrack :: SearchResult -> IO (Maybe TIO.Text)
singleTrack track = do
  let extinf = toExtinf track
  maybeUrl <- getUrl $ url track
  case maybeUrl of
    (Just trackUrl) -> (return $ (Just extinf) `mappend` (Just $ TIO.pack trackUrl) `mappend` (Just "\n"))
    Nothing -> return Nothing

genm3u :: [SearchResult] -> IO M3U
genm3u srs = do
  tracks <- (mapConcurrently singleTrack srs)
  return $
    M3U $ m3uHeader `mappend`
    "\n" `mappend`
    (mconcat $ catMaybes tracks)


searchM3U term = do
  (M3U m3u) <- M.join (genm3u <$> search term)
  return m3u
