{-# LANGUAGE OverloadedStrings #-}
module M3U where

import Types
import qualified Data.Text as TIO
import Data.Text.Encoding
import Data.Maybe
import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import Utils
import Playlists
import Search

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

singleTrack track = do
  let extinf = toExtinf track
  trackUrl <- TIO.pack <$> (getUrl $ url track)
  return $ extinf `mappend` trackUrl `mappend` "\n"

genm3u :: [SearchResult] -> IO M3U
genm3u srs = do
  tracks <- mapM singleTrack srs
  return $
    M3U $ m3uHeader `mappend`
    "\n" `mappend`
    (mconcat tracks)
