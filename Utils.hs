{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Types
import qualified Data.Text as TIO
import Data.Text.Encoding
import Data.Maybe
import Control.Applicative
import Data.Attoparsec.Text

isSep = string "by" <|>
        string " - " <|>
        string "- "

isTrack = do
  artist <- manyTill anyChar isSep
  title <- many1 anyChar
  return (TIO.pack artist, TIO.pack title)

parseTrack = parseOnly isTrack
