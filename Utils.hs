{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Types
import qualified Data.Text as TIO
import Data.Text.Encoding
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT

repeated xs = TIO.concat <$> (many1 $ string xs)

isSep = choice [string "by",
                repeated "-",
                repeated "|"]


isTrack = do
  artist <- manyTill anyChar isSep
  title <- many1 anyChar
  return (TIO.strip $ TIO.pack artist, TIO.strip $ TIO.pack title)

parseTrack = parseOnly isTrack
