module Types where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.String as S

data URL = URL { jurl :: T.Text }
  deriving (Show, Eq)

data VideoID = VideoID { videoID :: T.Text }
  deriving (Show, Eq)

data Thumbnail = Thumbnail { thumbnail :: URL }
  deriving (Show, Eq)

data JSearchResult = JSearchResult {
  jvideoID :: VideoID,
  snippet :: Snippet
}
  deriving (Show, Eq)

data Snippet = Snippet {
  jtitle :: T.Text,
  jdescription :: T.Text,
  jthumbnails :: Thumbnail
}
  deriving (Show, Eq)

data JItems = JItems [JSearchResult]
  deriving (Show)

data SearchResult = SearchResult {
  title :: T.Text,
  description :: T.Text,
  url :: T.Text,
  thumb :: T.Text
}
  deriving (Show, Eq)

data M3U = M3U T.Text
  deriving (Show)

instance S.IsString M3U where
  fromString = M3U . T.pack

instance FromJSON URL where
  parseJSON (Object v) = URL <$> v .: "url"
  parseJSON _ = error "invalid parse"

instance FromJSON Thumbnail where
  parseJSON (Object v) = Thumbnail <$> v .: "default"
  parseJSON _ = error "invalid parse"

instance FromJSON VideoID where
  parseJSON (Object v) = VideoID <$>
                          v .: "videoId"
  parseJSON _ = error "invalid parse"

instance FromJSON Snippet where
  parseJSON (Object v) = Snippet <$>
                         v .: "title" <*>
                         v .: "description" <*>
                         v .: "thumbnails"
  parseJSON _ = error "invalid parse"

instance FromJSON JSearchResult where
  parseJSON (Object v) = JSearchResult <$>
                         (v .: "id") <*>
                         (v .: "snippet")
  parseJSON _ = error "invalid parse"

instance FromJSON JItems where
  parseJSON (Object v) = JItems <$> v .: "items"
  parseJSON _ = error "invalid parse"
