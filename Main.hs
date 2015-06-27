import Control.Monad (unless)
import System.Info (os)
import System.Process (system, rawSystem)
import System.Exit    (ExitCode(..))
import System.Directory (doesFileExist)
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Exception

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

instance FromJSON URL where
  parseJSON (Object v) = URL <$> v .: "url"

instance FromJSON Thumbnail where
  parseJSON (Object v) = Thumbnail <$> v .: "default"

instance FromJSON VideoID where
  parseJSON (Object v) = VideoID <$>
                          v .: "videoId"

instance FromJSON Snippet where
  parseJSON (Object v) = Snippet <$>
                         v .: "title" <*>
                         v .: "description" <*>
                         v .: "thumbnails"

instance FromJSON JSearchResult where
  parseJSON (Object v) = JSearchResult <$>
                         (v .: "id") <*>
                         (v .: "snippet")

instance FromJSON JItems where
  parseJSON (Object v) = JItems <$> v .: "items"

makeURL :: T.Text -> T.Text
makeURL vid = "https://youtube.com/watch?v=" `T.append` vid

getItems :: BL.ByteString -> [SearchResult]
getItems str =
  let parsed = decode str :: Maybe JItems
    in maybe [] getResults parsed
    where
      getResults (JItems xs) = map getResult xs
      getResult x = SearchResult (jtitle . snippet $ x)
                                 (jdescription . snippet $ x)
                                 (makeURL . videoID . jvideoID $ x)
                                 (jurl . thumbnail . jthumbnails . snippet $ x)

cid = "571126085022-7ash7a48cdao0tesqe66ghtime34cfvo.apps.googleusercontent.com"
secret = "FjgeOtSZoJgU87FbuwJf2vwj"
file   = "./tokens.txt"
baseURI = "https://www.googleapis.com/youtube/v3/"

searchRequest :: String -> String -> String
searchRequest keyword accessTok =
  baseURI ++
  "search?part=snippet&q=" ++
  keyword ++
  "&type=video&access_token=" ++
  accessTok



main = do
  let client = OAuth2Client { clientId = cid, clientSecret = secret }
      permissionUrl = formUrl client ["https://www.googleapis.com/auth/youtube"]
  b <- doesFileExist file
  unless b $ do
      putStrLn $ "Load this URL: " ++ show permissionUrl
      putStrLn "Please paste the verification code: "
      authcode <- getLine
      tokens <- exchangeCode client authcode
      putStrLn $ "Received access token: " ++ show (accessToken tokens)
      writeFile file (show tokens)
  accessTok <- fmap (accessToken . read) (readFile file)
  tracks <- findTracks client accessTok "Foo+Fighters"
  print tracks

getNewTokens :: OAuth2Client -> IO ()
getNewTokens client = do
  tokens <- read <$> readFile file
  newTokens <- refreshTokens client tokens
  writeFile file (show newTokens)

findTracks client accessTok term = do
  response <- (try $ simpleHttp $ searchRequest term accessTok) :: IO (Either HttpException BL.ByteString)
  case response of
    (Left _) -> getNewTokens client >> findTracks client accessTok term
    (Right resp) -> return $ getItems resp

search :: String -> [T.Text]
search term = undefined
