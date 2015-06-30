module Search where

import Control.Monad (unless)
import System.Info (os)
import System.Process (system, rawSystem)
import System.Exit    (ExitCode(..))
import System.Directory (doesFileExist)
import Network.Google.OAuth2 (formUrl, exchangeCode, refreshTokens,
                               OAuth2Client(..), OAuth2Tokens(..))
import Network.Google (makeRequest, doRequest)
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Control.Exception
import Network.HTTP.Base (urlEncode, urlDecode)
import Types

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
file   = "/home/wes/.config/youtube-tokens.txt"
baseURI = "https://www.googleapis.com/youtube/v3/"

searchRequest :: String -> String -> String
searchRequest keyword accessTok =
  baseURI ++
  "search?part=snippet&q=" ++
  keyword ++
  "&type=video&access_token=" ++
  accessTok

getNewTokens :: OAuth2Client -> IO OAuth2Tokens
getNewTokens client = do
  tokens <- read <$> readFile file
  newTokens <- refreshTokens client tokens
  writeFile file (show newTokens)
  return newTokens

findTracks :: OAuth2Client -> String -> String -> IO [SearchResult]
findTracks client accessTok term = do
  response <- (try $ simpleHttp $ searchRequest term accessTok) :: IO (Either HttpException BL.ByteString)
  case response of
    (Left _) -> do
      tokens <- getNewTokens client
      findTracks client (accessToken tokens) term
    (Right resp) -> return $ getItems resp

search :: String -> IO [SearchResult]
search term = do
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
  findTracks client accessTok (urlEncode term)
