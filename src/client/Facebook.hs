module FacebookClient where 

import Network.HTTP.Wget
import Control.Monad.Reader
import Data.Digest.Pure.MD5
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as BS

version = 1.0 

data FacebookConfig = FacebookConfig {
      apiKey :: String,
      secretKey :: String,
      endPoint :: String
}

type FacebookM = ReaderT FacebookConfig IO  

type Params = [(String, String)]

status_set :: String -> FacebookM (Maybe String)
status_set status = 
    callMethod "facebook.status.set" [("status", status)]

addStandardParams :: String -> Params -> FacebookM Params 
addStandardParams method params = do
  config <- ask
  time <- liftIO getPOSIXTime
  return [("method", method),
          ("v", show version),
          ("api_key", apiKey config),
          ("call_id", init $ show time)]
  

postRequest :: String -> Params -> FacebookM (Maybe String)
postRequest endPoint params = 
    liftIO $ wget endPoint [] params

callMethod :: String -> Params -> FacebookM (Maybe String)
callMethod method params = do
  config <- ask
  params' <- addStandardParams method params
  let signature = sign params $ secretKey config 
  let finalParams = ("sig", signature) : params'
  postRequest (endPoint config) finalParams
  
sign ::  Params -> String -> String
sign params secret =
    show $ md5 $ BS.pack $
    (concat $ map (\(a,b) -> a ++ "=" ++ b) params) ++ secret  



