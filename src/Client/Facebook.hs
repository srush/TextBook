{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

module Client.Facebook where 

import Network.HTTP.Wget
import Control.Monad.Reader
import Control.Monad.State
import Data.Digest.Pure.MD5
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as BS
import Debug.Trace 
import Data.Function
import Data.List
import Text.JSON.Generic
import Text.JSON


version = 1.0 

data FacebookConfig = FacebookConfig {
      apiKey :: String,
      secretKey :: String,
      endPoint :: String
}

data SessionConfig = SessionConfig {
      session :: Maybe GetSession
}

type FacebookM = StateT SessionConfig (ReaderT FacebookConfig IO)

data MkJson = forall a. JSON a => MkJson a | NoEncode String

js :: (JSON a) => a -> MkJson 
js = MkJson

type Params = [(String, MkJson)]
type EncParams = [(String, String)]

runFacebook :: FacebookConfig -> FacebookM a -> IO a
runFacebook config facebook = 
    runReaderT (evalStateT facebook $ SessionConfig Nothing) config 
                              

status_set :: String -> FacebookM String
status_set status = 
    callMethod "facebook.status.set" [("status", js status)]


fql_query :: String -> FacebookM String
fql_query query = callMethod "facebook.fql.query" [("query", js query)]

auth_createToken :: FacebookM String
auth_createToken  = do
  token <- callMethod "facebook.auth.createToken" []
  return $ decodeJSON token

data GetSession = GetSession 
    {session_key :: String,
     uid :: Integer,
     expires :: Integer,
     secret :: String} deriving (Show, Data, Typeable)

auth_getSession :: String -> FacebookM GetSession
auth_getSession token  = do
    session <- callMethod "facebook.auth.getSession" 
               [("auth_token", NoEncode token),
                ("generate_session_secret", js True)]
    return $ decodeJSON session 

friends_get :: FacebookM [Integer]
friends_get  = do
  friends <- callMethod "facebook.friends.get" []
  return $ decodeJSON friends

addStandardParams :: String -> EncParams -> FacebookM EncParams 
addStandardParams method params = do
  config <- ask
  sessionHolder <- get
  time <- liftIO getPOSIXTime
  let base = [("method", method),
              ("v", show version),
              ("api_key", apiKey config),
              ("call_id", init $ show time),
              ("format", "json")] ++ params
  return $ maybe base (\s -> ("session_key", session_key s):base)
        (session sessionHolder)

postRequest :: String -> EncParams -> FacebookM (Maybe String)
postRequest endPoint params = 
    liftIO $ wget endPoint [] params

encodeParams = map enc 
    where
      enc (s, MkJson p) =  (s, encode p) 
      enc (s, NoEncode p ) = (s, p)
callMethod ::  String -> Params -> FacebookM String 
callMethod method params = do
  let encodedParams = encodeParams params
  config <- ask
  sessHolder <- get
  params' <- addStandardParams method $ trace (show encodedParams)  encodedParams
  let signature = sign params' $ 
                  maybe (secretKey config) secret (session sessHolder)
  liftIO (print (signature ++ "\n"))
  let finalParams = ("sig", signature) : params'
  Just result_str <- postRequest (endPoint config) finalParams
  liftIO $ print result_str
  return $ result_str
  
sign ::  EncParams -> String -> String
sign params secret =
    show $ md5 $ BS.pack $ trace all all
        where 
          all = (concat $ map (\(a,b) -> a ++ "=" ++ b) sparams) ++ secret
          sparams = sortBy (compare `on` fst) params            


