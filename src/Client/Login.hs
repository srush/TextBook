module Client.Login where 
import Client.Facebook
import qualified HSH as HSH
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace
import Text.JSON

createLoginUrl :: FacebookConfig -> String -> String
createLoginUrl config authToken = 
    "http://www.facebook.com/login.php?api_key=" ++ 
     apiKey config ++ "&v=" ++ show version 
     ++"&auth_token=" ++ authToken

createExtensionUrl apiKey perm =     
    "http://www.facebook.com/authorize.php?api_key=" ++ 
     apiKey ++ "&v=1.0&ext_perm=" ++ perm

waitForUser :: String -> IO ()
waitForUser url = do 
    HSH.runIO ("firefox \"" ++ url ++ "\"")
    print  url
    print  "Press enter when done..."
    getLine
    return ()

showLoginScreen :: FacebookM ()
showLoginScreen = do
  config <- ask
  token <- auth_createToken
  liftIO $ waitForUser $ createLoginUrl config token
  session <- auth_getSession token
  put (SessionConfig $ Just $ session)

askPermission :: String -> FacebookM ()
askPermission perm = do
    config <- ask
    liftIO $ waitForUser $ createExtensionUrl (apiKey config) perm