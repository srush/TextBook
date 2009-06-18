module Client.Login where 
import Client.Facebook
import qualified HSH as HSH
import HSH ((-|-))
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Debug.Trace
import Text.JSON
import System.Process 
createLoginUrl :: FacebookConfig -> String
createLoginUrl config  = 
    "http://www.facebook.com/login.php?api_key=" ++ 
     apiKey config ++ "&v=" ++ show version 
     ++"&fbconnect=1&connect_display=touch"

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

waitForLogin :: String -> IO(String)
waitForLogin url = do 
    HSH.runIO "rm /tmp/fblynx;touch /tmp/fblynx"
    system $ "lynx -accept-all-cookies --useragent=mozilla -error-file=\"/tmp/fblynx\" \"" ++ url ++ "\""
    auth_token <- HSH.runSL "cat /tmp/fblynx | grep auth_token | sed 's/.*auth_token=\\(\\w*\\) .*/\\1/'"
    return $ trace (show auth_token) auth_token -- strip the newline

showLoginScreen :: FacebookM (Integer)
showLoginScreen = do
  config <- ask
  token <- liftIO $ waitForLogin $ createLoginUrl config
  session <- auth_getSession token
  put (SessionConfig $ Just $ session)
  return $ uid session


askPermission :: String -> FacebookM ()
askPermission perm = do
    config <- ask
    liftIO $ waitForUser $ createExtensionUrl (apiKey config) perm