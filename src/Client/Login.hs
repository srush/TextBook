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
    "http://m.facebook.com/authorize.php?api_key=" ++ 
     apiKey ++ "&v=1.0&ext_perm=" ++ perm
     ++ "&next=http://fbplatform.mancrushonmcslee.com/textbook/permission.php"

waitForUser :: String -> IO ()
waitForUser url = do 
    HSH.runIO ("firefox \"" ++ url ++ "\"")
    print  url
    print  "Press enter when done..."
    getLine
    return ()


showBrowser :: String -> Maybe String -> IO () 
showBrowser url useragent = do
    system $ "lynx -accept-all-cookies "
               ++ maybe "" ("--useragent="++) useragent
               ++" -error-file=\"/tmp/fblynx\" \"" ++ url ++ "\""
    return ()

waitForLogin :: String -> IO(String)
waitForLogin url = do 
    HSH.runIO "rm /tmp/fblynx;touch /tmp/fblynx"
    showBrowser url $ Just "mozilla"
    auth_token <- HSH.runSL "cat /tmp/fblynx | grep auth_token | sed 's/.*auth_token=\\(\\w*\\) .*/\\1/'"
    return auth_token -- strip the newline

showLoginScreen :: FacebookM (GetSession)
showLoginScreen = do
  config <- ask
  token <- liftIO $ waitForLogin $ createLoginUrl config
  session <- auth_getSession token
  return session


askPermission :: String -> FacebookM ()
askPermission perm = do
    config <- ask
    liftIO $ showBrowser (createExtensionUrl (apiKey config) perm) Nothing