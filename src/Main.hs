module Main where

import Control.Monad
import Control.Monad.Reader
import Data.Binary
import Data.List
import Data.Time
import FUtil
import System.Console.GetOpt
import qualified Client.Facebook as Client
import qualified Client.Login as Login
import Friends
import System.Directory
import System.Environment
import System.FilePath
import qualified HSH as HSH

version = [0, 0]

data Options = Options {
  optHelp :: Bool,
  optVersion :: Bool,
  optReFetchCached :: Bool
}

defOpts :: Options
defOpts = Options {
  optHelp = False,
  optVersion = False,
  optReFetchCached = False
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "h" ["help"] (NoArg (\ o -> o {optHelp = True}))
    "Show this help",
  Option "" ["version"] (NoArg (\ o -> o {optVersion = True}))
    "Show version",
  Option "c" ["re-fetch-cached"] (NoArg (\ o -> o {optReFetchCached = True}))
    "Re-fetch any cached data"
  {-
  Option "" [""] (ReqArg (\ a o -> o {opt = read a}) "")
    ""
  Option "" [""] (OptArg (\ a o -> o {opt = read a}) "")
    ""
  Option "" [""] (NoArg (\ o -> o {opt = }))
    ""
  -}
  ]

baseConfig = Client.FacebookConfig 
             {Client.apiKey = "80d6cc5ca397c92c9cd41cfe09380b9d",
              Client.secretKey = "64f66142cb325b26cc535f5bf2646957", 
              Client.endPoint = "http://api.facebook.com/restserver.php"}


fb = Client.runFacebook baseConfig 


fbCmds = [
  fbShowCmds,
  fbFriends,
  fbFriends2,
  fbHi,
  fbHome,
  fbProfile,
  fbPoke,
  fbStatus,
  fbInit,
  fbProfilePic
  ]

data FbCmd = FbCmd {
  fbCmdName :: String,
  fbCmdHelp :: String,
  fbCmdFunc :: (Options, [String]) ->  Client.FacebookM ()
  }


-- maybe this is a silly way to do this.
-- after all, we could extract the command list from help.
-- anyway, this was easy.
fbShowCmds =
  FbCmd "commands" "Show list of all commands (for tab-completion)" . const .
  liftIO . putStr . unlines $ map fbCmdName fbCmds

ensureDir dir = unlessM (doesDirectoryExist dir) $ createDirectory dir

day = error "todo day"

getFriends :: Client.FacebookM [Int]
getFriends = error "todo getFriends"

getUser :: Client.FacebookM Integer
getUser = Login.showLoginScreen

cache :: (Binary a) => Options -> String -> UTCTime -> Client.FacebookM a -> Client.FacebookM a
cache opts name time f = do 
  when (optReFetchCached opts) $ liftIO $ do
    home <- getHomeDirectory
    let textbookDir = home </> ".TextBook"
        cacheDir = textbookDir </> "cache"
        expFN = name ++ ".exp"
        binFN = name ++ ".bin"
    ensureDir textbookDir
    ensureDir cacheDir
    doesFileExist expFN >>= \ t -> if t
      then decodeFile expFN
      else error "wat" --return Nothing
  f


fbInit = FbCmd "init" "Initialize a facebook account" $ \ (opts,args) -> do 
           user  <- cache opts "user" day getUser
           liftIO $ print user

fbFriends = FbCmd "friends" "Show list of all friends" $ \ (opts, args) -> do
  friends <- cache opts "friends" day getFriends
  -- fix this
  liftIO $ print friends


fbFriends2 = FbCmd "friends2" "Get your friend list" $ \(opts, args) ->  do 
    user  <- cache opts "user" day getUser
    friends <- fetchFriends user
    liftIO $ mapM_ (\(Friend name uid _) ->  print name) friends 

fbProfilePic = FbCmd "profilepic" "See a profie pic" $ \(opts, [user])-> do 
    _  <- cache opts "user" day getUser
    friend <-  fetchFriend user
    liftIO $ HSH.runIO ("firefox \"" ++ pic_big friend ++ "\"")


fbHi = FbCmd "hi" "Just for testing/lols" . const $
  liftIO $ print "hi"

fbHome = FbCmd "home" "Show home page information" . const $
  error "TODO home"

fbProfile = FbCmd "poke" "Check pokes or poke someone" . const $
  error "TODO poke"

fbPoke = FbCmd "profile" "Show profile page information" . const $
  error "TODO profile"

fbStatus = FbCmd "status" "Get/set your status" $ \(opts, [status])-> do 
    _  <- cache opts "user" day getUser
    friend <-  Client.status_set status
    liftIO $ print $ "Status set to: " ++ status

commandList :: String
commandList = intercalate "\n" . ("commands:":) . spaceTable $
  map (\ c -> ["  ", fbCmdName c, " ", fbCmdHelp c]) fbCmds

lookupCmd :: String -> [FbCmd]
lookupCmd cmd = filter ((cmd `isPrefixOf`) . fbCmdName) fbCmds

main :: IO ()
main = do
  let usage = "usage: fb COMMAND [ARGS]"
  args <- getArgs
  (opts, cmdAndArgs) <- case getOpt Permute options args of
    (o, n, [])   -> return (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $
      concat errs ++ usageInfo usage options ++ commandList
  if optVersion opts
    then putStrLn . intercalate "." $ map show version
    else if null cmdAndArgs || optHelp opts
      then putStrLn $ usageInfo usage options ++ commandList
      else do
        let (cmd:args) = cmdAndArgs
        case lookupCmd cmd of
          [c] -> fb $ fbCmdFunc c (opts, args)
          []  -> error $ "no commands matched: " ++ cmd ++ "\n" ++
            usageInfo usage options ++ commandList
          cs  -> error $ "command prefix is ambiguous: " ++ cmd ++ ": " ++
            intercalate " " (map fbCmdName cs)
