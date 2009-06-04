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
  fbHi,
  fbHome,
  fbProfile,
  fbPoke,
  fbStatus
  ]

data FbCmd = FbCmd {
  fbCmdName :: String,
  fbCmdHelp :: String,
  fbCmdFunc :: Options -> IO ()
  }


-- maybe this is a silly way to do this.
-- after all, we could extract the command list from help.
-- anyway, this was easy.
fbShowCmds =
  FbCmd "commands" "Show list of all commands (for tab-completion)" . const .
  putStr . unlines $ map fbCmdName fbCmds

ensureDir dir = unlessM (doesDirectoryExist dir) $ createDirectory dir

day = error "todo day"

getFriends :: IO [Int]
getFriends = error "todo getFriends"

cache :: (Binary a) => Options -> String -> UTCTime -> IO a -> IO a
cache opts name time f = do
  when (optReFetchCached opts) $ do
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
  return $ error "todo"

fbFriends = FbCmd "friends" "Show list of all friends" $ \ opts -> do
  friends <- cache opts "friends" day getFriends
  -- fix this
  print friends


--fbFriends = FbCmd "friends" "Get your friend list" $
--  fb $ do 
--    friends <- fetchFriends 4842
--    mapM_ (\(Friend name uid) -> liftIO $ print name) friends 


fbHi = FbCmd "hi" "Just for testing/lols" . const $
  print "hi"

fbHome = FbCmd "home" "Show home page information" . const $
  error "TODO home"

fbProfile = FbCmd "poke" "Check pokes or poke someone" . const $
  error "TODO poke"

fbPoke = FbCmd "profile" "Show profile page information" . const $
  error "TODO profile"

fbStatus = FbCmd "status" "Get/set your status" . const $
  error "TODO status"

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
          [c] -> fbCmdFunc c opts
          []  -> error $ "no commands matched: " ++ cmd ++ "\n" ++
            usageInfo usage options ++ commandList
          cs  -> error $ "command prefix is ambiguous: " ++ cmd ++ ": " ++
            intercalate " " (map fbCmdName cs)
