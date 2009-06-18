module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Binary
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
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
  io . putStr . unlines $ map fbCmdName fbCmds

ensureDir dir = unlessM (doesDirectoryExist dir) $ createDirectory dir

day :: NominalDiffTime
day = 24 * 60 * 60

cache :: (Binary a, MonadIO m) =>
  Options -> String -> NominalDiffTime -> m a -> m a
cache opts name time f = do
  home <- io $ getHomeDirectory
  let textbookDir = home </> ".TextBook"
      cacheDir = textbookDir </> "cache"
      expFN = cacheDir </> name ++ ".exp"
      binFN = cacheDir </> name ++ ".bin"
  nowTime <- io $ getCurrentTime
  cacheValMb <- io $ do
    ensureDir textbookDir
    ensureDir cacheDir
    if optReFetchCached opts then return Nothing else
      doesFileExist expFN >>= \ t -> if t
        then do
          expireTime <- posixSecondsToUTCTime . fromIntegral <$>
            (decodeFile expFN :: IO Int)
          if nowTime < expireTime
            then doesFileExist binFN >>= \ t' -> if t'
              then Just <$> decodeFile binFN
              else return Nothing  -- this shouldn't happen..
            else return Nothing
        else return Nothing
  case cacheValMb of
    Just val -> return val
    Nothing -> do
      val <- f
      io $ encodeFile expFN
        (round . utcTimeToPOSIXSeconds $ addUTCTime time nowTime :: Int)
      io $ encodeFile binFN val
      return val

getFriends :: Client.FacebookM [Int]
getFriends = error "todo getFriends"
{-
getFriends = do
  t <- io $ getPOSIXTime
  return [4, 6, round t]
-}

getUser :: Client.FacebookM Integer
getUser = Login.showLoginScreen

fbInit = FbCmd "init" "Initialize a facebook account" $ \ (opts,args) -> do
  user <- cache opts "user" day getUser
  io $ print user

fbFriends = FbCmd "friends" "Show list of all friends" $ \ (opts, args) -> do
  friends <- cache opts "friends" day getFriends
  -- fix this
  io $ print friends

fbFriends2 = FbCmd "friends2" "Get your friend list" $ \ (opts, args) -> do
  user <- cache opts "user" day getUser
  friends <- fetchFriends user
  io $ mapM_ (\ (Friend name uid _) ->  print name) friends

fbProfilePic = FbCmd "profilepic" "See a profie pic" $ \ (opts, [user]) -> do
  cache opts "user" day getUser
  friend <- fetchFriend user
  io $ HSH.runIO ("firefox \"" ++ pic_big friend ++ "\"")

fbHi = FbCmd "hi" "Just for testing/lols" . const $
  io $ print "hi"

fbHome = FbCmd "home" "Show home page information" . const $
  error "TODO home"

fbProfile = FbCmd "poke" "Check pokes or poke someone" . const $
  error "TODO poke"

fbPoke = FbCmd "profile" "Show profile page information" . const $
  error "TODO profile"

fbStatus = FbCmd "status" "Get/set your status" $ \ (opts, [status]) -> do
  cache opts "user" day getUser
  friend <- Client.status_set status
  io $ print $ "Status set to: " ++ status

commandList :: String
commandList = intercalate "\n" . ("commands:":) . spaceTable $
  map (\ c -> ["  ", fbCmdName c, " ", fbCmdHelp c]) fbCmds

lookupCmd :: String -> [FbCmd]
lookupCmd cmd = case filter ((== cmd) . fbCmdName) prefixMatches of
  [match] -> [match]
  _ -> prefixMatches
  where
  prefixMatches = filter ((cmd `isPrefixOf`) . fbCmdName) fbCmds

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
