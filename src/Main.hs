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
import qualified Events as E
import qualified FriendFast as FF
import Stream
import Data.Char
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf
import qualified HSH as HSH
import System.Process 
import Data.Map ((!))
import Text.JSON.Types
import System.Process 
import System.IO
import Data.Maybe (mapMaybe)
version = [0, 0]

type Permission = String
type Uid = Integer
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
              Client.secretKey = "282d51185be4ed0a8c28def8b8688b1",
              Client.endPoint = "http://api.srush2.devrs006.facebook.com/restserver.php"}


fb = Client.runFacebook baseConfig


fbCmds = [
  fbInit,
  fbShowCmds,
  fbFriends,
  fbProfile,
  fbProfilePic,
  fbBirthday,
  fbFinger,
  fbStream,
  fbUserStream,
  fbEvent,
  fbStatus,
  fbWall
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


requestSession :: Client.FacebookM Client.GetSession
requestSession = Login.showLoginScreen 

ensureLogin :: Client.FacebookM Integer
ensureLogin = do
  -- todo, deal with opts
  session <- cache defOpts "session" day requestSession
  Client.addSession session
  Just user <- Client.getUser
  return user


requestPermission perm = Login.askPermission perm

ensurePermission :: Permission -> Client.FacebookM ()
ensurePermission perm = do 
  -- todo, deal with opts
  cache defOpts ("perm1:"++perm) day $ requestPermission perm


fbInit = FbCmd "init" "Initialize a facebook account" $ \ (opts,args) -> do
  user <- ensureLogin
  io $ printf "%d \n" user

--fbFriends = FbCmd "friends" "Show list of all friends" $ \ (opts, args) -> do
  --user <- ensureLogin
  --friends <- getFriends -- cache opts "friends" day
  -- fix this
  --io $ print friends

fbFriends = FbCmd "friends" "Get your friend list" $ \ (opts, args) -> do
  user <- ensureLogin
  friends <- FF.fetchFriendsFast user
  io $ mapM_ (printf "Name: %s\n" . FF.name) friends


whenJust = maybe ([]) 

ddize :: [(String,String)] -> String
ddize assoc =
  printf "<dl>" ++
  (concat $ map (\(a,b) -> printf "<dt>%s</dt><dd>%s</dd>" a b ) assoc) ++  
  printf "</dl>"

renderFriend :: Friend -> [(String,String)]
renderFriend friend = 
  
  [("Name:", name friend)] ++
  [("Uid:",  show $(uid friend))] ++
  whenJust (\f->  [("Birthday:",  f)])  (birthday friend) ++ 
  whenJust (\f->  [("Music:",  f)])  (music friend) ++ 
  whenJust (\f->  [("TV:",  f)])  (tv friend) ++ 
  whenJust (\f->  [("Books:",  f)])  (books friend) ++ 
  whenJust (\f->  [("Quotations:",  f)])  (quotes friend) ++ 
  whenJust (\f->  [("About:",  f)])  (about_me friend)
  
renderEvent event = 
  [("Name:",  E.name event)] ++
  whenJust (\f-> [("Tagline:", f)]) (E.tagline event) ++ 
  whenJust (\f-> [("Description:", f)]) (E.description event) 
  

nameToUid user fname = 
    if isDigit $ head fname then 
        return $ Just $ read fname 
    else do
  trie <- FF.fetchFriendStorage user
  case FF.getFriendsWithName trie fname of 
    [(_, f)] -> return $ Just $ FF.uid f
    [] -> do 
      io $ putStrLn $ "No friends name " ++ fname
      return Nothing
    ls -> do
      io $ putStrLn ("Many users with names starting with " ++ fname) 
      io $ mapM_ (\(_,f) -> printf "%d - %s\n"  (FF.uid f) (FF.name f)) $ ls
      return Nothing


showHTML rendered = do 
  file <- openFile "/tmp/fbtextbook" WriteMode
  hPutStrLn file $ rendered
  hClose file
  system "cat /tmp/fbtextbook | w3m -T text/html"
  return () 


fbFinger = FbCmd "finger" "Get a friend's info" $ \ (opts, [fname]) -> do
  user <- ensureLogin
  uid <- nameToUid user fname
  case uid of 
    Just uid -> do 
        friend <- fetchFriend uid
        io $ showHTML $ ddize $ renderFriend friend
    Nothing -> 
        return () 

fbEvent = FbCmd "events" "Get all my events" $ \ (opts, _) -> do
  user <- ensureLogin
  events <- E.fetchEvents user
  io $ showHTML $ concat $ map(\e -> (ddize (renderEvent e) ++  "<br>")) events


fbProfilePic = FbCmd "profilepic" "See a profie pic" $ \ (opts, [fname]) -> do
  user <- ensureLogin
  fuid <- nameToUid user fname
  case fuid of 
    Just fuid -> do 
             friend <- fetchFriend fuid
             io $ case pic_big friend of 
                 Just pic -> do {system ("jp2a --colors \"" ++ pic ++ "\"");return ()}
                 Nothing  -> putStrLn "No pic available"
    Nothing -> 
        return () 




fbHi = FbCmd "hi" "Just for testing/lols" . const $
  io $ print "hi"

fbHome = FbCmd "home" "Show home page information" . const $
  error "TODO home"

fbProfile = FbCmd "poke" "Check pokes or poke someone" . const $
  error "TODO poke"

fbPoke = FbCmd "profile" "Show profile page information" . const $
  error "TODO profile"

fbWall = FbCmd "wall" "Write on a friends wall" $ \ (opts, [fname, message]) -> do
  user <- ensureLogin
  ensurePermission "publish_stream"
  uid <- nameToUid user fname 
  case uid of 
    Just uid -> do
        Client.stream_publish message $ Just uid
        io $ printf "Wrote \"%s\" on to: %s\n" message uid
    Nothing -> return ()

fbStatus = FbCmd "status" "Set your status" $ \ (opts, [status]) -> do
  user <- ensureLogin
  ensurePermission "publish_stream"
  Client.stream_publish status Nothing
  io $ printf "Status set to: %s\n" status

fbStream = FbCmd "stream" "Get your stream" $ \ (opts, _) -> do
  user <- ensureLogin
  ensurePermission "read_stream"
  (stream, idmap) <- fetchStream user
  
  io $ showHTML $ ddize $ mapMaybe (format idmap) stream
    where format idmap (JSObject sobj) = case get_field sobj "message" of 
                                 Just (JSString m) -> Just $ ((idmap ! (sobj !^ "actor_id")), (fromJSString m))
                                 _ -> Nothing
          format  _ _= Nothing


fbUserStream = FbCmd "userstream" "Get a users stream" $ \ (opts, [fname]) -> do
  user <- ensureLogin
  ensurePermission "read_stream"
  uid <- nameToUid user fname 
  case uid of 
    Just uid -> do
        (stream, idmap) <- fetchUserStream uid
        io $ showHTML $ ddize $ mapMaybe (format idmap) stream
    Nothing -> return ()
   where format idmap (JSObject sobj) = case get_field sobj "message" of 
                                               Just (JSString m) -> Just $ ((idmap ! (sobj !^ "actor_id")), (fromJSString m))
                                               _ -> Nothing
         format  _ _= Nothing


fbBirthday = FbCmd "birthday" "Get birthdays for a given day" $ \ (opts, [date]) -> do
  user <- ensureLogin
  friends <- fetchFriends user
  let friends' = filter (maybe False (isPrefixOf date) . birthday) friends
  io $ mapM_ (printf "Birthday: %s\n" . show .birthday) friends'


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
