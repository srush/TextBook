module Main where

import Control.Monad
import Data.List
import FUtil
import System.Environment
import System.Console.GetOpt

version = [0, 0]

data Options = Options {
  optHelp :: Bool,
  optVersion :: Bool
}

defOpts :: Options
defOpts = Options {
  optHelp = False,
  optVersion = False
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "h" ["help"] (NoArg (\ o -> o {optHelp = True}))
    "Show this help",
  Option "" ["version"] (NoArg (\ o -> o {optVersion = True}))
    "Show version"
  {-
  Option "" [""] (ReqArg (\ a o -> o {opt = read a}) "")
    ""
  Option "" [""] (OptArg (\ a o -> o {opt = read a}) "")
    ""
  Option "" [""] (NoArg (\ o -> o {opt = }))
    ""
  -}
  ]

fbCmds = [
  fbHome,
  fbProfile,
  fbPoke,
  fbStatus
  ]

data FbCmd = FbCmd {
  fbCmdName :: String,
  fbCmdHelp :: String,
  fbCmdFunc :: IO ()
  }

fbHome = FbCmd "home" "Show home page information" $
  error "TODO"

fbProfile = FbCmd "poke" "Page someone" $
  error "TODO"

fbPoke = FbCmd "profile" "Show profile page information" $
  error "TODO"

fbStatus = FbCmd "status" "Set your status" $
  error "TODO"

commandList :: String
commandList = intercalate "\n" . ("commands:":) . spaceTable $
  map (\ c -> ["  ", fbCmdName c, " ", fbCmdHelp c]) fbCmds

main :: IO ()
main = do
  let usage = "usage: fb COMMAND [ARGS]"
  args <- getArgs
  (opts, commandAndArgs) <- case getOpt Permute options args of
    (o, n, [])   -> return (foldl (flip id) defOpts o, n)
    (_, _, errs) -> error $
      concat errs ++ usageInfo usage options ++ commandList
  if optVersion opts
    then putStrLn . intercalate "." $ map show version
    else if null commandAndArgs || optHelp opts
      then putStrLn $ usageInfo usage options ++ commandList
      else do
        let (command:args) = commandAndArgs
        print "hi"
