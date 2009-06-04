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
  fbShowCmds,
  fbHi,
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
  error "TODO home"

fbProfile = FbCmd "poke" "Check pokes or poke someone" $
  error "TODO poke"

fbPoke = FbCmd "profile" "Show profile page information" $
  error "TODO profile"

fbStatus = FbCmd "status" "Get/set your status" $
  error "TODO status"

-- maybe this is a silly way to do this.
-- after all, we could extract the command list from help.
-- anyway, this was easy.
fbShowCmds =
  FbCmd "commands" "Show list of all commands (for tab-completion)" .
  putStr . unlines $ map fbCmdName fbCmds

fbHi = FbCmd "hi" "Just for testing/lols" $
  print "hi"

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
          [c] -> fbCmdFunc c
          []  -> error $ "no commands matched: " ++ cmd ++ "\n" ++
            usageInfo usage options ++ commandList
          cs  -> error $ "command prefix is ambiguous: " ++ cmd ++ ": " ++
            intercalate " " (map fbCmdName cs)
