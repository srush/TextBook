module Main where

import FUtil
import System.Console.GetOpt

data Options = Options {
  optHelp :: Bool
}

defOpts :: Options
defOpts = Options {
  optHelp = False
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "h" ["help"] (NoArg (\ o -> o {optHelp = True))
    "Show this help."
  {-
  Option "" [""] (ReqArg (\ a o -> o {opt = read a}) "")
    ""
  Option "" [""] (OptArg (\ a o -> o {opt = read a}) "")
    ""
  Option "" [""] (NoArg (\ o -> o {opt = }))
    ""
  -}
  ]

commands = [
  ("home",    fbHome),
  ("profile", fbProfile),
  ("poke",    fbPoke),
  ("status",  fbStatus)
  ]

fbHome = error "TODO"

fbProfile = error "TODO"

fbPoke = error "TODO"

fbStatus = error "TODO"

main :: IO ()
main = do
  let usage = "usage: TODO"
  (opts, args) <- doArgs usage defOpts options
  print "hi"
  --let (command:args)
