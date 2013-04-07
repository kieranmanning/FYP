module Main where
import GCompiler
import GDisplay
import GADT
import GEval
import GPrelude
import Haskell2JS
import Data.List
import Data.Maybe
import Control.Monad
import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt

runProg = gmState2JS . compile 

run = last . eval . compile 

main = do
  args <- getArgs
  let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optInput = input,
                optOutput = output,
                optRuntime = runtime,
                optCompileOnly = compileOnly } = opts
  src <- input
  case compileOnly of 
	True  -> 	output $ show $ compile  (read src::CoreProgram)
  	False ->  	case runtime of
	  	(True, x)  -> 	output $ runProg (read src::CoreProgram)
	  	(False, _) -> 	output $ runProg (read src::CoreProgram)


data Options = Options  {
    optInput  :: IO String,
    optOutput :: String -> IO (),
    optRuntime :: (Bool, IO String),
    optCompileOnly :: Bool
  }

defaultOptions :: Options
defaultOptions = Options {
    optInput  = error $ "no file specified. run with -h for help",
    optOutput = writeFile "a.out.js",
    optRuntime = (False, error $ "no file specified"),
    optCompileOnly = False  
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"]      (NoArg showVersion)         		"show version number",
    Option ['r'] ["runtime"]      (ReqArg includeRuntime "FILE")    "include runtime in ouput",
    Option ['c'] ["compile-only"] (NoArg compileOnly)         		"compile but don't serialize to JS",
    Option ['h'] ["help"] 		  (NoArg showHelp)         			"Display helpful information",
    Option []	 ["magpie"]		  (NoArg magpie)					"",
    Option ['i'] ["input"]        (ReqArg readInput "FILE")   		"input file to read",
    Option ['o'] ["output"]       (ReqArg writeOutput "FILE") 		"output file to write"
  ]

showVersion _ = do
  putStrLn $ intercalate "\n" cowsay
  exitWith ExitSuccess

cowsay =   [" ________________________________",
 			"< Version 0.0 and holding steady >",
 			" --------------------------------",
 			"       \\   ^__^                     ",
 			"        \\  (oo)\\_______         ",
 			"           (__)\\       )\\/\\     ",
 			"               ||----w |        ",
 			"               ||     ||        "]

showHelp _ = do
  txt <- readFile "help.txt"
  putStrLn txt
  exitWith ExitSuccess

magpie _ = do
	putStrLn "magpie"
	exitWith ExitSuccess

readInput arg opt 		= return opt { optInput 	  	= readFile arg }
compileOnly opt 		= return opt { optCompileOnly 	= True }
includeRuntime arg opt 	= return opt { optRuntime 		= (True, readFile arg) }
writeOutput arg opt 	= return opt { optOutput 		= writeFile arg }

