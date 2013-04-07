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

runProg x = gmState2JS (compile x) 

run = last . eval . compile 

main = do
    args <- getArgs
    let (actions, nonOpts, msgs ) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optInput = input,
                  optOutput = output } = opts
    input >>= output

header = "Usage: main [OPTION...]"


data Options = Options {
    optInput :: IO String,
    optOutput :: String -> IO ()
}

defaultOptions :: Options
defaultOptions = Options {
    optInput = getContents,
    optOutput = putStr
}

options = [Option ['v'] ["version"]         (NoArg showVersion)         "show version number",
           Option ['h'] ["help"]            (NoArg showHelp)            "show help info",
           Option ['i'] ["input"]           (ReqArg readInput "FILE")   "input filename",
           Option ['o'] ["output"]          (ReqArg writeOutput "FILE")   "output filename"]

showVersion :: Options -> IO Options
showVersion _ = do
    putStrLn "Spaskell Catalyst version 0.0"
    exitWith ExitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
    putStrLn "Useful help output, coming soon!"
    exitWith ExitSuccess

readInput :: Options -> IO Options
readInput arg opt = return opt { optInput = readFile arg }

writeOutput :: Options -> IO Options
writeOutput arg opt = return opt { optOutput = writeFile arg }
