module Main where
import GCompiler
import GDisplay
import GADT
import GEval
import GPrelude
import Haskell2JS
import Data.Maybe
import System.IO
import System.Environment
import System.Console.GetOpt

runProg x = gmState2JS (compile x) 

run = last . eval . compile 

main = do
	[i, o] <- getArgs
	f <- readFile i
	r <- readFile "runtime.js"
	writeFile o $ r ++ (runProg (read f))

{-
data Flag = Version | OutFile String | InFile String
	deriving (Show)

options :: [OptDescr Flag]
options = [Option ['v'] ["version"] (NoArg Version) "show version number", 
		   Option ['o'] ["outfile"] (ReqArg outp "FILE") "specify name of output file"]

inp, outp :: Maybe String -> Flag 
outp = OutFile "stdout"
inp = InFile . fromMaybe "stdin" 

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
	case (getOpt Permute options argv) of
	   (o,n,[]  ) -> return (o,n)
	   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."
  -}