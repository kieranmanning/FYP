module Run where
import GCompiler
import GDisplay
import GADT
import GEval
import GPrelude
import Haskell2JS

runProg = gmState2JS . compile

