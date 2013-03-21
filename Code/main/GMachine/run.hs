module Run where
import GCompiler
import GDisplay
import GEval
import GPrelude(idTest)

runProg = showResults . eval . compile
