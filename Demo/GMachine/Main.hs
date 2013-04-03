module Main where
import GCompiler
import GDisplay
import GEval
import GPrelude

runProg = showResults . eval . compile 

main = do 

