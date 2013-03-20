module Main where
import Compiler

main = do
    putStrLn "---Test program---"
    putStrLn (show idTest)
    putStrLn "---Result --- "
    putStrLn (runCore2JS idTest)
