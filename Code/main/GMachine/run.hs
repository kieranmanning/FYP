module Run where
import GCompiler
import GDisplay
import GADT
import GEval
import GPrelude

--eval :: GmState -> [GmState] 
--compile :: CoreProgram -> GmState
runProg = eval . compile

test = last . eval . compile

testGlobals prog = s 
	where (c, s, h, g, st) = last ( eval ( compile prog ))

testStack prog = s 
	where (c, s, h, g, st) = last ( eval ( compile prog ))

testCode prog = "Uneval'd code from compiling: " ++ (show c)
	where (c, s, h, g, st) = compile prog 

--testHeap prog = h 
--	where (c, s, h, g, st) = 

--testGetAllFuncs :: GmState -> 
testGetAllFuncs (c, s, h, g, st) = 
	map (hLookup h) (map snd g) 

emptyState :: GmState
emptyState = ([], [], (0, [0..20], []), [], 0)

{-
	JavaScript strategy.

	Names of our functions will reside in Globals after compilation.
	hLookup heap globals[f] will return the code to construct f 
	


-}