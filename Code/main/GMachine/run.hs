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

gmState2String :: GmState -> String 
gmState2String (c, s, h, g, st) = 
	"var GmCode = " 	++ show(c) ++ " ; \n" ++
	"var GmStack = " 	++ show(s) ++ " ; \n" ++
	"var GmGlobals = " 	++ show(g) ++ " ; \n"

gmCode2String :: GmCode-> String -> String
gmCode2String (x:xs) acc
	| x == (PushGlobal name) = "var" ++ name ++ "=PushGlobal(" 
	  ++ name ++ ");" ++ (gmCode2String xs acc)
	| x == Unwind = "var unwind = Unwind();" 
	  ++ (gmCode2String xs acc)

-}