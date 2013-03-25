module Haskell2JS where
import GCompiler
import GDisplay
import GADT
import GEval
import GPrelude
import Data.List

type JS = String

gmState2JS :: GmState -> JS 
gmState2JS (c, s, d, h, g, st) =
	gmCode2JS c ++ "\n \n" ++ 
	gmStack2JS s ++ "\n \n" ++ 
	gmDump2JS d ++ "\n \n" ++ 
	gmHeap2JS h ++ "\n \n" ++ 
	gmGlobals2JS g ++ "\n \n" ++
	"var GmState = [GmCode, GmStack, GmHeap, GmGlobals] \n \n" ++
	"function main(){\n\treturn evalx(GmState);\n}"

gmCode2JS :: GmCode -> JS
gmCode2JS is = "var GmCode = [" ++ isJsList ++ "];"
	where
		isJsList = intercalate "," (map gmInstruction2JS is)

gmStack2JS :: GmStack -> JS 
gmStack2JS _ = "var GmStack = [];"

gmDump2JS :: GmDump -> JS 
gmDump2JS _ = "var GmDump = [];"

gmGlobals2JS :: GmGlobals -> JS
gmGlobals2JS g = "var GmGlobals = {" ++ kvlist ++ "};"
	where
		kv x = (show(fst x)) ++ ":" ++ (show(snd x))
		kvlist = intercalate "," (map kv g)

gmHeap2JS :: GmHeap -> JS
gmHeap2JS (oc, fa, h) = 
	"var GmHeap = {\nobjCount:" ++ (show oc) ++ ",\n" ++
	"freeAddrs:" ++ (show fa) ++ ",\n" ++ 
	"addrObjMap:{" ++ (addrObjMap2JS h) ++ "}\n};"

addrObjMap2JS :: [(Int, Node)] -> JS 
addrObjMap2JS x = intercalate "," (map addrObj2JS x)

addrObj2JS :: (Int, Node) -> JS 
addrObj2JS (x, n) = show x ++ ":" ++ (gmNode2JS n)

gmNode2JS :: Node -> JS
gmNode2JS n = do
	case n of
		NNum x		 -> "new NNum(" ++ (show x) ++ ")"
		NAp a1 a2 	 -> "new NAp(" ++ (show a1) ++ "," ++ (show a2) ++ ")"		
		NGlobal x is -> "new NGlobal(" ++ (show x)  ++ "," ++ "[" ++
						(intercalate "," (map gmInstruction2JS is)) ++ "])"
		NInd a 		 -> "new NInd(" ++ (show a) ++ ")"


gmInstruction2JS :: Instruction -> JS
gmInstruction2JS i = do 
	case i of
		Unwind		 -> "new Unwind()"
		PushGlobal g -> "new PushGlobal(" ++ (show g) ++ ")"
		PushInt	x	 -> "new PushInt(" ++ (show x) ++ ")"
		Push x 		 -> "new Push(" ++ (show x) ++ ")"
		Mkap		 -> "new Mkap()"
		Pop x 		 -> "new Pop(" ++ (show x) ++ ")"
		Update x 	 -> "new Update(" ++ (show x) ++ ")"
		Eval 		 -> "new Eval()"
		Add 		 -> "new Add()"
