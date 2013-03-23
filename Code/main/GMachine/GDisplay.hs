module GDisplay where 
import GEval
import GCompiler
import GADT

---------------------------------------------------------------------------------
-- Displaying results. 
---------------------------------------------------------------------------------

data Iseq 
	= INil 
	| IStr String 
	| IAppend Iseq Iseq
	| IIndent Iseq
	| INewline
	deriving(Show)

--Miranda inbuilt function {
spaces :: Int -> [Char]
spaces x = spaceAcc x []

spaceAcc :: Int -> [Char] -> [Char]
spaceAcc 0 sps 		= sps 
spaceAcc inc sps 	= spaceAcc (inc - 1) (' ':sps)
--}

iNum :: Int -> Iseq 
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n = 
	iStr (spaces (width - length digits) ++ digits)
		where
			digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
	where
		lay_item (n, sseq) =
			iConcat [ iFWNum 4 n, iStr "( ", iIndent sseq, iNewLine]

iNil :: Iseq 
iNil = INil 

iStr :: String -> Iseq 
iStr str = IStr str 

iAppend :: Iseq -> Iseq -> Iseq 
iAppend seq1 seq2 = IAppend seq1 seq2

iNewLine :: Iseq 
iNewLine = IStr "\n"

iIndent :: Iseq -> Iseq 
iIndent seq = seq 

iDisplay :: Iseq -> String
iDisplay seq = flatten [seq]

iConcat :: [Iseq] -> Iseq 
iConcat [] = INil
iConcat (x:xs) = x `iAppend` iConcat xs

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = INil
iInterleave _ [x] = x
iInterleave sep (x:xs) = x `iAppend` sep `iAppend` iInterleave sep xs

flatten :: [Iseq] -> String
flatten [] = ""
flatten (INil 	: xs) = flatten xs
flatten (IStr s : xs) = s ++ (flatten xs)
flatten (IAppend s1 s2 : seqs) = flatten (s1 : s2 : seqs)

showResults :: [GmState] -> [Char]
showResults states = 
	iDisplay (iConcat [
		iStr "Supercombinator definitions", iNewLine,
		iInterleave iNewLine (map (showSC s) (getGlobals s)),
		iNewLine, iNewLine, iStr "State transitions", iNewLine, iNewLine,
		iLayn (map showState states),
		iNewLine, iNewLine,
		showStats (last states)])
		where (s:ss) = states

showSC :: GmState -> (Name, Addr) -> Iseq
showSC s (name, addr) = 
	iConcat [iStr "Code for ", iStr name, iNewLine,
		showInstructions code, iNewLine, iNewLine]
	where
		(NGlobal arity code) = (hLookup (getHeap s) addr)

showInstructions :: GmCode -> Iseq 
showInstructions is =
	iConcat [iStr "  Code : {",
		iIndent (iInterleave iNewLine (map showInstruction is)),
		iStr "}", iNewLine]

showInstruction :: Instruction -> Iseq
showInstruction Unwind			= iStr "Unwind"
showInstruction (PushGlobal f) 	= (iStr "PushGlobal ") 	`iAppend` (iStr f)
showInstruction (Push n)		= (iStr "Push ")		`iAppend` (iNum n)
showInstruction (PushInt n)		= (iStr "PushInt ")		`iAppend` (iNum n)
showInstruction Mkap			= iStr "Mkap"
--showInstruction (Slide n) 		= (iStr "Slide ")		`iAppend` (iNum n)

showState :: GmState -> Iseq
showState s = 
	iConcat [showStack s, 			iNewLine,
	showInstructions (getCode s), 	iNewLine]

showStack :: GmState -> Iseq 
showStack s = 
	iConcat [
		iStr "Stack [",
		iIndent (iInterleave iNewLine 
			(map (showStackItem s) (reverse (getStack s)))),
		iStr "]"
	]

showStackItem :: GmState -> Addr -> Iseq
showStackItem s a =
	iConcat [iStr (showaddr a), iStr ": ",
		showNode s a (hLookup (getHeap s) a)]


showNode :: GmState -> Addr -> Node -> Iseq
showNode s a (NNum n)		= iNum n
showNode s a (NGlobal n g)	= iConcat [iStr "Global ", iStr v]
	where v = head [n | (n,b) <- getGlobals s, a==b]
showNode s a (NAp a1 a2)	= iConcat [iStr "Ap ", iStr (showaddr a1),
									   iStr " ",   iStr (showaddr a2)]

showStats :: GmState -> Iseq 
showStats s = 
	iConcat [ iStr "steps taken = ", iNum (statGetSteps (getStats s))]
