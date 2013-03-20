module GCompiler where
import Data.List
import GPrelude
import GADT

---------------------------------------------------------------------------------
-- Compiler.
---------------------------------------------------------------------------------

{-	
 -	This will represent the state of our G-machine.
 -	The components mean the usual.
 -}
type GmState 
	= (	GmCode,
		GmStack,
		GmHeap,
		GmGlobals,
		GmStats)

-- BEGIN GMCODE DEF AND UTILS {
type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, stack, heap, globals, stats) = i 

putCode :: GmCode -> GmState -> GmState
putCode i' (i, stack, heap, globals, stats) =
	(i', stack, heap, globals, stats)

data Instruction 
	= Unwind
	| PushGlobal Name 
	| PushInt Int 
	| Push Int 
	| Mkap
	| Slide Int
	deriving(Show)
instance Eq Instruction where
	Unwind			== Unwind			= True
	PushGlobal n1 	== PushGlobal n2 	= n1 == n2 
	PushInt x 		== PushInt y 		= x == y
	Push x 			== Push y 			= x == y
	Mkap			== Mkap				= True
	Slide x			== Slide y			= x == y
	_				== _				= False
-- } END GMCODE DEF AND UTILS


-- BEGIN GMSTACK DEF AND UTILS {
type GmStack = [Addr]

type Addr = Int

getStack :: GmState -> GmStack 
getStack (i, stack, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack' (i, stack, heap, globals, stats) =
	(i, stack', heap, globals, stats)
-- } END GMSTACK DEF AND UTILS

-- BEGIN GMHEAP {
type GmHeap = Heap Node

-- Heap object count, unused addrs, addr->obj mapping
type Heap a = (Int, [Int], [(Int, a)])

data Node 
	= NNum Int 
	| NAp Addr Addr 
	| NGlobal Int GmCode

getHeap :: GmState -> GmHeap
getHeap (i, stack, heap, globals, stats) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (i, stack, heap, globals, stats) =
	(i, stack, heap', globals, stats)
-- } END GMHEAP

-- BEGIN GmGLOBALS {
type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals 
getGlobals (i, stack, heap, globals, stats) = globals
-- } END GMGLOBALS

-- BEGIN GMSTATS {
type GmStats = Int 

statInitial :: GmStats 
statInitial = 0

statIncSteps :: GmStats -> GmStats 
statIncSteps s = s + 1

statGetSteps :: GmStats -> Int
statGetSteps s = s

getStats :: GmState -> GmStats 
getStats (i, stack, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState 
putStats stats' (i, stack, heap, globals, stats) =
	(i, stack, heap, globals, stats')
-- } END GMSTATS

compile :: CoreProgram -> GmState
compile program =
	(initialCode, [], heap, globals, statInitial)
	where
		(heap, globals) = buildIinitialHeap program

buildIinitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildIinitialHeap program 
	= mapAccuml allocateSc hInitial compiled
	where
		compiled = (map compileSc program) ++ compiledPrimitives

type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) =
	(heap', (name, addr))
	where
		(heap', addr) = hAlloc heap (NGlobal nargs instns)

initialCode :: GmCode 
initialCode = [PushGlobal "main", Unwind]

compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
	= (name, length env, compileR body (zip env [0..]))

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = ASSOC Name Int

compileR :: GmCompiler
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]

compileC :: GmCompiler 
compileC (EVar v) env 
	| elem v (aDomain env)			= [Push n]
	| otherwise						= [PushGlobal v]
	where
		n = aLookup env v (error "shit the bed")
compileC (ENum n) env 				= [PushInt n]
compileC (EAp e1 e2) env 			= compileC e2 env ++ 
									  compileC e1 (argOffset 1 env) ++
									  [Mkap]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env] 

---------------------------------------------------------------------------------
-- Compiler Primitives.
---------------------------------------------------------------------------------

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = []

---------------------------------------------------------------------------------
-- Compiler Utils.
---------------------------------------------------------------------------------

mapAccuml :: (a -> b -> (a,c)) 			-- func of accuml, 
										-- elem input list that
										-- returns new accuml, 
										-- elem of result list
			 -> a 						-- initial accumulator
			 -> [b] 					-- input list
			 -> (a, [c])				-- out accumulator, result list
mapAccuml f acc [] = (acc, [])
mapAccuml f acc (x:xs) 	= (acc2, x':xs')
	where 
		(acc1, x') 	= f acc x
		(acc2, xs') 	= mapAccuml f acc1 xs

hInitial :: Heap a
hInitial = (0, [1..100], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n = ((size+1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a 
hUpdate (size, free, cts) a n = (size, free, (a, n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size-1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a 
hLookup (size, free, cts) a = 
	aLookup cts a (error ("not found node" ++ showaddr a))

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Int
hSize (size, free, cts) = size

hNull :: Addr 
hNull = 0

hIsNull :: Addr -> Bool
hIsNull a = (a == 0)

showaddr a = "#" ++ show a

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ("nonexistant addres")
remove ((a', n):cts) a 	| a == a' = cts
						| a /= a' = (a', n) : remove cts a

type ASSOC a b = [(a, b)]

aLookup [] k' def = def 
aLookup ((k,v):bs) k' def 	| k == k' = v 
							| k /= k' = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key, val) <- alist]							