module GCompiler where
import Data.List
import GPrelude
import GADT

---------------------------------------------------------------------------------
-- Compiler State Representations and Utils.
---------------------------------------------------------------------------------

{-	
 -	This will represent the state of our G-machine.
 -	The components mean the usual.
 -}
type GmState 
	= (	GmCode,
		GmStack,
		GmDump,
		GmHeap,
		GmGlobals,
		GmStats)

-- BEGIN GMCODE DEF AND UTILS {
type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (i, stack, dump, heap, globals, stats)  = i 

putCode :: GmCode -> GmState -> GmState
putCode i' (i, stack, dump, heap, globals, stats) =
	(i', stack, dump, heap, globals, stats)

data Instruction 
	= Unwind
	| PushGlobal Name 
	| PushInt Int 
	| Push Int 
	| Mkap
	| Pop Int
	| Alloc Int 
		| Update Int
	| Eval
	| Add | Sub | Mul | Div
	| Eq  | Ne  | Lt  | Le | Gt | Ge
	| Cond GmCode GmCode
	deriving(Eq, Show)

{-}
instance Eq Instruction where
	Unwind			== Unwind			= True
	PushGlobal n1 	== PushGlobal n2 	= n1 == n2 
	PushInt x 		== PushInt y 		= x == y
	Push x 			== Push y 			= x == y
	Mkap			== Mkap				= True
	Update x 		== Update y 		= x == y
	Pop x			== Pop y 			= x == y
	Slide x			== Slide y			= x == y
	_				== _				= False
-}
-- } END GMCODE DEF AND UTILS

type GmDump = [GmDumpItem]

type GmDumpItem = (GmCode, GmStack)

getDump :: GmState -> GmDump 
getDump (code, stack, dump, heap, globals, stats) = dump 

putDump :: GmDump -> GmState -> GmState
putDump dump' (i, stack, dump, heap, globals, stats) =
	(i, stack, dump', heap, globals, stats)

dumpInitial = []

-- BEGIN GMSTACK DEF AND UTILS {
type GmStack = [Addr]

type Addr = Int

getStack :: GmState -> GmStack 
getStack (i, stack, dump, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack' (i, stack, dump, heap, globals, stats) =
	(i, stack', dump, heap, globals, stats)
-- } END GMSTACK DEF AND UTILS

-- BEGIN GMHEAP {
type GmHeap = Heap Node

-- Heap object count, unused addrs, addr->obj mapping
type Heap a = (Int, [Int], [(Int, a)])

-- NNum value | NAp Addr Addr | NGlobal arity instructions
data Node 
	= NNum Int 
	| NAp Addr Addr  		
	| NGlobal Int GmCode 	-- NGlobal Arity Code
	| NInd Addr 			-- Indirection
	deriving(Eq, Show)

getHeap :: GmState -> GmHeap
getHeap (i, stack, dump, heap, globals, stats) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (i, stack, dump, heap, globals, stats) =
	(i, stack, dump, heap', globals, stats)
-- } END GMHEAP

-- BEGIN GmGLOBALS {
type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals 
getGlobals (i, stack, dump, heap, globals, stats) = globals
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
getStats (i, stack, dump, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState 
putStats stats' (i, stack, dump, heap, globals, stats) =
	(i, stack, dump, heap, globals, stats')
-- } END GMSTATS

---------------------------------------------------------------------------------
-- Compiler.
---------------------------------------------------------------------------------

{-	Control Flow
 -	
 -	compile = \program -> buildHeap program
 -
 - 	buildHeap compiles program + prelude to
 -
 -}

-- Take ScDefns and start with init'd empty GmState
compile :: CoreProgram -> GmState
compile program =
	(initialCode, [], dumpInitial, heap, globals, statInitial)
	where
		(heap, globals) = buildInitialHeap program

-- Same as TemplateInst. Build heap and globals with 
-- starting ScDefns
buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program 
	= mapAccuml allocateSc hInitial compiled
	where
		compiled = map compileSc (program ++ preludeDefs) ++ compiledPrimitives

-- Represents an SC compiled to its global name, arity and instructions
type GmCompiledSC = (Name, Int, GmCode)

-- Takes a compiled SC and creates a global with its arity
-- and instructions which it then hallocs
allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) =
	(heap', (name, addr))
	where
		(heap', addr) = hAlloc heap (NGlobal nargs instns)

-- Starting code
initialCode :: GmCode 
initialCode = [PushGlobal "main", Unwind]

-- env of [NodeName, StackLocation]
type GmEnvironment = ASSOC Name Int
-- The type of our compiler schemes
type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

-- Takes an standard Core SC and returns (Global name, Arity, Insts)
-- Follows semantics of compilerR from SPJ implementorial
-- Mostly just handoff to compileR
compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body)
	= (name, length env, compileR body (zip env [0..]))


-- CoreExpr -> (Assoc Name NumElemsToSlide) -> GmCode
-- hands off to compileC to handle pattern match and slides
-- n' unwinds
compileR :: GmCompiler
--compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
	where d = (length env)

-- CoreExpr -> (Assoc Name NumElemsToSlide) -> GmCode
-- Generates code to construct graph of an expe in env
-- and leave a pointer to it on top of stack
compileC :: GmCompiler 
compileC (EVar v) env 
	| elem v (aDomain env)			= [Push n]
	| otherwise						= [PushGlobal v]
	where n = aLookup env v (error "shit the bed")
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


{-
 -	*NOTE* heap set [1..20] for ease of analysis
 -}
hInitial :: Heap a
hInitial = (0, [1..20], [])

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