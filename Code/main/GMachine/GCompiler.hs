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
	= (	GmOutput,
		GmCode,
		GmStack,
		GmDump,
		GmHeap,
		GmGlobals,
		GmStats)

type GmOutput = [Char]

getOutput :: GmState -> GmOutput
getOutput (o, i, stack, dump, heap, globals, stats) = o

putOutput :: GmOutput -> GmState -> GmState
putOutput o' (o, i, stack, dump, heap, globals, stats) =
	(o', i, stack, dump, heap, globals, stats)

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (o, i, stack, dump, heap, globals, stats)  = i 

putCode :: GmCode -> GmState -> GmState
putCode i' (o, i, stack, dump, heap, globals, stats) =
	(o, i', stack, dump, heap, globals, stats)

data Instruction 
	= Unwind
	| Slide Int
	| PushGlobal Name 
	| PushInt Int 
	| Push Int 
	| Mkap
	| Pop Int
	| Alloc Int 
	| Update Int
	| Eval
	| Add | Sub | Mul | Div | Neg
	| Eq  | Neq | Lt  | Le | Gt | Ge
	| Cond GmCode GmCode
	| Pack Int Int 
	| Casejump [(Int, GmCode)]
	| Split Int 
	deriving(Eq, Show)

type GmDump = [GmDumpItem]

type GmDumpItem = (GmCode, GmStack)

getDump :: GmState -> GmDump 
getDump (o, i, stack, dump, heap, globals, stats) = dump 

putDump :: GmDump -> GmState -> GmState
putDump dump' (o, i, stack, dump, heap, globals, stats) =
	(o, i, stack, dump', heap, globals, stats)

dumpInitial = []

type GmStack = [Addr]

type Addr = Int

getStack :: GmState -> GmStack 
getStack (o, i, stack, dump, heap, globals, stats) = stack

putStack :: GmStack -> GmState -> GmState
putStack stack' (o, i, stack, dump, heap, globals, stats) =
	(o, i, stack', dump, heap, globals, stats)

type GmHeap = Heap Node

-- Heap object count, unused addrs, addr->obj mapping
type Heap a = (Int, [Int], [(Int, a)])

-- NNum value | NAp Addr Addr | NGlobal arity instructions
data Node 
	= NNum Int 
	| NAp Addr Addr  		
	| NGlobal Int GmCode 	-- NGlobal Arity Code
	| NInd Addr 			-- Indirection
	| NConstr Int [Addr]
	deriving(Eq, Show)

getHeap :: GmState -> GmHeap
getHeap (o, i, stack, dump, heap, globals, stats) = heap

putHeap :: GmHeap -> GmState -> GmState
putHeap heap' (o, i, stack, dump, heap, globals, stats) =
	(o, i, stack, dump, heap', globals, stats)


type GmGlobals = ASSOC Name Addr

getGlobals :: GmState -> GmGlobals 
getGlobals (o, i, stack, dump, heap, globals, stats) = globals


type GmStats = Int 

statInitial :: GmStats 
statInitial = 0

statIncSteps :: GmStats -> GmStats 
statIncSteps s = s + 1

statGetSteps :: GmStats -> Int
statGetSteps s = s

getStats :: GmState -> GmStats 
getStats (o, i, stack, dump, heap, globals, stats) = stats

putStats :: GmStats -> GmState -> GmState 
putStats stats' (o, i, stack, dump, heap, globals, stats) =
	(o, i, stack, dump, heap, globals, stats')

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
	([], initialCode, [], [], heap, globals, statInitial)
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
initialCode = [PushGlobal "main", Eval]

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
compileR e env = compileE e env ++ [Update d, Pop d, Unwind]
	where d = (length env)

compileE :: GmCompiler
compileE (ENum n) env 				 = [PushInt n]
compileE (EAp(EAp(EVar op)e1)e2)env = do
	case aElem op builtInDyadic of
		False -> compileC (EAp(EAp(EVar op)e1)e2)env 
		True -> compileE e1 env ++ compileE e1 env ++ [getDyadicInst op]
compileE (EAp(EVar "neg")e) env = compileE e env ++ [Neg]
compileE (ECase expr alts) env = 
	compileE expr env ++ [Casejump $ compileD alts env]
compileE (EConstr t a) env = [Pack t a]
compileE e env  = compileC e env

compileD :: [CoreAlt] -> GmEnvironment -> [(Int, GmCode)]
compileD alts env = compileAlts compileE' alts env

compileAlts :: (Int -> GmCompiler) 
			-> [CoreAlt]
			-> GmEnvironment
			-> [(Int, GmCode)]
compileAlts comp alts env =
	[(tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env))
		| (tag,names,body) <- alts]

compileE' :: Int -> GmCompiler
compileE' offset expr env =
	[Split offset] ++ compileE expr env ++ [Slide offset]

builtInDyadic :: [(Name, Instruction)]
builtInDyadic =
	[("+", Add)]

getDyadicInst :: Name -> Instruction
getDyadicInst op = aLookup builtInDyadic op $ error "no such built in"

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
compileC (EConstr t a) env 			= [Pack t a]									 

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env =
	zip (map fst defs) [n-1, n-2..0] ++ argOffset n env
	where
		n = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(v, n+m) | (v,m) <- env] 

---------------------------------------------------------------------------------
-- Compiler Primitives.
---------------------------------------------------------------------------------

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives = 
	[("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
	 ("neg", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
	 --("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
	 --("!=", 2, [Push 1, Eval, Push 1, Eval, Neq, Update 2, Pop 2, Unwind]),
	 --("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
	 	]

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
hInitial = (0, [1..30], [])

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

aElem item assoc = elem item (map fst assoc)

aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key, val) <- alist]							