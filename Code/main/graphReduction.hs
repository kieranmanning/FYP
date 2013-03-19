module GraphRed where

---------------------------------------------------------------------------------
-- Definition of our (current core-like) language.
---------------------------------------------------------------------------------

{-
I'm using the 'Core-like' language from the SJP implementorial
book. Further reading should start there for anyone interested.

Why this isn't 'Core' core:

The current version of core is doing my head in. It also contains
unnecessary information. I may well update this to represent actual
core but i'll probably end up parsing GHC current core into this
version as all I need is already expressed here.

The only significant difference really is in the type-signatures,
type annotations and the non-unary infix applicator (?!!).
-}

type Name = String
type IsRec = Bool

bindersOf :: [(a,b)] -> [a]
bindersOf defns = map fst defns

rhssOf :: [(a,b)] -> [b]
rhssOf defns = map snd defns

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a 	-> Bool
isAtomicExpr (EVar v) 	= True
isAtomicExpr (ENum n) 	= True
isAtomicExpr e 			= False

type Program a = [ScDefn a]
type CoreProgram = Program Name


---Should definitely talk about [Super]Combinators
---in background section of report. Link to haskell
---wiki for further info

{- Super Combinator definition -}
{-	(Name, Args, Body) -}
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

{- Defintion of a core Expr -}
type CoreExpr = Expr Name

data Expr a 
	= EVar Name
	| ENum Int
	| EConstr Int Int
	| EAp (Expr a) (Expr a)
	| ELet 
		IsRec
		[(a, Expr a)]
		(Expr a)
	| ECase
		(Expr a)
		[Alter a]
	| ELam [a] (Expr a)
	deriving(Show)

---------------------------------------------------------------------------------
-- Prelude and such.
---------------------------------------------------------------------------------

preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x")]	-- Identity, here we come...


---------------------------------------------------------------------------------
-- Compilation.
---------------------------------------------------------------------------------

{-
Notes

The 'Ti-' prefix denotes a relation to template instantiation,
the poor graph-reduction cousin to the G-Machine. Time permitting
I might attempt to make this into a proper G-Machine.

The Significant type in all of is this TiState, representing
the stack, dump, heap, global vars and stats related to compilation.
The individual components are fairly self explanatory from looking
at their types. 
-}

{-
data AExpr 
	= Num Int 
	| Plus AExpr AExpr 
	| Mult AExpr AExpr

aExprEval :: AExpr -> Int 
aExprEval (Num n) = n
aExprEval (Plus e1 e2) = (aExprEval e1) + (aExprEval e2)
aExprEval (Mult e1 e2) = (aExprEval e1) * (aExprEval e2)

type MultState = (Int, Int, Int, Int)
-}

--runProg :: [Char] -> [Char]

--parse :: [Char] -> CoreProgram

--compile :: CoreProgram -> TiState

--eval :: TiState -> [TiState]

--showResults :: [TiState] -> [Char]

--runProg = showResults . eval . compile . parse 

--Compiler

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

data TiDump = DummyTiDump

initialTiDump = DummyTiDump

type TiHeap = Heap Node 

data Node = NAp Addr Addr 					-- Application
	      | NSuperComb Name [Name] CoreExpr -- sc
	      | NNum Int 						-- Ints		 

type TiGlobals = ASSOC Name Addr 

type TiStats = Int 
tiStatInitial :: TiStats
tiStatInitial= 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s+1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = 
	(stack, dump, heap, sc_defs, stats_fun stats)

-- Heap object count, unused addrs, addr->obj mapping
type Heap a = (Int, [Int], [(Int, a)])
type Addr = Int

compile program = 
	(initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
	where
		sc_defs = program ++ preludeDefs ++ extraPreludeDefs

		(initial_heap, globals) = buildIinitialHeap sc_defs

		extraPreludeDefs = [] 

		initial_stack = [address_of_main]
		address_of_main = aLookup globals "main" (error "main undefined")

buildIinitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals) 
buildIinitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs 

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) =
	(heap', (name, addr))
	where
		(heap', addr) = hAlloc heap (NSuperComb name args body)

---------------------------------------------------------------------------------
-- Evaluation.
---------------------------------------------------------------------------------


eval state = state : rest_states
			 where
			 	rest_states | tiFinal state = []
			 				| otherwise = eval next_state
			 	next_state 	= doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool 
tiFinal ([sole_addr], dump, heap, globals, stats) =
	isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "empty stack"
tiFinal state = False	-- because non-empty

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

step :: TiState -> TiState
step state =
	dispatch (hLookup heap (head stack))
	where
		(stack, dump, heap, globals, stats) = state 
		dispatch(NNum n) = numStep state n 
		dispatch(NAp a1 a2) = apStep state a1 a2 
		dispatch(NSuperComb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState 
numStep state n = error "num on top of stack"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 =
	(a1 : stack, dump, heap, globals, stats)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState 
scStep (stack, dump, heap, globals, stats) sc_name arg_names body =
	(stack', dump, heap', globals, stats)
	where
		stack' = result_addr : (drop (length arg_names+1) stack) 
		(heap', result_addr) = instantiate body heap env
		env = arg_bindings ++ globals 
		arg_bindings = zip arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr] 
getargs heap (sc:stack) = 
	map get_arg stack 
	where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr 				-- SC in particular
			-> TiHeap 					-- pre heap
			-> ASSOC Name Addr 			-- name->addr mapping
			-> (TiHeap, Addr)			-- post heap + instance root addr
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env =
	hAlloc heap2 (NAp a1 a2) 
	where
		(heap1, a1) = instantiate e1 heap env
		(heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env =
	(heap, aLookup env v (error ("undefined name " ++ show v)))


---------------------------------------------------------------------------------
-- Displaying results. JS Should take over here
---------------------------------------------------------------------------------

showResults states =
	iDisplay (iConcat [iLayn (map showState states),
						showStates (last states)
						])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) =
	iConcat [showStack heap stack, iNewLine]

showStack :: TiHeap -> TiStack -> Iseq 

---------------------------------------------------------------------------------
-- Utils.
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
hInitial = (0, [1..], [])

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