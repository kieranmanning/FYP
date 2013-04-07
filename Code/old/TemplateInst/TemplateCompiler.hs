module Compiler where
import Data.List

---------------------------------------------------------------------------------
-- Contents.
---------------------------------------------------------------------------------

{-	Sorry this needs a 'contents'. will split tomorrow.
 -
 -	1. Core(-Like) ADT Defintion
 -  2. Prelude funcs and tests (*proof of concept)
 -  3. Graph Reduction
 -		3.1 Compiler
 -		3.2 Evaluation
 -		3.3 Javascript
 -	4. Displaying/Pretty printing. Superflous now.
 -	5. Utils
 -  
 -}



---------------------------------------------------------------------------------
-- Definition of our (current core-like) language.
---------------------------------------------------------------------------------

{-

-*NB*-

For talks on wednesday:
	- Decide on scope
	- Ask about compiling to a lower object code;
	  Might be an easier platform for getting to JS.
	- G-machine to be implemented via slave labour.
	- Disregard the display crap. It could probably
	  be rewritten with with as little as a handful
	  of show statements.
	- Happy parser for core.
	- Front Facing dsl.
	- Not currently performing graph updates, just
	  dumping additionally onto the stack
	- Only significant difference to GHC current core
	  is that a core program here is represented as a
	  list of ScDefns whereas GHCCCore has an additional
	  bind data type external to the Expr and is represented
	  as a series of top-level bindings.

Some of this is annotated on paper. Should be saved for purposes
of adding to report.

I'm using the 'Core-like' language from the SJP implementorial
book. Further reading should start there for anyone interested.

Why this isn't 'Core' core:

The current version of core is doing my head in. It also contains
unnecessary information. I may well update this to represent actual
core but i'll probably end up parsing GHC current core into this
version as all I need is already expressed here.

The only significant difference really is in the type-signatures,
type annotations and the non-unary infix applicator (?!!).

G-Machine will be required for case alternatives and probably
for any kind of ADT representations.
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
-- Prelude and tests and such.
---------------------------------------------------------------------------------

test = eval ( compile ( idTest ) )

preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x"),	-- Identity, here we come...
	 ("Id2", ["y"], EVar "y")]

idTest :: CoreProgram
idTest = 
	[("main", [], (
		(EAp (EVar "Id") 
				(EAp (EVar "Id2") (ENum 21)))
		))
	]


--underSaturatedTest :: CoreProgram
--underSaturatedTest =
--	[("main", [], (EAp (EVar "Id") ()))]

---------------------------------------------------------------------------------
-- Graph Reduction.
---------------------------------------------------------------------------------

{-
This is our graph reduction machine. In terms of data types, it
consists of a stack, dump, heap and a mapping of global names.
The stack stores the addresses of our graph nodes, which are
stored in the heap. The dump is not currently used, probably will
be beyond the identity function. The heap consists of tagged
nodes addressed by the stack. Globals which are represented using
a specified 'ASSOC' name->addr mapping store the addresses of
heap nodes representing supercombinators.

Heap nodes can currently be applicators, supercombinators or
numbers. 

The 'Ti-' prefix denotes a relation to template instantiation,
the poor graph-reduction cousin to the G-Machine. Time permitting
I might attempt to make this into a proper G-Machine.

The Significant type in all of this is TiState, representing
the stack, dump, heap, global vars and stats related to compilation.
The individual components are fairly self explanatory from looking
at their types. 
-}



-- runProg :: [Char] -> [Char]

--parse :: [Char] -> CoreProgram

compile :: CoreProgram -> TiState

eval :: TiState -> [TiState]

-- showResults :: [TiState] -> [Char]

-- parsing is now our concern right now 
-- runProg = showResults . eval . compile . parse 

runProg = showResults . eval . compile 



---------------------------------------------------------------------------------
-- Compiler.
---------------------------------------------------------------------------------


type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiStack = [Addr]

type Addr = Int

data TiDump = DummyTiDump
	deriving(Show)

initialTiDump = DummyTiDump

type TiHeap = Heap Node 

-- Heap object count, unused addrs, addr->obj mapping
type Heap a = (Int, [Int], [(Int, a)])

data Node = NAp Addr Addr 					-- Application
	      | NSuperComb Name [Name] CoreExpr -- sc
	      | NNum Int 						-- Ints		 
	deriving(Show)

type TiGlobals = ASSOC Name Addr 

--Used to measure number of steps taken by compiler
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

-- **** Expand here later, unless switching to G-machine
step :: TiState -> TiState
step state =
	dispatch (hLookup heap (head stack))
	where
		(stack, dump, heap, globals, stats) = state 
		dispatch(NNum n) = numStep state n 
		dispatch(NAp a1 a2) = apStep state a1 a2 
		dispatch(NSuperComb sc args body) = scStep state sc args body

-- Can't step through a literal int. what would do?!
numStep :: TiState -> Int -> TiState 
numStep state n = error "num on top of stack"

-- applicator evaluation step. add to stack
apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 =
	(a1 : stack, dump, heap, globals, stats)

-- Instantiate body of SuperCom and bind argument names
-- to the argument address in the stack, which we can
-- then discard (it seems).
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState 
scStep (stack, dump, heap, globals, stats) sc_name arg_names body =
	(stack', dump, heap', globals, stats)
	where
		stack' = result_addr : (drop (length arg_names+1) stack) 
		(heap', result_addr) = instantiate body heap env
		env = arg_bindings ++ globals 
		arg_bindings = zip arg_names (getargs heap stack)


-- presumed to have a SuperCom on top of stack. returns list
-- of arguments from application nodes.
getargs :: TiHeap -> TiStack -> [Addr] 
getargs heap (sc:stack) = 
	map get_arg stack 
	where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

-- Takes a supercombinator body, heap and env and 
-- 
instantiate :: CoreExpr 				-- sc body
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
-- Javascript. 
---------------------------------------------------------------------------------

{-	The general approach is to let the compiler reduce
 -	our functions as much as possible, then grab them
 -	from the heap via their tags and convert them to
 -	the most minimal JS possible. We're only on identity
 -	functions at the moment, so it'll take some more 
 -	work to deal with the Applications more complicated
 -	supercombinators (not to mention the whole "Rest 
 -	of the language") but we'll get to that
 -}

--runCore2JS :: CoreProgram -> String
runCore2JS prog = let x = eval(compile(prog)) in
	intercalate "\n" (map node2JS (interpret2JS(x)))

interpret2JS :: [TiState] -> [Node]
interpret2JS graphStates = deconState (last graphStates)

deconState :: TiState -> [Node] 
deconState (stack, dump, heap, globals, stats) =
	getNodesFromHeap heap

getNodesFromHeap :: TiHeap -> [Node]
getNodesFromHeap heap = 
	map (hLookup heap) (hAddresses heap)

node2JS :: Node -> String
node2JS (NNum x)	 		= " "
node2JS (NSuperComb s b e) 	= superComb2JS (NSuperComb s b e)
node2JS (NAp a1 a2)			= " "

superComb2JS :: Node -> String
superComb2JS (NSuperComb sId b e) =
	"function " 			++ 
	sId 					++ 
	scBinders2JSParams b 	++
	"{ \n" ++ expr2JS e  ++ " } \n"

scBinders2JSParams :: [Name] -> String
scBinders2JSParams b = "( " ++ (intercalate ", " b) ++ " )"

expr2JS :: Expr a -> String
expr2JS expr = do 
	case expr of 
		EVar name 	-> "\t return " ++ name ++ "; \n "
		ENum int 	-> "\t return " ++ (show int) ++ "; \n"
		EAp e1 e2 	-> "\t not tonight... \n"



{-

*Ignore this. Copy Pasted so i wouldn't wear out my mousewheel*

	= EVar Name
	| ENum Int
	| EConstr Int Int
	| EAp (Expr a) (Expr a)


node2JS :: Node -> String
node2JS node 	| NNum x = show x
				| NSuperComb s b e = (show s) ++ (show b) ++ (show e)
				| NApp a1 a2 = 

hAddresses :: Heap a -> [Addr] - returns all addresses

hLookup :: Heap -> Addr -> Node


type TiHeap = Heap Node 
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
-- Heap object count, unused addrs, addr->obj mapping
type Heap a = (Int, [Int], [(Int, a)])

data Node = NAp Addr Addr 					-- Application
	      | NSuperComb Name [Name] CoreExpr -- sc
	      | NNum Int 						-- Ints		 
	deriving(Show)
-}


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

showResults :: [TiState] -> [Char]
showResults states =
	iDisplay (iConcat [iLayn (map showState states),
						showState (last states)
						])

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) =
	iConcat [showStack heap stack, iNewLine]

showStack :: TiHeap -> TiStack -> Iseq 
showStack heap stack = 
	iConcat [
		iStr "Stack [",
		iIndent (iInterleave iNewLine (map show_stack_item stack)),
		iStr " ]"
	]
	where
		show_stack_item addr =
			iConcat [showFWAddr addr, iStr ": ", 
					 showStkNode heap (hLookup heap addr)]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) =
	iConcat [ iStr "NAp ", showFWAddr fun_addr,
			  iStr " ", showFWAddr arg_addr, iStr " (",
			  showNode (hLookup heap arg_addr), iStr ")"
	]
showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [iStr "NAp ", showAddr a1, 
								iStr " ", showAddr a2
								]
showNode (NSuperComb name args body) = iStr ("NSuperComb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (spaces (4 - length str) ++ str)
	where
		str = show addr

showStats :: TiState -> Iseq 
showStats (stack, dump, heap, globals, stats) =
	iConcat [ iNewLine, iNewLine, iStr "Total num steps = ",
			  iNum (tiStatGetSteps stats)
	]


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