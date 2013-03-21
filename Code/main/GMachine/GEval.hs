module GEval where
import GCompiler
import GADT

---------------------------------------------------------------------------------
-- Evaluation.
---------------------------------------------------------------------------------

eval :: GmState -> [GmState] 
eval state = state : restStates 
			where
				restStates 	| gmFinal state = []
							| otherwise		= eval nextState 
				nextState 	= doAdmin (step state)


-- Boring. Just stats.
doAdmin :: GmState -> GmState 
doAdmin s = putStats (statIncSteps (getStats s)) s

-- Test if we've any more graph to eval
gmFinal :: GmState -> Bool 
gmFinal s = 
	case (getCode s) of 
		[] 			-> True 
		otherwise 	-> False

-- Step through each stored instruction representing
-- Our progress in eval'ing the graph
step :: GmState -> GmState 
step state = dispatch i (putCode is state)
		where (i:is) = getCode state

-- Handle instructions. Intepret instruction symbols
-- and return actions effecting the state 
dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f)	= pushglobal f
dispatch (PushInt n)	= pushint n 
dispatch  Mkap 			= mkap 
dispatch (Push n)		= push n
dispatch (Slide n) 		= slide n 
dispatch  Unwind 		= unwind

-- Pushes the 
pushglobal :: Name -> GmState -> GmState 
pushglobal f state = 
	putStack (a : getStack state) state
	where 
		a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))

-- Put int into heap and add its addr to stack.
pushint :: Int -> GmState -> GmState 
pushint n state =
	putHeap heap' (putStack (addr: getStack state) state)
	where (heap', addr) = hAlloc (getHeap state) (NNum n)

-- Take top two addrs from stack, construct NAp, alloc
-- space for it on heap and add with new pointer on stack
mkap :: GmState -> GmState 
mkap state =
	putHeap heap' (putStack (a:as') state)
	where 
		(heap', a) 	= hAlloc (getHeap state) (NAp a1 a2)
		(a1:a2:as')	= (getStack state)

-- Traverse back up tree by 'Int' after SC and add pointer 
-- to argument found (on the right...?) to the stack
push :: Int -> GmState -> GmState 
push n state =
	putStack (a:as) state 
	where 
		as = getStack state 
		a  = getArg (hLookup (getHeap state) (as !! (n+1)))

-- Get's right hand ('arg') of NAp node
getArg :: Node -> Addr 
getArg (NAp a1 a2) = a2

-- Tidies stack post SC instantiating. Drops x addrs
-- from stack and moves everything up (figuratively)
slide :: Int -> GmState -> GmState 
slide n state =
	putStack (a: drop n as) state 
	where (a:as) = getStack state

-- This will require explanation.
-- Firstly, if there is a Num on top of the graph, then
-- we are done. Replace the code with [] to signify this
-- (which will be acknowledge by gmFinal.)
-- If there is an NAp, we need to continue unwinding.
-- In the case of an SC, we need to checkout if we have
-- enough arguments to fully saturate the SC and if so
-- put the node's code into GmState 
unwind :: GmState -> GmState 
unwind state =
	newState (hLookup heap a)
	where
		(a:as) = getStack state
		heap  = getHeap state
		newState (NNum n) = state
		newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)
		newState (NGlobal n c)
			| (length as) < n 		= error "unwinding undersaturated"
			| otherwise 			= putCode c state
