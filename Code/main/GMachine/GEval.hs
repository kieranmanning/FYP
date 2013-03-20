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

doAdmin :: GmState -> GmState 
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GmState -> Bool 
gmFinal s = 
	case (getCode s) of 
		[] 			-> True 
		otherwise 	-> False

step :: GmState -> GmState 
step state = dispatch i (putCode is state)
		where (i:is) = getCode state

dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f)	= pushglobal f
dispatch (PushInt n)	= pushint n 
dispatch  Mkap 			= mkap 
dispatch (Push n)		= push n
dispatch (Slide n) 		= slide n 
dispatch  Unwind 		= unwind

pushglobal :: Name -> GmState -> GmState 
pushglobal f state = 
	putStack (a : getStack state) state
	where 
		a = aLookup (getGlobals state) f (error ("Undeclared global" ++ f))

pushint :: Int -> GmState -> GmState 
pushint n state =
	putHeap heap' (putStack (addr: getStack state) state)
	where (heap', addr) = hAlloc (getHeap state) (NNum n)

mkap :: GmState -> GmState 
mkap state =
	putHeap heap' (putStack (a:as') state)
	where 
		(heap', a) 	= hAlloc (getHeap state) (NAp a1 a2)
		(a1:a2:as')	= (getStack state)

push :: Int -> GmState -> GmState 
push n state =
	putStack (a:as) state 
	where 
		as = getStack state 
		a  = getArg (hLookup (getHeap state) (as !! (n+1)))

getArg :: Node -> Addr 
getArg (NAp a1 a2) = a2

slide :: Int -> GmState -> GmState 
slide n state =
	putStack (a: drop n as) state 
	where (a:as) = getStack state

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
