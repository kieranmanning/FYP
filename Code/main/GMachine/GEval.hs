module GEval where
import GCompiler
import GADT

---------------------------------------------------------------------------------
-- Evaluation.
---------------------------------------------------------------------------------


-- Pop state. if !finalState, step through and continue eval'ing
eval :: GmState -> [GmState] 
eval state = state : restStates 
			where
				restStates 	| gmFinal state = []
							| otherwise		= eval nextState 
				nextState 	= doAdmin (step state)


-- Boring. Just stats.
doAdmin :: GmState -> GmState 
doAdmin s = putStats (statIncSteps (getStats s)) s

-- Test if we've any more states to eval
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

stepx :: GmState -> Int -> GmState 
stepx state x | x == 0 = state 
			  | otherwise = stepx (step state) (x-1)

-- Handle instructions. Intepret instruction symbols
-- and return actions effecting the state 
dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f)	= pushglobal f
dispatch (PushInt n)	= pushint n 
dispatch  Mkap 			= mkap 
dispatch (Push n)		= push n
dispatch (Update n) 	= update n 
dispatch (Pop n) 		= pop n
dispatch  Unwind 		= unwind
dispatch (Cond a b) 	= cond a b
dispatch Eval 			= evalx
dispatch Sub 			= sub 
dispatch Eq 			= eq
dispatch Add 			= add
dispatch Neg 			= neg

-- Updates the stack with the location of the global
-- in question, assumed to be in the stack.
pushglobal :: Name -> GmState -> GmState 
pushglobal f state = 
	putStack (a : getStack state) state
	where 
		a = aLookup (getGlobals state) f (error ("Undeclared global " ++ f))

{-
-- Put int into heap and add its addr to stack.
pushint :: Int -> GmState -> GmState 
pushint n state =
	putHeap heap' (putStack (addr: getStack state) state)
	where (heap', addr) = hAlloc (getHeap state) (NNum n)
-}

pushint :: Int -> GmState -> GmState
pushint n state =
	case aLookup globals (show n) (-1) of
		(-1) -> putStack stack' (putHeap heap' (putGlobals globals' state))
				where
					(heap', addr) = hAlloc heap $ NNum n 
					stack' = addr : stack 
					globals' = ((show n), addr) : globals 
		addr -> putStack stack' state 
			    where
			    	stack' = addr : stack 
		where
			heap = getHeap state 
			stack = getStack state 
			globals = getGlobals state 

-- Construct a new NAp Addr Addr
-- Addr1 and Addr2 are popped directly from the 
-- front of the stack. 
-- New node (NAp) will have addr Anew
-- New heap = hAlloc space for our new NAp node
-- Replace front two addrs in stack with addr of new node
-- Return state 
mkap :: GmState -> GmState 
mkap state =
	putHeap heap' (putStack (a:as') state)
	where 
		(heap', a) 	= hAlloc (getHeap state) (NAp a1 a2)
		(a1:a2:as')	= (getStack state)

-- Traverse back up tree by 'Int' after SC and add pointer 
-- to argument found (on the right...?) to the stack
-- NOTE: Only called when (stack !! n + 1) is an NAp
-- NOTENOTE: It actually does *just* go up n + 1 (jump the
-- current node, which i think is always an SC) and grab
-- the right hand arg of that NAp
{-
push :: Int -> GmState -> GmState 
push n state =
	putStack (a:as) state 
	where 
		as = getStack state 
		a  = getArg (hLookup (getHeap state) (as !! (n+1)))
-}

push :: Int -> GmState -> GmState
push n state =
	putStack stack' state 
	where
		stack = getStack state 
		stack' = (stack !! n):stack

-- Get's right hand ('arg') of NAp node
getArg :: Node -> Addr 
getArg (NAp a1 a2) = a2


-- needs cleanup
update :: Int -> GmState -> GmState
update n state = newState
	where
		stack = getStack state 
		oldAddr = stack !! (n+1)
		(newHeap, newAddr) = hAlloc (getHeap(state)) (NInd 	(stack!!0))
		(a, as) = splitAt (n+1) stack 
		tempStack = a ++ (newAddr : (drop 1 as))
		newStack = drop 1 tempStack
		newState = (putStack newStack (putHeap newHeap(state)))

-- seems fine
pop :: Int -> GmState -> GmState
pop n state = putStack(drop n (getStack state)) state

evalx :: GmState -> GmState 
evalx state = putDump dump' newState 
	where 
		dump'   = (i, s) : (getDump state)
		(a:s)  = getStack state 
		i 	   = getCode state
		newState = putCode [Unwind] (putStack [a] state)

boxInteger :: Int -> GmState -> GmState 
boxInteger n state = 
	putStack (a:getStack state) (putHeap h' state)
	where
		(h', a) = hAlloc (getHeap state) (NNum n)

unboxInteger :: Addr -> GmState -> Int 
unboxInteger a state =
	ub (hLookup (getHeap state) a)
	where 
		ub (NNum i) = i
		ub _		= error "Unboxing non-integer"

boxBoolean :: Bool -> GmState -> GmState 
boxBoolean b state =
	putStack (a:getStack state) (putHeap h' state)
	where
		(h', a) = hAlloc (getHeap state) (NNum b')
		b' | b  = 1
		   | otherwise = 0

{-
unboxBoolean :: Addr -> GmState -> Bool
unboxBoolean a state = 
	ub (hLookup (getHeap state) a)
-}

-- represent monadic (ERRRR ARITY ONE...) operators
primitive1 :: (b -> GmState -> GmState)		-- boxing func
		   -> (Addr -> GmState -> a)		-- unboxing func
		   -> (a -> b)						-- operator
		   -> (GmState -> GmState)			-- state transition
primitive1 box unbox op state 
	= box (op (unbox a state)) (putStack as state)
	where
		(a:as)	= getStack state

-- generics dyadic operator handling
primitive2 :: (b -> GmState -> GmState)		-- boxing
		   -> (Addr -> GmState -> a)		-- unboxing
		   -> (a -> a -> b)					-- op
		   -> (GmState -> GmState)			-- state trans
primitive2 box unbox op state 
	= box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
	where
		(a0:a1:as) = getStack state

arithmetic1 :: (Int -> Int)	-> (GmState -> GmState)
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> (GmState -> GmState)
arithmetic2 = primitive2 boxInteger unboxInteger

comparison :: (Int -> Int -> Bool) -> (GmState -> GmState)
comparison = primitive2 boxBoolean unboxInteger

cond :: GmCode -> GmCode -> GmState -> GmState
cond i1 i2 state = putCode i' (putStack as state)
	where
		i = getCode state 
		(a:as) = getStack state 
		(NNum b) = hLookup (getHeap state) a 
		i' | b == 0 = (i2 ++ i)
		   | b == 1 = (i1 ++ i)
		   | otherwise = error $ "cond called on nonbool"

add :: GmState -> GmState
add = arithmetic2 (+)

sub :: GmState -> GmState
sub = arithmetic2 (-)

eq :: GmState -> GmState
eq = comparison (==)

neg :: GmState -> GmState
neg = arithmetic1 (negate)

{-
-- Tidies stack post SC instantiating. Drops x addrs0
-- from stack and moves everything up (figuratively)
slide :: Int -> GmState -> GmState 
slide n state =
	putStack (a: drop n as) state 
	where (a:as) = getStack state
-}

-- This will require explanation.
-- Firstly, if there is a Num on top of the graph, then
-- we are done. Replace the code with [] to signify this
-- (which will be acknowledge by gmFinal.) 
-- If there is an NAp, we need to continue unwinding.
-- In the case of an SC, we need to checkout if we have
-- enough arguments to fully saturate the SC and if so
-- put the node's code into GmState 
-- For an indirection, replace the stack top with the 
-- addr that the indirection points to.

-- type GmDump = [GmDumpItem]
-- type GmDumpItem = (GmCode, GmStack)

unwind :: GmState -> GmState 
unwind state =
	newState (hLookup heap a)
	where
		(a:as) = getStack state
		heap  = getHeap state
		newState (NNum n) 
		-- this is going to be a pain to javascripterize.
		-- currently works because ((c,s):ds) wont be called
		-- unless needed. IF ONLY SOMEONE WOULD COME UP WITH
		-- A WAY TO INTRODUCE LAZINESS TO JAVASCRIPT...
			| (getDump state) == [] = state
			| otherwise  = putDump ds (putCode (c) (putStack (a:s) state))
			where
				((c,s):ds) = getDump state
		newState (NInd a1) =  putCode [Unwind] (putStack (a1:as) state)
		newState (NAp a1 a2) = putCode [Unwind] (putStack (a1:a:as) state)
		newState (NGlobal n c)
			| (length (a:as)-1) < n = error "unwinding undersaturated"
			| otherwise 			= putCode c (putStack (rearrange n heap (a:as)) state)

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as =
	take n as' ++ drop n as 
	where
		as' = map (getArg . hLookup heap) (tail as)