
/*****************************************************************************
 *	Some Important Notes
*****************************************************************************/

/*	- Naming Conventions
 *	In general, functions are camelCase lower-case first letter.
 *	DataType constructors are Capitalized.
 *
 *	Paramaters to functions start with an upper-case letter, to
 *	easier distinguish from internally created vars. This is important
 *	since there are so many repititions of similar words.
 *
 *	In general, a var begins its life as name and is superseded
 * 	by newName.
 *	
 *	- Data Types
 *	Data types are represented as functions of their constructors
 *	taking the constructor args as func args. More explanation 
 *	further down.
 *
 *	- Haskell -> JS gotchas
 *	There's no list comprehensions, so i've implemented a head/tail
 *	but they are pretty dodgy. NOTE: Fixed
 *
 *
 *
 *	- Death to javascript.
 *	Just yeah.	
 *
 */


/*****************************************************************************
 *	Some General Utility Functions
*****************************************************************************/


function head(list){
	x = list[0];
	return x;
}

function tail(list){
     h = list.slice(1, (list.length));
     return h;
}

Array.prototype.drop = function(from, to) {
  var rest = this.slice((to || from) + 1 || this.length);
  this.length = from < 0 ? this.length + from : from;
  return this.push.apply(this, rest);
};

/*****************************************************************************
 *	Representing GmState
*****************************************************************************/

/*
 *	Right, so this is how we're representing Data Types.
 *	node = new NAp(a1, a2) to instantiate. 
 *	node instanceof NAp to check our node is an NAp
 *	Just going to have to live with node.a1 to get at
 *	our values. surewhatcanyoudo.
 *	
 */

// data Node 
function NAp(a1, a2){
	this.a1 = a1;
	this.a2 = a2;
}

function NNum(n){
	this.n = n;
}

function NGlobal(numargs, instructions){
	//ugh. javascript stole my arity keyword.
	//probably doesn't even do anything cool with it.
	this.numargs = numargs;
	this.instructions = instructions
}

// data Instruction
function UnWind(){

}

function PushGlobal(Name){
	this.Name = Name;
}

function PushInt(Int){
	this.Int = Int;
}

function Push(Int){
	this.Int = Int;
}

function Mkap(){

}

function Slide(Int){
	this.Int = Int;
}

/* The rest of these defs are probably useless */

var Stats = 0;

/* Stack :: [Addr] */
var GmStack = [];

/* Code :: [Instruction] */
var GmCode = [];

/* Heap :: (Int, [Int], [Int -> Node]) */
var GmHeap = {
	objCount = 0,
	Addrs = [],
	AddrObjMap = {}
};

/* GmGlobals = [(Name, Addr)] */
var GmGlobals = {};

var GmState = [GmCode, GmStack, GmHeap, GmGlobals]

/*****************************************************************************
 *	Some Compiler Utility Functions
*****************************************************************************/

/*
 *	Think it's safe to say these funcs are all ok. They mostly
 *	correspond to the util functions in GCompiler.hs. You might
 *	find further enlightenment there.
 */

/* hAlloc :: GmHeap -> Node -> GmHeap */
function hAlloc(GmHeap, Node){
	size 	 		= GmHeap[0];
	addrs 	 		= GmHeap[1];
	addrObjs 		= GmHeap[2];
	next 	 		= head(addrs);
	newAddrs 		= tail(addrs);
	addrObjs[next] 	= Node;
	newHeap 		= [(size-1), newAddrs, addrObjs];
	return [newHeap, next];
}

function hLookup(GmHeap, Addr){
	return GmHeap.AddrObjMap[Addr];
}

function getCode(GmState){
	return GmState[0];
}

function putCode(GmCode, GmState){
	return [GmCode, GmState[1], GmState[2], GmState[3]];
}

function getHeap(GmState){
	return GmState[3];
}

function putHeap(GmHeap, GmState)
	return [GmState[0], GmState[1], GmHeap, GmState[3]];
}

function getStack(GmState){
	return GmState[1];
}

function putStack(GmStack, GmState){
	return [GmState[0], GmStack, GmState[2], GmState[3]];
}

function getGlobals(GmState){
	return GmState[3];
}

function putGlobals(GmGlobals, GmState){
	return [GmState[0], GmState[1], GmState[2], GmGlobals]];
}

/*****************************************************************************
 *	Our G-code instructions
*****************************************************************************/

/*
 *	Will all require extensive testing. Really gotta be careful
 *	wherever we're using a handrolled data type representation.
 */


/*
 *	Reminder
 *  Stack is just an array of Ints, 
 *  Heap consists of (Free, Addrs, [Addr->Name])
 */

/* pushglobal :: Name -> GmState -> GmState */
function pushglobal(Name, State){
	stack 	 = getStack(State);
	heap 	 = getHeap(State);
	nameAddr = hLookup(Name, heap);
	newStack = nameAddr.concat(stack);
	return putStack(newStack, State);
}

/* Pushint :: Int -> GmState -> GmState */
function pushint(Int, State){
	heap  			= getHeap(State);
	//where node is instanceof func NNum(){}
	node 			= new NNum(Int);
	(newHeap, addr) = hAlloc(heap, node);
	newStack 		= addr.concat(stack)
	newState		= putStack ( putHeap(newState) );
	return newState;
}

/*
-- Construct a new NAp Addr Addr
-- Addr1 and Addr2 are popped directly from the 
-- front of the stack. 
-- New node (NAp) will have addr Anew
-- New heap = hAlloc space for our new NAp node
-- Replace front two addrs in stack with addr of new node
-- Return state.
*/
/* Mkap :: GmState -> GmState */
function mkap(oldState){
	stack 				= getStack(oldState);
	a1 					= head(stack);
	a2 					= head(stack);
	stackRest 			= stack;
	node 				= new NAp(a1, a2);
	(newHeap, nodeAddr) = hAlloc(node);
	newStack 			= nodeAddr.concat(stackRest);
	return putState(newStack, oldState);
}

// Push-specific utility func
function getArg(NAp){
	return NAp.a2;
}
/* see GEval.hs if(when) you forget how this works*/
/* Push :: Int -> GmState -> GmState */
function push(N, State){
	stack 		= getStack(State);
	nodeAddr 	= stack[1 + N];
	arg 		= getArg(hLookup(getHeap(State), nodeAddr));
	return putStack(node.concat(stack), oldState));
}

/* Literally just drops N, moves bottom/front Node
 * to replace. If old stack = [2,1,0], new stack
 * after a slide 1 will be [2, 0]. Node that heap
 * isn't actually changed by this. Array.drop()
 * defined above. */
/* slide :: Int -> GmState -> GmState */
function slide(N, State){
	// be careful with implementation of head/tail
	a 			= head(getStack(State));
	as 			= tail(getStack(State));
	newStack 	= a.concat( ( as.drop(N) ) );
	return putStack(newStack, State);
}

/*
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
*/

/* Unwind :: GmState -> GmState */
function unwind(State){
	// be careful with implementation of head/tail	
	stack 	= getStack(State);
	heap 	= getHeap(State);
	a 		= head(stack);
	as 		= head(stack);
	node 	= hLookup(heap, a);
	if(node instanceof NNum){
		return State;
	} 
	if(node instanceof NAp){
		//Check these next 4 lines carefully
		leftArg 	= NAp.a1;
		newStack 	= leftArg.concat(a.concat(as));
		var unwind 	= new Unwind();
		return putCode([unwind], (putStack(newStack, State)));
	}
	if(node instanceof NGlobal){
		numargs 	= NGlobal.numargs;
		code 		= NGlobal.instructions;
		if(as.length < numargs){
			console.error("unwinding undersaturated");
		} else {
			putCode(code, state);
		}
	} else {
		console.error("unwind failing. check line 281");
	}
}

/*****************************************************************************
 *	Evaluator
*****************************************************************************/

/* gmFinal :: GmState -> Bool */
function gmFinal(State){
	code = getCode(State);
	if(code.length == 0){
		return true;
	} else {
		return false;
	}
}

/* eval :: GmState -> [GmState] */
function eval(State){
	var restStates 	= new Object;
	if(gmFinal(State)){
		restStates 	= [];
	} else {
		nextState 	= step(State);
		restStates 	= eval(nextState);
	}
	return State.concat(restStates);
}

/* step :: GmState -> GmState */
function step(State){
	// again, check these heads work
	code 		= getCode(State);
	i 			= head(code);
	is 			= head(code);
	newState 	= putCode(is, State);

	// these could be really cool higher order things
	// ala GCompiler.hs but ehhhhhhhh
	if(i instanceof PushGlobal){
		f = i.Name;
		return pushglobal(f, newState);
	}
	if(i instanceof PushInt){
		n = i.Int;
		return pushint(n, newState);
	}
	if(i instanceof Mkap){
		return mkap(newState);
	}
	if(i instanceof Push){
		n = i.Int;
		return push(n, newState);
	}
	if(i instanceof Slide){
		n = i.Int;
		return slide(n, State);
	}
	if(i instanceof Unwind){
		return unwind(State);
	}
}

/*****************************************************************************
 *	Output Dump...
*****************************************************************************/

