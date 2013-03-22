
function test(){
	alert("here i am");
}

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


Array.prototype.drop = function(N) {
  var from = 0;
  var to = N-1;
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
function Unwind(){

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

//var Stats = 0;

/* Stack :: [Addr] */
//var GmStack = [];

/* Code :: [Instruction] */
//var GmCode = [];

/* Heap :: (Int, [Int], [Int -> Node]) */
//var GmHeap = {
//	objCount = 0,
//	freeAddrs = [],
//	addrObjMap = {}
//};

/* GmGlobals = [(Name, Addr)] */
//var GmGlobals = {};

//var GmState = [GmCode, GmStack, GmHeap, GmGlobals]

/*****************************************************************************
 *	Test Environment
*****************************************************************************/

/*
 *	Until i get the serialization done, i'll be using a handrolled
 *	test state representing post-compile pre-eval idTest (GPrelude.hs)
 *	$ ghci run.hs; compile idTest
 */

var idCode = new NGlobal(
	1, 
	[new Push(0), new Slide(2), new Unwind()]
);

var mainCode = new NGlobal(
	0, 
	[new PushInt(1), new PushGlobal("Id"), 
	 new Mkap(), 	 new Unwind()]
);

var GmCode = [new PushGlobal("main"), new Unwind()];

var GmStack = [];

var GmHeap = {
	objCount:2, 
	freeAddrs:[3,4,5,6,7,8,9,10,11,12,13,14,15], 
	addrObjMap:{
		2 : idCode,
		1 : mainCode
	}
};

var GmGlobals = {
	"main": 1,
	"Id":2
};

var GmState = [GmCode, GmStack, GmHeap, GmGlobals]; 


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
	size 	 		  = GmHeap.objCount;
	freeAddrs 	 	  = GmHeap.freeAddrs;
	addrObjMapx 	  = GmHeap.addrObjMap;
	next 	 		  = head(freeAddrs);
	newAddrs 		  = tail(freeAddrs);
	addrObjMapx[next] = Node;
	newHeap 		  = {objCount	: (size-1), 
						freeAddrs	: newAddrs,
						addrObjMap	: addrObjMapx};
	return [newHeap, next];
}

function hLookup(GmHeap, Addr){
	node = GmHeap.addrObjMap[Addr];
	if(node == undefined){
		console.log("undefined hLookup")
	}
	return node;
}

/*	Add error for non-existant name here */
function aLookup(GmGlobals, Name){
	global = GmGlobals[Name];
	if(global == undefined){
		console.log("undefined aLookup");
	}
	return global;
}

function getCode(GmState){
	return GmState[0];
}

function putCode(GmCode, GmState){
	return [GmCode, GmState[1], GmState[2], GmState[3]];
}

function getHeap(GmState){
	return GmState[2];
}

function putHeap(GmHeap, GmState){
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
	return [GmState[0], GmState[1], GmState[2], GmGlobals];
}

// All these tested 12:45 22/03

/*****************************************************************************
 *	Our G-code instructions
*****************************************************************************/

/*
 *	Will all require extensive testing. Really gotta be careful
 *	wherever we're using a handrolled data type representations.
 */


/*
 *	Reminder
 *  Stack is just an array of Ints, 
 *  Heap consists of (Free, Addrs, [Addr->Name])
 */


/* pushglobal :: Name -> GmState -> GmState */
function pushglobal(Name, State){
	console.log("pushglobal called");	
	stack 	 	= getStack(State);
	globals 	= getGlobals(State);
	addr 		= aLookup(globals, Name);
	//newStack 	= addr.concat(stack);
	stack.push(addr);
	newStack 	= stack;
	return putStack(newStack, State);
}	// Looks good 12:45 22/03

/* Pushint :: Int -> GmState -> GmState */
function pushint(Int, State){
	console.log("pushint called");
	heap  			= getHeap(State);
	stack 			= getStack(State);
	//where node is instanceof func NNum(){}
	node 			= new NNum(Int);
	[newHeap, addr] = hAlloc(heap, node);
	stack.push(addr);
	newStack 		= stack;
	newState		= putStack(newStack, State);
	newNewState		= putHeap(newHeap, newState);
	return newState;
}	// Looking alright 12:49 22/03

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
	console.log("mkap called");
	stack 				= getStack(oldState);
	//replacing heads with pops because a:b:c
	a1 					= stack.pop();
	console.log("mkap a1: " + a1);
	a2 					= stack.pop();
	console.log("mkap a2: " + a2);
	stackRest 			= stack;
	node 				= new NAp(a1, a2);
	[newHeap, nodeAddr] = hAlloc(node);
	//newStack 			= nodeAddr.concat(stackRest);
	stack.push(nodeAddr);
	newStack			= stack;
	return putState(newStack, oldState);
}	// going to need two nodes to test this

// Push-specific utility func
function getArg(NAp){
	a2 = NAp.a2;
	if(a2 == undefined){
		console.log("undefined getArg");
	}
	return a2;
}
/* see GEval.hs if(when) you forget how this works*/
/* Push :: Int -> GmState -> GmState */
function push(N, State){
	console.log("push called");
	stack 		= getStack(State);
	heap 		= getHeap(State)
	nodeAddr 	= stack[1 + N];
	arg 		= getArg(hLookup(heap, nodeAddr));
	//newStack 	= node.concat(stack)
	stack.push(arg);
	newStack 	= stack;
	return putStack(newStack, State);
}	// again, heap specific.

/* Literally just drops N, moves bottom/front Node
 * to replace. If old stack = [2,1,0], new stack
 * after a slide 1 will be [2, 0]. Node that heap
 * isn't actually changed by this. Array.drop()
 * defined above. */
/* slide :: Int -> GmState -> GmState */
function slide(N, State){
	console.log("slide called");
	// be careful with implementation of head/tail
	stack 		= getStack(State);
	a 			= head(stack);
	as 			= tail(stack);
	as.drop(N);
	as.push(a);
	newStack 	= as;
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
	console.log("unwind called");
	// be careful with implementation of head/tail	
	stack 	= getStack(State);
	heap 	= getHeap(State);
	a 		= head(stack);
	as 		= tail(stack);
	aslen	= as.length;
	node 	= hLookup(heap, a);
	if(node instanceof NNum){
		return State;
	} 
	if(node instanceof NAp){
		//Check these next 4 lines carefully
		a1 			= node.a1;
		as.push(a);
		as.push(a1);
		newStack 	= as;
		var unwind 	= new Unwind();
		return putCode([unwind], (putStack(newStack, State)));
	}
	if(node instanceof NGlobal){
		console.log("unwind NGlobal called");
		numargs 	= node.numargs;
		code 		= node.instructions;
		console.log("numargs: " + numargs + " | code: " + code);
		if(aslen < numargs){
			console.error("unwinding undersaturated");
		} else {
			return(putCode(code, State));
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
	console.log("gmFinal called");
	code = getCode(State);
	if(code.length == 0){
		return true;
	} else {
		return false;
	}
} //	working fine 13:37 22/03

/* eval :: GmState -> [GmState] */
/*
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
*/

/* step :: GmState -> GmState */
function step(State){
	// again, check these heads work
	code 		= getCode(State);
	i 			= head(code);
	is 			= tail(code);
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

