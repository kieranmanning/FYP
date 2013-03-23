function test(){

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
 *	- Death to javascript.
 *	Just yeah.	
 *
 *	- Name
 *	Needs a name. cataλyst too cheesy? not exactly pressing at any rate.
 *
 */


/*****************************************************************************
 *	Some General Utility Functions
*****************************************************************************/


function head(list){
	var x = list[0];
	return x;
}

function tail(list){
	var x = list;
     var h = x.slice(1, (list.length));
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

function NInd(a){
	this.a = a
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

function Update(A){
	this.a = A;

}

function Pop(Int){
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

// breaking on...
//	getCode(step(step(step(step(step(GmState))))))

var idCode = new NGlobal(
	1, 
	[new Push(0), new Update(1), new Pop(1), new Unwind()]
);

var mainCode = new NGlobal(
	0, 
	[new PushInt(1), new PushGlobal("Id"), 
	 new Mkap(), 	 new Update(0),
	 new Pop(0),	 new Unwind()]
);

var GmCode = [new PushGlobal("main"), new Unwind()];

var GmStack = [];

var GmHeap = {
	objCount:2, 
	freeAddrs:[3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], 
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
function hAlloc(xGmHeap, Node){
	var GmHeap			  = xGmHeap
	var size 	 		  = GmHeap.objCount;
	if(size >= GmHeap.freeAddrs.length){
		console.log("Heap space exhausted");
		throw "stop execution";
	}
	//console.log("Heap: " + GmHeap);
	var freeAddrs 	 	  = GmHeap.freeAddrs;
	console.log(freeAddrs);
	//console.log("freeAddrs: " + freeAddrs);
	var addrObjMapx 	  = GmHeap.addrObjMap;
	var next 	 		  = head(freeAddrs);
	var newAddrs 		  = tail(freeAddrs);
	//console.log("newAddrs: " + newAddrs); 
	var addrObjMapx;
	addrObjMapx[next]	  = Node;
	//console.log("AddrObj: " + addrObjMapx);
	var newHeap 		  = {objCount	: (size+1), 
							freeAddrs	: newAddrs,
							addrObjMap	: addrObjMapx};
	//console.log("leaving hAlloc: " + newHeap.freeAddrs );
	return [newHeap, next];
}

function hLookup(GmHeap, Addr){
	var node = GmHeap.addrObjMap[Addr];
	if(node == undefined){
		console.log("undefined hLookup");
	}
	return node;
}

/*	Add error for non-existant name here */
function aLookup(GmGlobals, Name){
	var global = GmGlobals[Name];
	if(global == undefined){
		console.log("undefined aLookup");
	}
	return global;
}

function getCode(GmState){
	return GmState[0];
}

function putCode(GmCode, GmState){
	var newState = [GmCode, GmState[1], GmState[2], GmState[3]];
	return newState;
}

function getHeap(GmState){
	return GmState[2];
}

function putHeap(GmHeap, GmState){
	var newState = [GmState[0], GmState[1], GmHeap, GmState[3]];
	return newState;
}

function getStack(GmState){
	return GmState[1];
}

function putStack(GmStack, GmState){
	var newState = [GmState[0], GmStack, GmState[2], GmState[3]];
	return newState;
}

function getGlobals(GmState){ 
	return GmState[3];
}

function putGlobals(GmGlobals, GmState){
	var newState = [GmState[0], GmState[1], GmState[2], GmGlobals];
	return newState;
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
function pushglobal(Name, xState){
	console.log("pushglobal called");	
	var State 		= xState;	
	var stack 	 	= getStack(State);
	var globals 	= getGlobals(State);
	var addr 		= aLookup(globals, Name);
	//newStack 	= addr.concat(stack);
	//stack.push(addr);
	stack 			= [addr].concat(stack);
	var newStack 	= stack;
	return putStack(newStack, State);
}	// Looks good 12:45 22/03

/* Pushint :: Int -> GmState -> GmState */
function pushint(Int, xState){
	console.log("pushint called");
	var State 			= xState;
	var heap  			= getHeap(State);
	var stack 			= getStack(State);
	//where node is instanceof func NNum(){}
	var node 			= new NNum(Int);
	var newHeap;
	var addr;
	[newHeap, addr] 	= hAlloc(heap, node);
	//stack.push(addr);
	stack 				= [addr].concat(stack);
	var newStack 		= stack;
	var newState		= putStack(newStack, State);
	var newNewState		= putHeap(newHeap, newState);
	return newNewState;
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
function mkap(xState){
	var oldState 			= xState;
	console.log("mkap called");
	var stack 				= getStack(oldState);
	var heap 				= getHeap(oldState);	
	//replacing heads with pops because a:b:c
	var a1 					= stack[0];
	stack.splice(0,1);
	console.log("mkap a1: " + a1);
	var a2 					= stack[0];
	stack.splice(0,1);
	console.log("mkap a2: " + a2);
	var stackRest 			= stack;
	var node 				= new NAp(a1, a2);
	//console.log(node);
	[newHeap, nodeAddr] 	= hAlloc(heap, node);
	console.log(nodeAddr);
	//newStack 			= nodeAddr.concat(stackRest);
	//console.log("this far?");
	//stack.push(nodeAddr);
	stack 					= [nodeAddr].concat(stack);
	var newStack			= stack;
	var newState 			= putHeap(newHeap, oldState);
	var newNewState 		= putStack(newStack, newState);
	return newNewState;
}	// going to need two nodes to test this

// Push-specific utility func
function getArg(NAp){
	var a2 = NAp.a2;
	if(a2 == undefined){
		console.log("undefined getArg");
	}
	return a2;
}
/* see GEval.hs if(when) you forget how this works*/
/* Push :: Int -> GmState -> GmState */
function push(N, xState){
	console.log("push called");
	var State 		= xState;
	var stack 		= getStack(State);
	var heap 		= getHeap(State)
	var nodeAddr 	= stack[1 + N];
	var arg 		= getArg(hLookup(heap, nodeAddr));
	//newStack 	= node.concat(stack)
	//stack.push(arg);
	stack 			= [arg].concat(stack);
	var newStack 	= stack;
	var newState 	=  putStack(newStack, State);
	return newState;
}	// again, heap specific.

/*
update n state = newState
	where
		stack = getStack state 
		oldAddr = stack !! (n+1)
		(newHeap, newAddr) = hAlloc (getHeap(state)) (NInd oldAddr)
		(a, as) = splitAt (n+1) stack 
		tempStack = a ++ (newAddr : (drop 1 as))
		newStack = drop 1 stack
		newState = (putStack newStack (putHeap newHeap(state)))
*/

/* update :: Int -> GmState -> GmState */
function update(N, xState){
	console.log("update called");
	var State 			= xState;
	var stack 			= getStack(State);
	var oldAddr 		= stack[N+1];
	if(oldAddr == undefined){
		console.log("accessing undefined stack space - line 376");
	}
	var newHeap;
	var newAddr;
	[newHeap, newAddr] 	= hAlloc(getHeap(State), (new NInd(stack[0])));
	var stack;
	stack[N+1] 			= newAddr;
	stack.drop(1);
	var newStack 		= stack;
	var newState 		= putStack(newStack, State);
	var newNewState 	= putHeap(newHeap, newState);
	return newNewState;
}

/* pop :: Int -> GmState -> GmState */
function pop(N, xState){
	console.log("pop called");
	var State 	 = xState;
	var stack 	 = getStack(State);
	var newStack = stack.splice(N, stack.length);
	return putStack(newStack, State);
}	// working fine

/* Literally just drops N, moves bottom/front Node
 * to replace. If old stack = [2,1,0], new stack
 * after a slide 1 will be [2, 0]. Node that heap
 * isn't actually changed by this. Array.drop()
 * defined above. */
/* slide :: Int -> GmState -> GmState */
/*
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
*/


/* Unwind :: GmState -> GmState */
function unwind(xState){
	var State 	= xState;
	console.log("unwind called");
	// be careful with implementation of head/tail	
	var stack 	= getStack(State);
	var heap 	= getHeap(State);
	var a 		= head(stack);
	var as 		= tail(stack);
	var aslen	= as.length;
	var node 	= hLookup(heap, a);
	if(node instanceof NNum){
		return State;
	}
	if(node instanceof NInd){
		var addr 		= node.a;
		var newStack 	= stack;
		newStack.drop(1);
		//newStack.push(addr);
		newStack		= [addr].concat(newStack);
		return putCode([new Unwind()], putStack(newStack, State));
	}	 
	if(node instanceof NAp){
		//Check these next 4 lines carefully
		var a1 			= node.a1;
		//as.push(a);
		//as.push(a1);
		as 				= [a1,a].concat(as);
		var newStack 	= as;
		var unwind 		= new Unwind();
		return putCode([unwind], (putStack(newStack, State)));
	}
	if(node instanceof NGlobal){
		//console.log("unwind NGlobal called");
		var numargs 	= node.numargs;
		var code 		= node.instructions;
		//console.log("numargs: " + numargs + " | code: " + code);
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

/* step :: GmState -> GmState */
function step(xState){
	console.log("step called");
	var State 		= xState;
	// again, check these heads work
	var code 		= getCode(State);
	var i 			= head(code);
	var is 			= tail(code);
	var newState 	= putCode(is, State);
	// these could be really cool higher order things
	// ala GCompiler.hs but ehhhhhhhh
	if(i instanceof PushGlobal){
		var f = i.Name;
		return pushglobal(f, newState);
	}
	if(i instanceof PushInt){
		var n = i.Int;
		return pushint(n, newState);
	}
	if(i instanceof Mkap){
		return mkap(newState);
	}
	if(i instanceof Push){
		var n = i.Int;
		return push(n, newState);
	}
	if(i instanceof Update){
		var a = i.a;
		return update(a, newState);
	}
	if(i instanceof Pop){
		var n = i.Int;
		return pop(n, newState);
	}
	if(i instanceof Unwind){
		return unwind(State);
	}
}

/* gmFinal :: GmState -> Bool */
function gmFinal(xState){
	var State 	   = xState;
	//console.log("gmFinal called");
	var code 	   = getCode(State);
	if(code.length == 0){
		return true;
	} else {
		return false;
	}
} //	working fine 13:37 22/03

var accStates = [];
var iterations = 0;

/* eval :: GmState -> [GmState] */
function evalx(State){
	var currentState = State;
	while(!gmFinal(currentState)){
		//accStates.push(currentState);
		accStates = [currentState].concat(accStates);
		nextState = step(currentState);
		var currentState = nextState;
		if(iterations > 50){
			console.log("eval to infinity. killing");
			return currentState;
		}
		iterations = iterations + 1;
		var code = getCode(currentState);
		console.log("Iteration " + iterations + " - code: " + JSON.stringify(code));
	}
	return currentState;
}

/*****************************************************************************
 *	Output Dump...
*****************************************************************************/

