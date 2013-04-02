// need something to call from the html test page so things load.
// replace this with a main(){eval(prog)} when finished.
function test(){

}

var True = true;
var False = false;

/*****************************************************************************
 *	Some Important Notes
*****************************************************************************/
/*
 *	-Contents
 *		1. General utils 
 *		2. G-Machine state representation
 *		3. Test Env (remove in final)
 *		4. G-machine state utility functions
 */

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
 *	Note that haskell treats list decons as [head:tail] whereas JS
 *	is all like [tail:head], where head will be what it hits with
 *	JS primitive push/pop.
 *
 *	- Death to javascript.
 *	Just yeah.	
 *
 *	- Name
 *	Needs a name. cataλyst too cheesy? not exactly pressing at any rate.
 *
 *	- In place operators
 *	For me: be careful with in-place operators, they are the devil.
 *	For anyone else: there are some code hunks that might seem a 
 *	bit odd, in particular when accessing lists. JS pop/push effect
 *	the opposite end of a list to haskell, and their in-place nature
 *	(along with that of slice) make certain operations a little bit
 *	un-intuitive.
 *
 *
 *	- Efficiency and general code quality
 *	Needs to be refactored for efficiency and general cleanliness.
 *	Dump unnecessary comments etc. to keep file size low.
 */

/*****************************************************************************
 *	Some General Utility Functions
*****************************************************************************/

function id(x){
	return x;
}

function head(list){
	var x = list[0];
	return x;
}

function tail(list){
	var x = list;
	//console.log("x: " + JSON.stringify(x));
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

function NConstr(t, a){
	this.t = t;
	this.a = a;
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

function Split(n){
	this.n = n;
}

function Slide(n){
	this.n = n;
}

function Mkap(){

}

function Update(A){
	this.a = A;

}

function Pack(t, n){
	this.t = t;
	this.n = n;
}

function Cond(i1, i2){
	this.i1 = i1;
	this.i2 = i2;
}

function Casejump(a, b){
	this.a = a;
	this.b = b;
}

function Pop(Int){
	this.Int = Int;
}

function Eval(){

}

function Add(){

}

function Sub(){

}

function Div(){

}

function Mul(){

}

function Neq(){

}

function Neg(){

}

function Eq(){

}

function Cond(c1, c2){
	this.c1 = c1;
	this.c2 = c2;
}


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
	var Heap			  = xGmHeap
	var size 	 		  = Heap.objCount;
	var freeAddrs 	 	  = Heap.freeAddrs;
	var addrObjMapx 	  = Heap.addrObjMap;
	var next 	 		  = head(freeAddrs);
	var newAddrs 		  = tail(freeAddrs);
	addrObjMapx[next]	  = Node;
	var newHeap 		  = {objCount	: (size+1), 
							freeAddrs	: newAddrs,
							addrObjMap	: addrObjMapx};
	return [newHeap, next];
}

function hLookup(GmHeap, Addr){
	var node = GmHeap.addrObjMap[Addr];
	if(node == undefined){
		console.log("undefined hLookup" + Addr);
	}
	return node;
}

/*	Add error for non-existant name here */
function aLookup(GmGlobals, Name){
	var global = GmGlobals[Name];
	if(global == undefined){
		console.log("undefined aLookup: " + Name);
	}
	return global;
}

function getOutput(GmState){
	return GmState[0];
}

function putOutput(GmOutput, GmState){
	var newState = [GmOutput, GmState[1], GmState[2], GmState[3], Gmstate[4], Gmstate[5]];
	return newState;	
}

function getCode(GmState){
	return GmState[1];
}

function putCode(GmCode, GmState){
	var newState = [GmState[0], GmCode, GmState[2], GmState[3], GmState[4], GmState[5]];
	return newState;
}

function getStack(GmState){
	return GmState[2];
}

function putStack(GmStack, GmState){
	var newState = [GmState[0], GmState[1], GmStack, GmState[3], GmState[4], GmState[5]];
	return newState;
}

function getDump(GmState){
	return GmState[3];
}

function putDump(GmDump, GmState){
	var newState = [GmState[0], GmState[1], GmState[2], GmDump, GmState[4], GmState[5]];
	return newState;
}

function getHeap(GmState){
	return GmState[4];
}

function putHeap(GmHeap, GmState){
	var newState = [GmState[0], GmState[1], GmState[2], GmState[3], GmHeap, GmState[5]];
	return newState;
}

function getGlobals(GmState){ 
	return GmState[5];
}

function putGlobals(GmGlobals, GmState){
	var newState = [GmState[0], GmState[1], GmState[2], GmState[3], GmState[4], GmGlobals];
	return newState;
}

function dumpEmpty(GmDump){
	return ((GmDump[0].length == 0) && (GmDump[1].length == 0))
}


// All these tested 12:45 22/03

/*****************************************************************************
 *	Our G-code instructions
*****************************************************************************/

/* pushglobal :: Name -> GmState -> GmState */
function pushglobal(Name, xState){
	//console.log("pushglobal called");	
	var State 		= xState;	
	var stack 	 	= getStack(State);
	var globals 	= getGlobals(State);
	var addr 		= aLookup(globals, Name);
	var newStack 	= [addr].concat(stack);
	return putStack(newStack, State);
}	// Looks good 12:45 22/03

/* Pushint :: Int -> GmState -> GmState */
function pushint(Int, xState){
	var State 			= xState;
	var heap  			= getHeap(State);
	var stack 			= getStack(State);
	var node 			= new NNum(Int);
	var newHeap;
	var addr;
	[newHeap, addr] 	= hAlloc(heap, node);
	var newStack 		= [addr].concat(stack);
	var newState		= putStack(newStack, State);
	var newNewState		= putHeap(newHeap, newState);
	return newNewState;
}	// Looking alright 12:49 22/03

/*
function pushint(Int, xState){
	var State = xState;
	var heap = getHeap state;
	var stack = getStack state;
	var globals = getGlobals state;
	var exists = false;
	for(var i=0;i<globals.length;i++){
		if(globals[i] == Int){
			var newStack = [i].concat(stack);
			return putStack(newStack, State);
		}
	} 
	var newHeap;
	var addr;
	[newHeap, addr] = hAlloc(heap, new NNum(Int));
	var newStack = [addr].concat(stack);
	var newGlobals = []
}
*/

/* Mkap :: GmState -> GmState */
function mkap(xState){
	var oldState 			= xState;
	var stack 				= getStack(oldState);
	var heap 				= getHeap(oldState);	
	var a1 					= stack[0];
	stack.splice(0,1);
	var a2 					= stack[0];
	stack.splice(0,1);
	var stackRest 			= stack;
	var node 				= new NAp(a1, a2);
	[newHeap, nodeAddr] 	= hAlloc(heap, node);
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

function push(N, xState){
	//console.log("push called");
	var State 		= xState;
	var stack 		= getStack(State);
	var newStack	= [stack[N]].concat(stack);
	var newState 	= putStack(newStack, State);
	return newState;
}	// again, heap specific.

// check for case of n == 0
function pack(t, n, xState){
	var State = xState;
	var stack = getStack(State);
	var heap  = getHeap(State);
	var addrs = [];
	for(var x = 0; x < n; x++){
		addrs[x] = stack[x];
	}
	var newHeap;
	var addr;
	[newHeap, addr] = hAlloc(heap, new NConstr(t, addrs));
	stack.drop(n);
	var newStack = [addr].concat(stack);
	return putStack(newStack, (putHeap(newHeap, State)));
}


function casejump(tagbranches, xState){
	var State = xState;
	var stack = getStack(State);
	var i = getCode(State);
	var node = hLookup(getHeap(State), head(stack));
	var tag = node.t;
	for(var j=0;j<tagbranches.length;j++){
		if(tagbranches[j][0] == tag){
			var branchCode = tagbranches[j][1];
			return putCode(branchCode.concat(i), State);
		}
	}
	console.error("shit the bed in casejump");
}

function split(n, xState){
	var State = xState;
	var stack = getStack(State);
	var node = hLookup(getHeap(State), head(stack));
	var addrs = node.a;
	var addrsUsed = [];
	stack.drop(1);
	for(var i=0;i<n;i++){
		addrsUsed[i] = addrs[i];
	}
	var newStack = addrsUsed.concat(stack);
	return putStack(newStack, State);
}

/* update :: Int -> GmState -> GmState */
function update(N, xState){
	//console.log("update called");
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
	//console.log("pop called");
	var State 	 = xState;
	var stack 	 = getStack(State);
	var newStack = stack.splice(N, stack.length);
	return putStack(newStack, State);
}	// working fine

/* eval :: GmState -> GmState */
function evalInst(xState){
	console.log("evalInst called");
	var State = xState;
	var Stack = getStack(State);
	var i = getCode(State);
	var a = head(Stack); 
	var s = tail(Stack);
	var newDump = [[i,s]].concat(getDump(State));
	console.log("New dump: " + newDump);
	var newState = putStack([a], State);
	var newNewState = putCode([new Unwind], newState);
	var finalState = putDump(newDump, newNewState);
	return finalState;
}

// working
/* boxInteger :: Int -> GmState -> GmState */
function boxInteger(N, xState){
	var State = xState;
	var h;
	var a;
	[h,a] = hAlloc(getHeap(State), (new NNum(N)));
	//console.log(a);
	var newStack = [a].concat(getStack(State));
	var newState = putStack(newStack, (putHeap(h, State)));
	return newState;
}

// working
/* unboxInteger :: Int -> GmState -> GmState */
function unboxInteger(A, xState){
	var State = xState;
	var node = hLookup(getHeap(State), A);
	if(node instanceof NNum){
		return node.n;
	} else {
		console.log("error unboxing non-int");
	}
}

/* boxBoolean :: Bool -> GmState -> GmState */
function boxBoolean(b, xState){
	var State = xState;
	if(b == true){
		var newB = 1;
	} else {
		if(b == false){
			var newB = 2;
		} else {
			console.error("unexpected boolean representation");
		}
	}
	var newHeap;
	var a;
	[newHeap, a] = hAlloc(getHeap(State), (new NConstr(newB, [])))
	var newStack = [a].concat(getStack(State));
	return putStack(newStack, 
		   putHeap(newHeap, State));
}

// seems to be working
/* primitive1 :: (b -> GmState -> GmState)
			  -> (Addr -> GmState -> a)
			  -> (a -> b)
			  -> (GmState -> GmState) */
function primitive1(box, unbox, op, xState){
	var State = xState;
	var Stack = getStack(State);
	var a = head(Stack);
	var as = tail(Stack);
	console.log(as);
	var val = unbox(a, State);
	var result = op(val);
	var newState = putStack(as, State);
	var newNewState = box(result, newState);
	return newNewState;
}

// seems to be working
/* primitive2 :: (b -> GmState -> GmState)
 			  -> (Addr -> GmState -> a)
 			  -> (a -> a -> b)
 			  -> (GmState -> GmState) */
function primitive2(box, unbox, op, xState){
	var State = xState;
	var Stack = getStack(State);
	var a0 = head(Stack);
	var a1 = head(tail(Stack));
	var as = tail(tail(Stack));
	var val0 = unbox(a0, State);
	var val1 = unbox(a1, State);
	var result = op(val0, val1);
	var newState = putStack(as, State);
	return box(result, newState);
}

var x;
var y;
/* cond :: GmCode -> GmCode -> GmState -> GmState */
function cond(i1, i2, xState){
	x = i1;
	y = i2;
	var State = xState;
	var i = getCode(State);
	var stack = getStack(State);
	var a = head(stack);
	var as = tail(stack);
	var node = hLookup(getHeap(State), a);
	console.log("node.t: " + node.t);
	if(!(node instanceof NConstr)){
		console.error("cond called on non-bool");
		return "SHITTHEBED";
	}
	if(node.t == 2){
		return putCode(i2.concat(i), putStack(as, State));
	}
	if(node.t == 1){
		return putCode(i1.concat(i), putStack(as, State));
	}
	console.error("unsuccesful cond");
}

/* working */
function neg(xState){
	var State = xState;
	function op(x){
		return -x;
	}
	return primitive1(boxInteger, unboxInteger, op, State);
}

/* working */
function add(xState){
	var State = xState;
	function op(x, y){
		return x + y;
	}
	return primitive2(boxInteger, unboxInteger, op, State);
}

function mul(xState){
	var State = xState;
	function op(x, y){
		return x * y;
	}
	return primitive2(boxInteger, unboxInteger, op, State);	
}

function div(xState){
	var State = xState;
	function op(x, y){
		return x / y;
	}
	return primitive2(boxInteger, unboxInteger, op, State);
}

function sub(xState){
	var State = xState;
	function op(x, y){
		return x - y;
	}
	return primitive2(boxInteger, unboxInteger, op, State);
}

function eq(xState){
	var State = xState;
	function op(x, y){
		return (x == y);
	}
	return primitive2(boxBoolean, unboxInteger, op, State);
}

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
	//console.log("unwind called");
	var stack 	= getStack(State);
	var heap 	= getHeap(State);
	var a 		= head(stack);
	var as 		= tail(stack);
	var aslen	= as.length;
	var node 	= hLookup(heap, a);
	if(node instanceof NNum){
		var dump = getDump(State);
		if(dumpEmpty(dump)){
			return State;
		} else {
			[c, s] = head(dump);
			ds = tail(dump);
			var newState = putDump(ds, 
						  (putCode(c, 
						  (putStack([a].concat(s), 
						  (State))))))
			return newState;
		};
	}
	if(node instanceof NConstr){
		var dump = getDump(State);
		if(dumpEmpty(dump)){
			return State;
		} else {
			[c, s] = head(dump);
			ds = tail(dump);
			var newState = putDump(ds, 
						  (putCode(c, 
						  (putStack([a].concat(s), 
						  (State))))))
			return newState;
		};
	}
	if(node instanceof NInd){
		var addr 		= node.a;
		var newStack 	= stack;
		newStack.drop(1);
		newStack		= [addr].concat(newStack);
		return putCode([new Unwind()], putStack(newStack, State));
	}	 
	if(node instanceof NAp){
		var a1 			= node.a1;
		as 				= [a1,a].concat(as);
		var newStack 	= as;
		var unwind 		= new Unwind();
		return putCode([unwind], (putStack(newStack, State)));
	}
	if(node instanceof NGlobal){
		var numargs 	= node.numargs;
		var code 		= node.instructions;
		if((stack.length-1) < numargs){
			console.error("unwinding undersaturated. see 3.7.2");
		} else {
			var newState = 
				putCode(code, 
				putStack(rearrange(numargs, heap, stack), State)
				);
			return(putCode(code, newState));
		}
	} else {
		console.error("unwind failing");
	}
}


/* rearrange :: Int -> GmHeap -> GmStack -> GmStack */
function rearrange(n, heap, as){
	var newas = [];
	var oldas = as;
	for(var x = 0; x < n; x++){
		newas[x] = getArg(hLookup(heap, tail(as)[x]));
	}
	oldas.drop(n);
	return newas.concat(oldas);
}

/*****************************************************************************
 *	Evaluator
*****************************************************************************/

var tempState;
var lastinst;
/* step :: GmState -> GmState */
function step(xState){
	//console.log("step called");
	var State 		= xState;
	tempState 		= State;
	// again, check these heads work
	var code 		= getCode(State);
	var i 			= head(code);
	lastinst		= i;
	//console.log(JSON.stringify(code));
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
	if(i instanceof Split){
		var n = i.n;
		return split(n);
	}
	if(i instanceof Pack){
		var n = i.n;
		return pack(n);
	}
	if(i instanceof Unwind){
		return unwind(newState);
	}
	if(i instanceof Pack){
		var n = i.n;
		var t = i.t;
		return pack(t, n);
	}
	if(i instanceof Casejump){
		var a = i.a;
		var b = i.b;
		return casejump(a, b);
	}
	if(i instanceof Add){
		return add(newState);
	}
	if(i instanceof Sub){
		return sub(newState);
	}
	if(i instanceof Mul){
		return mul(newState);
	}
	if(i instanceof Neg){
		return neg(newState);
	}
	if(i instanceof Eq){
		return eq(newState);
	}
	if(i instanceof Eval){
		return evalInst(newState);
	}
	if(i instanceof Cond){
		var c1 = i.c1;
		var c2 = i.c2;
		return cond(c1, c2, newState);
	}
	console.error("no such instruction in dispatch: " + i)
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
function evalProg(State){
	var currentState = State;
	while(!gmFinal(currentState)){
		//accStates.push(currentState);
		accStates = [currentState].concat(accStates);
		nextState = step(currentState);
		currentState = nextState;
		if(iterations > 300){
			console.log("eval to infinity. killing");
			iterations = 0;
			return currentState;
		}
		iterations = iterations + 1;
		var code = getCode(currentState);
		console.log("Iteration " + iterations + " - code: " + JSON.stringify(code));
	}
	var topAddr = head(getStack(currentState));
	console.log( hLookup(getHeap(currentState), topAddr) );
	return currentState;
}

/*****************************************************************************
 *	Output Dump...
*****************************************************************************/

var GmOutput = [];
 
var GmCode = [new PushGlobal("main"),new Eval()];
 
var GmStack = [];
 
var GmDump = [];
 
var GmHeap = {
objCount:12,
freeAddrs:[13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100],
addrObjMap:{
12:new NGlobal(3,[new Push(0),new Eval(),new Cond([new Push(1)],[new Push(2)]),new Update(3),new Pop(3),new Unwind()]),
11:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Neq(),new Update(2),new Pop(2),new Unwind()]),
10:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Eq(),new Update(2),new Pop(2),new Unwind()]),
9:new NGlobal(1,[new Push(0),new Eval(),new Neg(),new Update(1),new Pop(1),new Unwind()]),
8:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Div(),new Update(2),new Pop(2),new Unwind()]),
7:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Mul(),new Update(2),new Pop(2),new Unwind()]),
6:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Sub(),new Update(2),new Pop(2),new Unwind()]),
5:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Add(),new Update(2),new Pop(2),new Unwind()]),
4:new NGlobal(1,[new Push(0),new Eval(),new Update(1),new Pop(1),new Unwind()]),
3:new NGlobal(0,[new PushInt(3),new PushGlobal("fac"),new Mkap(),new Eval(),new Update(0),new Pop(0),new Unwind()]),
2:new NGlobal(1,[new Push(0),new PushGlobal("xeq0"),new Mkap(),new Eval(),new Cond([new PushInt(1)],[new PushInt(1),new Push(1),new PushGlobal("-"),new Mkap(),new Mkap(),new PushGlobal("fac"),new Mkap(),new Eval(),new Push(1),new Eval(),new Mul()]),new Update(1),new Pop(1),new Unwind()]),
1:new NGlobal(1,[new Push(0),new Eval(),new PushInt(0),new Eq(),new Update(1),new Pop(1),new Unwind()])}
};
 
var GmGlobals = {"xeq0":1,"fac":2,"main":3,"Id":4,"+":5,"-":6,"*":7,"/":8,"neg":9,"==":10,"!=":11,"if":12};
 
var GmState = [GmOutput, GmCode, GmStack, GmDump, GmHeap, GmGlobals]; 
 
function main(){
	return evalProg(GmState);
}