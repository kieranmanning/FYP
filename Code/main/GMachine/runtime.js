
/*****************************************************************************
 *	Some General Utility Functions
*****************************************************************************/

function head(list){
	x = list;
	return x.reverse().pop();
}

function tail(list){
	x = list;
	discard = x.reverse().pop();
	return x.reverse();
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

function NGlobal(arity, instructions){
	this.arity = arity;
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

/* hAlloc :: GmHeap -> Node -> GmHeap */
function hAlloc(GmHeap, Node){
	size = GmHeap[0];
	addrs = GmHeap[1];
	addrObjs = GmHeap[2];
	next 	 = head(addrs);
	newAddrs = tail(addrs);
	addrObjs[next] = Node;
	newHeap = [(size-1), newAddrs, addrObjs];
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
 *	Reminder
 *  Stack is just an array of Ints, 
 *  Heap consists of (Free, Addrs, [Addr->Name])
 */

/* pushglobal :: Name -> GmState -> GmState */
function pushglobal(Name, oldState){
	oldStack = getStack(oldState);
	heap 	 = getHeap(oldState);
	nameAddr = hLookup(Name, heap);
	newStack = nameAddr.concat(oldStack);
	return putStack(newStack, oldState);
}

/* Pushint :: Int -> GmState -> GmState */
function pushint(Int, oldState){
	heap  = getHeap(oldState);
	//where node is instanceof func NNum(){}
	node = new NNum(Int);
	(newHeap, addr)  = hAlloc(heap, node);
	newStack = addr.concat(stack)
	newState = putStack(oldState);
	newNewState = putHeap(newState);
	return newNewState;
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
	stack = getStack(oldState);
	a1 = head(stack);
	a2 = head(stack);
	stackRest = stack;
	node = new NAp(a1, a2);
	(newHeap, nodeAddr) = hAlloc(node);
	newStack = nodeAddr.concat(stackRest);
	return putState(newStack, oldState);
}

/* see GEval.hs if(when) you forget how this works*/
/* Push :: Int -> GmState -> GmState */
function push(N, OldState){
	function getArg(NAp){
		return NAp.a2;
	}
	oldStack = getStack(oldState);
	nodeAddr = oldStack[1 + N];
	arg = getArg(hLookup(getHeap(OldState), nodeAddr);
	return putStack(node.concat(oldStack), oldState);
}

/* Literally just drops N, moves bottom/front Node
 * to replace. If old stack = [2,1,0], new stack
 * after a slide 1 will be [2, 0]. Node that heap
 * isn't actually changed by this. */
/* slide :: Int -> GmState -> GmState */
function slide(N, OldState){
	a = head(getStack(oldState));
	as = tail(getStack(oldState));
	newStack = a.concat( (oldStack.drop(N)));
	return putStack(newStack, oldstate);
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
function unwind(oldState){
	stack = getStack(oldState);
	heap = getHeap(oldState);
	a = head(stack);
	as = head(stack);

	node = hLookup(heap, a);
	if(node == NNum a)


}

/*****************************************************************************
 *	Evaluator
*****************************************************************************/

/* eval :: GmState -> [GmState] */
function eval(GmState){

}

/* gmFinal :: GmState -> Bool */
function gmFinal(GmState){
	code = getCode(GmState);
	return (code.length == 0);
}

/* step :: GmState -> GmState */
function step(GmState){
	iConsIs = getCode(GmState);
	i = iConsIs.reverse().pop();
	is = iConsIs.reverse();
}


