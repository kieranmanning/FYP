
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

function Eval(){

}

function Add(){

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


var GmCode = [new PushGlobal("main"),new Eval()];
 
var GmStack = [];
 
var GmDump = [];
 
var GmHeap = {
objCount:4,
freeAddrs:[5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20],
addrObjMap:{4:new NGlobal(1,[new Push(0),new Eval(),new Neg(),new Update(1),new Pop(1),new Unwind()]),3:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Add(),new Update(2),new Pop(2),new Unwind()]),2:new NGlobal(1,[new Push(0),new Update(1),new Pop(1),new Unwind()]),1:new NGlobal(0,[new PushInt(1),new PushGlobal("Id"),new Mkap(),new Update(0),new Pop(0),new Unwind()])}
};
 
var GmGlobals = {"main":1,"Id":2,"+":3,"neg":4};
 
var GmState = [GmCode, GmStack, GmDump, GmHeap, GmGlobals] 
 
function main(){
	return evalx(GmState);
}
