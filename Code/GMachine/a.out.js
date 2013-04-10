var GmOutput = [];
 
var GmCode = [new PushGlobal("main"),new Eval()];
 
var GmStack = [];
 
var GmDump = [];
 
var GmHeap = {
objCount:10,
freeAddrs:[11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100],
addrObjMap:{
10:new NGlobal(3,[new Push(0),new Eval(),new Cond([new Push(1)],[new Push(2)]),new Update(3),new Pop(3),new Unwind()]),
9:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Neq(),new Update(2),new Pop(2),new Unwind()]),
8:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Eq(),new Update(2),new Pop(2),new Unwind()]),
7:new NGlobal(1,[new Push(0),new Eval(),new Neg(),new Update(1),new Pop(1),new Unwind()]),
6:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Div(),new Update(2),new Pop(2),new Unwind()]),
5:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Mul(),new Update(2),new Pop(2),new Unwind()]),
4:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Sub(),new Update(2),new Pop(2),new Unwind()]),
3:new NGlobal(2,[new Push(1),new Eval(),new Push(1),new Eval(),new Add(),new Update(2),new Pop(2),new Unwind()]),
2:new NGlobal(1,[new Push(0),new Eval(),new Update(1),new Pop(1),new Unwind()]),
1:new NGlobal(0,[new PushInt(7),new PushInt(1),new Sub(),new Update(0),new Pop(0),new Unwind()])}
};
 
var GmGlobals = {"main":1,"Id":2,"+":3,"-":4,"*":5,"/":6,"neg":7,"==":8,"!=":9,"if":10};
 
var GmState = [GmOutput, GmCode, GmStack, GmDump, GmHeap, GmGlobals]; 
 
function main(){
	return evalProg(GmState);
}