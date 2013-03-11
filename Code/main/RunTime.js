

/*
 *	Representing types and ops as JS funcs
 */

/*
 *  Types. Ok. So. If our type safety is already taken 
 *  care of, it shouldn't be too crazy to say we can 
 *  express everything as a single 'object' type.
 */

"""
Credit to Fay in report for a lot of this. Maybe dig
out utrecht papers is citations necessary.
"""

function Thunk(val){
	this.forced = false;
	this.value = val;
}

function JSApplyFunc(){
	var f = arguments[0];
	for (var i=1, len = arguments.lenght; i < len; i++){
		f = (f instanceof JSObj? _(f) : f)(arguments[i]);
	}
	return f;
}

JSObj.prototype.force = function(nocache){
	return nocache? 
}

/* Lists */

function JSCons(x, xs){

}

function JSList(){

}

/* Because laziness */
function JSCons(x){
	return function(y){
		return new JSCons(y, y);
	}
}

function 