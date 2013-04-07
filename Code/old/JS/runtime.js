
"""
Credit to Fay in report for a lot of this. Maybe dig
out utrecht papers if citations necessary.
"""

/*
 *	runtime.js will provide the runtime for our language. Reduces
 *	the amount of code we need to spit out.
 */

/*	identityFunc example...
 *	
 *	f a = a will become 
 *	
 *	Main.f :: %forall auniq . auniq -> auniq =
 *		\ @ aTypeVar (a::aTypeVar) -> a
 *
 *	which in a strict sense would be converted to
 *	
 *	function f(a){return a;}
 *
 *	all we need to look at is main.f = a -> a
 *
 *
 *
 *	in practice, that's still correct but 'a' will
 *	need to be considered a thunk. It shouldn't be
 *	forced in the identify function, however.
 *
 *	co-incidentally, errors for undersaturated 
 *	funcs are going to be tricky
 *	
 */


/*******************************************************************************
 * Worky Bits.
 *******************************************************************************/
function thunk(val){
	this.forced = false;
	this.value = val;
}

function ForceThunk(thunkish){

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



/*******************************************************************************
 * Integer Operations.
 *******************************************************************************/

function JSIntegerAddUnderSaturated(x){
	return function(y){
		return new thunk(function(){
			return force(x) + force(y);
		});
	}
}

function JSIntegerAddFullySaturated(x, y){
	return new thunk(function(){
		return force(x) + force(y);
	});
}

