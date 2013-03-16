{-# LANGUAGE GADTs #-}

module Blop where

{--
	General format of function:

	main:Main:fIdent :: universally quantified type sig =

		\ [param::type] -> func @ [type params] 

	general application form...

	op.name @ resultType opType [params]

	nested functions...

	\ [param:type] -> op.name @ resultType opType 
						(op.name @ resultType opType x y)  
							z

	For immediate future, assume type-sigs in dough code
	and we can ignore polymorphic types in bread

--}

--	%forall [] = \ []		 -> func @ stuff
--	We'll work with a non-nested non-type decl one for now

{--
* Lets worry about functions first*

data CoreProgram
	= [CoreBind]

data CoreBind 
	= NonRec b (CoreExpr b)
	| Rec [(b, (CoreExpr b))]
--}

data CoreFunc 
	= CoreFunc CoreFuncIdent CoreBinderList (CoreExpr )
	deriving Show

type CoreFuncIdent 
	= String

type Id = Var

type Var = String

-- \ (x::Int) (y::Int) 
type CoreBinderList 
	= [(Id, CoreType)]

data CoreType
	= CoreInteger Int 
	| CoreBoolean Bool 
	| CoreAddOp
	deriving Show

{--
**I really dont know if this is going to be necessary.
Maybe if we were screwing with types.

data Var
  = Id {
	varName    :: !Name,
	realUnique :: FastInt,
   	varType    :: Type,
	idScope    :: IdScope,
	id_details :: IdDetails,	-- Stable, doesn't change
	id_info    :: IdInfo }		-- Unstable, updated by simplifier
    deriving Typeable
--}

-- Note that ops are currently represented as types
data CoreExpr
	= App (CoreExpr) (Arg)
	| Lam CoreExpr
	| Var Id 
	| Lit CoreLiteral
	| Type CoreType
	deriving Show

type Arg = CoreExpr 

-- Whether we're hitting a literal or a type int
-- in Core, we still just want the value of it 
-- in JS, so treat all as built-ins
data CoreLiteral 
	= CoreLitInt Int 
	| CoreLitString String
	deriving Show

coreExpr2english :: CoreExpr -> String 
coreExpr2english expr = do
	case expr of
		App e a -> " @ " ++ "(" ++ (cE2e e) ++ ") (" ++ (cE2e a) ++ ") "
		Var i 	-> i
		Lit i 	-> (unLit i) 
		Type t 	-> (unType t)
	where cE2e = coreExpr2english

unLit i = do
	case i of
		CoreLitInt int 		-> show int
		CoreLitString str 	-> show str 

unType t = do
	case t of
		CoreInteger i  	-> show i 
		CoreBoolean b   -> show b 
		CoreAddOp 		-> "+"

type JSProgram = String

type JSFunc = String 

type JSObj = String 

type JSType = String

createJSFunc :: CoreFuncIdent -> CoreExpr -> JSFunc
createJSFunc fId cexpr
	= "function " + (show fId) + "(){" +


{--	keep in mind that the CoreExpr is only the expr
 --	part of the return lambda in a func decl
 --	
--}	

coreType2JS :: CoreType -> JSType
coreType2JS t = do 
	case t of
		 CoreInteger i 	-> show i 
		 CoreBoolean b 	-> show b
		 CoreAddOp		-> "+"


{--
	Haskell - f x y = x + y
	Core 	- \ x::Int y::Int -> op.plus @ x y
	JS 		- function f(x, y){return x + y;}
	ie. 	  function fIdent(binders){return CoreExpr;}


--}
coreApp2JS :: CoreExpr -> JSFunc 
coreApp2JS (App e a) = 

{--
coreExpr2JS :: CoreExpr -> JSProgram
coreExpr2JS expr = do
	case expr of
		Lam
--}