{-# LANGUAGE GADTs #-}

module Blop where
import Data.List

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

*INFORMAL*

data core = [CoreFuncs]

data CoreFunc =
	fIdent TypeSig Binders Expr

data Binders = [(Vars, Type)]

data Expr
	= Var Id
	| Lit Literal
	| App Expr Expr



--}

---------------------------------------------------------------------------------
-- This is our code representing core programs parsed.
-- The general format is a list of `CoreBind`s, which
-- consist of binders in exprs bound to Var ids.
---------------------------------------------------------------------------------


-- | Binding, used for top level bindings in a module and local bindings in a @let@.
data Bind b 
	= NonRec b (Expr b)
	| Rec [(b, (Expr b))]
  	deriving (Show)

getBindersOf :: Bind b -> [b]
getBindersOf (NonRec binder _) = [binder]
getBindersOf (Rec pairs)       = [binder | (binder, _) <- pairs]

{--
--	These are the definitions as given by GHC's 
--	description of core. mixing with the SPJ
--	implementation defns.
--	
type CoreProgram = [CoreBind]	-- See Note [CoreProgram]
-- The common case for the type of binders and variables when
-- we are manipulating the Core language within GHC
type CoreBndr = Var
-- | Expressions where binders are 'CoreBndr's
type CoreExpr = Expr CoreBndr
-- | Argument expressions where binders are 'CoreBndr's
type CoreArg  = Arg  CoreBndr
-- | Binding groups where binders are 'CoreBndr's
type CoreBind = Bind CoreBndr
--}

-- SPJ Implementations: A tutorial
type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type Id = Var

type Name = String

isScDefn :: ScDefn a -> (String, Bool)
isScDefn (name, [binders], expr) = (name, True)
isScDefn _	= ("nope", False)

-- *Note* - Actual core Vars store more information
-- but the majority is only relevant to machine code
-- output and optimization.
type Var = String

data CoreType
	= CoreInteger Int 
	| CoreBoolean Bool 
	| CoreAddOp
	deriving (Show)

data Expr b
  	= Var	  Id
  	| Lit   Literal
  	| App   (Expr b) (Arg b)
  	| Lam   b (Expr b)
  	| Let   (Bind b) (Expr b)
  {-
  | Case  (Expr b) b Type [Alt b]	-- See #case_invariant#
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion
  -}
	deriving (Show)

isAtomicExpr :: Expr b -> Bool
isAtomicExpr (Var v) = True
isAtomicExpr (Lit l) = True 

type Arg b = Expr b

-- Whether we're hitting a literal or a type int
-- in Core, we still just want the value of it 
-- in JS, so treat all as built-ins
data Literal 
	= LitInt Int 
	| LitString String
	deriving (Show)

data AExpr 
	= Num Int 
	| Plus AExpr AExpr 
	| Mult AExpr AExpr

aExprEval :: AExpr -> Int 
aExprEval (Num n) = n
aExprEval (Plus e1 e2) = (aExprEval e1) + (aExprEval e2)
aExprEval (Mult e1 e2) = (aExprEval e1) * (aExprEval e2)


---------------------------------------------------------------------------------
-- We're now getting into the JS and translation side of things.
---------------------------------------------------------------------------------

-- Utility types to make our code a little more self-documenting
type JSProgram = String

type JSFunc = String 

type JSFuncIdent = String 

type JSParams = [String]

type JSObj = String 

type JSType = String

type JSExpr = String


---Using SPJ Core version. See below for current core 

--coreApp2JS :: Expr b -> String 
--coreApp2JS (App e1 e2) =

--coreLam2JS :: Expr b -> String 
--coreLam2JS (Lam b (expr))

coreExpr2JS :: Expr b -> JSExpr
coreExpr2JS expr = do 
	case expr of 
		(Var v) 	-> v 
		(Lit l) 	-> unLit l
		--(App e1 e2) -> coreApp2JS e1 e2
		--(Lam b e) 	-> coreLam2JS b e 

unLit l = do
	case l of 
		LitInt i 	-> show i 
		LitString s -> s

--mkJSFunc :: JSFuncIdent -> JSParams -> JSExpr -> JSFunc 
--msJSFunc fid params expr 

-- Core output for identify function with type sig:
-- main.Main.fident :: Int -> Int = \ (a::Int) -> a

-- ignore if not already deleted
{--
test :: CoreBind -> CoreBind
test a = a

mkJSFunc :: JSFuncIdent -> JSParams -> JSExpr -> JSFunc 
mkJSFunc fid params expr
	= ( "function " ++ fid
				    ++ "(" ++ p ++ "){" 
				    ++ "return " ++ expr ++ " ;"
				    ++ "}" )
	where p = intercalate "," params

mkJsObj :: (Show val) => Id -> val -> JSObj 
mkJsObj i val 
	= "var " ++ i ++ "{value=" 
			 ++ (show val) ++ ";} \n"

coreBind2JS :: CoreBind -> JSExpr 
coreBind2JS (NonRec binders expr) = do 
	case expr of 
		Var x 
			-> mkJSFunc "fId" binders "x" 
		Lit x					
			-> show x
		App (expr) (arg)		
			-> "aldfj"
		Lam bind (expr)			
			-> "when are these ever used?"
		Let bind (expr)		
			-> "adfad"

coreExprVar2JS :: Expr b -> String
coreExprVar2JS (Var x) = show x

coreExprLit2JS :: Expr b -> String 
coreExprLit2JS (Lit x) = show x

--coreExprApp2JS :: Expr b -> String 
--coreExprApp2JS (App expr arg)

coreProgram2JS :: CoreProgram -> JSProgram
coreProgram2JS coreProg = foldr (++) "" (map coreBind2JS coreProg)

--}

{-

coreExpr2english :: CoreExpr b -> String 
coreExpr2english (expr b) = do
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


-- var objId{ value = val; }
createJSObj :: (Show a) => a -> Id -> JSObj 
createJSObj x i 
	= "var " ++ i ++ "{value=" 
			 ++ (show a) ++ ";}"

-}

{--	keep in mind that the CoreExpr is only the expr
 --	part of the return lambda in a func decl.
 --	*NOTE* or is it. read more.
--}	

{-
coreType2JS :: CoreType -> JSType
coreType2JS t = do 
	case t of
		 CoreInteger i 	-> show i 
		 CoreBoolean b 	-> show b
		 CoreAddOp		-> "+"
-}

{--
	Haskell - f x y = x + y
	Core 	- \ x::Int y::Int -> op.plus @ x y
	JS 		- function f(x, y){return x + y;}
	ie. 	  function fIdent(binders){return CoreExpr;}


--}
