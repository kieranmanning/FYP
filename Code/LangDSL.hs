{-# LANGUAGE GADTs, StandaloneDeriving #-}

module LangDSL where
{- import qualified Data.Map as Map -}
import qualified Data.Map as Map
import Control.Monad.ST


type Ident 		= String

type SymbolTable= Map.Map Ident Expr

type Evaluation a = ST SymbolTable a

data Program 	= Program StmtList
	deriving(Show, Eq)

type StmtList 	= [Stmt]

data Stmt 
	= Decl Decl 
	| Print Expr
	deriving(Show, Eq)

data Decl where 
	VarDecl 	:: Ident -> Expr -> Decl
	LambdaDecl  :: Ident -> BinderList -> Expr -> Decl
	FuncDecl 	:: Ident -> ParamList -> Expr -> Decl
	deriving(Show, Eq)

type BinderList = [Ident]

type ParamList 	= [Ident]

data Expr where
	GetV 		:: Ident -> Expr
	TypeInt 	:: Int -> Expr
	TypeBool 	:: Bool -> Expr
	TypeList 	:: [Expr] -> Expr
	LetExp 		:: Decl -> Expr -> Expr
	AddOp 		:: Expr -> Expr -> Expr
	SubOp 		:: Expr -> Expr -> Expr
	AndOp 		:: Expr -> Expr -> Expr
	ExprError 	:: Expr
	deriving(Show, Eq)

{-
evalStmt :: Stmt -> Stmt
evalStmt stmt = case stmt of decls, whatever.
					simplify.
-}

{-					Eval decls 					-}



{- 					Eval Exprs					-}

{-
evalExpr :: Expr -> Expr
evalExpr = \x -> bigFuckOffCaseStatement x
-}

{-
evalGetV :: Ident -> Expr
evalGetV = \x -> lookup x
-}



evalExprListOp :: Expr -> Expr 
evalExprListOp expr = do 
	case expr of 
		TypeList x -> TypeList x
		_ -> ExprError


evalExprIntOp :: Expr -> Expr 
evalExprIntOp expr = do 
	case expr of
		TypeInt x -> TypeInt x
		x -> TypeInt (eval x)
	where eval x = do 
		case x of
			TypeInt x -> x
			AddOp x y -> (eval x) + (eval y)
			SubOp x y -> (eval x) - (eval y)


evalExprBoolOp :: Expr -> Expr 
evalExprBoolOp expr = do 
	case expr of
		TypeBool x -> TypeBool x
		x -> TypeBool (eval x)
	where eval x = do 
		case x of 
			TypeBool x -> x
			AndOp x y -> (eval x) && (eval y)
