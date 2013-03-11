{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification  #-}

module LangDSL where

type Ident = String

data LangProgram = LangProgram StmtList
	deriving(Show, Eq)

type StmtList = [Stmt]

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

type ParamList = [Ident]

data Expr where
	GetV :: Ident -> Expr
	TypeInt :: Int -> Expr
	TypeBool :: Bool -> Expr
	TypeList :: [Expr] -> Expr
	LetExp :: Decl -> Expr -> Expr
	FuncAppl :: Ident -> [Expr] -> Expr
	AddOp :: Expr -> Expr -> Expr
	SubOp :: Expr -> Expr -> Expr
	AndOp :: Expr -> Expr -> Expr
	ExprError :: Expr
	deriving(Show, Eq)

{-
evalStmt :: Stmt -> Stmt
evalStmt stmt = case stmt of decls, whatever.
					simplify.
-}

{-					Eval decls 					-}
evalDecl :: Decl -> Decl 
evalDecl decl = do 
	case decl of
		(VarDecl i e) 		-> evalVarDecl(VarDecl i e)
		(LambdaDecl i b e) 	-> evalLambdaDecl(LambdaDecl i b e)
		(FuncDecl i p e) 	-> evalFuncDecl(FuncDecl i p e)

{- evalVarDecl :: Decl -> Store() -}
evalVarDecl :: Decl -> Decl
evalVarDecl decl = decl

{- evalLambdaDecl :: Decl -> Store() -}
evalLambdaDecl :: Decl -> Decl
evalLambdaDecl decl = decl

{- evalFuncDecl :: Decl -> Store() -}
evalFuncDecl :: Decl -> Decl
evalFuncDecl decl = decl

{- 					Eval Exprs					-}
evalExpr :: Expr -> Expr
evalExpr expr = do
	case expr of
		GetV x 		-> evalExprLookUp	(GetV x)
		TypeInt x 	-> evalExprIntOp	(TypeInt x)
		TypeBool x 	-> evalExprBoolOp	(TypeBool x)
		TypeList x 	-> evalExprListOp	(TypeList x)
		LetExp d e 	-> evalLetExp		(LetExp d e)
	{-	FuncAppl x 	-> evalFuncAppl		(FuncAppl x)	-}
		AddOp a b 	-> evalExprIntOp	(AddOp a b)
		SubOp a b 	-> evalExprIntOp	(SubOp a b)
		AndOp a b 	-> evalExprBoolOp	(AndOp a b)


evalExprLookUp :: Expr -> Expr 
evalExprLookUp get = get

evalExprFuncAppl :: Expr -> Expr
evalExprFuncAppl fget = fget


evalExprListOp :: Expr -> Expr 
evalExprListOp expr = do 
	case expr of 
		TypeList x 	-> TypeList x
		_ 			-> ExprError


evalExprIntOp :: Expr -> Expr 
evalExprIntOp expr = do 
	case expr of
		TypeInt x 	-> TypeInt x
		x 			-> TypeInt (eval x)
	{-	_ 			-> ExprError	-}
	where eval x = do 
		case x of
			TypeInt x -> x
			AddOp x y -> (eval x) + (eval y)
			SubOp x y -> (eval x) - (eval y)


evalExprBoolOp :: Expr -> Expr 
evalExprBoolOp expr = do 
	case expr of
		TypeBool x 	-> TypeBool x
		x 			-> TypeBool (eval x)
	{-	_ 			-> ExprError	-}
	where eval x = do 
		case x of 
			TypeBool x 	-> x
			AndOp x y 	-> (eval x) && (eval y)

evalLetExp :: Expr -> Expr 
evalLetExp letxp = letxp