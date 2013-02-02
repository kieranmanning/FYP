{-# LANGUAGE GADTs, StandaloneDeriving #-}

module LangDSL where


data Empty

data StmtList
	= Stmt StmtList
	| Empty
	deriving (Show, Eq)

data Stmt 
	= Decl 
	| Print Stmt 


data Expr 
	= ExprInt Int
	| ExprBool Bool 
	| ExprString String 
	| ExprOp Expr Expr 
	| ExprLambda Lambda 

data Lambda = Lambda BinderList Expr 

data BinderList 
	= Ident BinderList
	| Exmpty

data Decl
	= Ident Expr 

data Type 
	= TypeInt
	| TypeBool
	| TypeString
	| TypeIdent Ident 
	| TypeIdentList Ident
	deriving (Show, Eq)

data Term
	= 




{-
type Var = String
type Binders =  [Var]	

data Term a where
	Expr    :: Expr a -> Term a 
	Factor  :: Factor a -> Term a
deriving instance Show a => Show (Term a)

data Factor a where
	N      :: Int -> Factor Int
	B      :: Bool -> Factor Bool 
	Lambda :: Binders -> Term a -> Factor a
	List   :: [Factor a] -> Factor a
deriving instance Show a => Show (Factor a)

data Stmt a where
	Seq    :: Stmt a -> Stmt a -> Stmt a
	Decl   :: Var -> Term a -> Stmt a
deriving instance Show a => Show (Stmt a)

data Expr a where
	--Numerical expressions:
	Add  :: Expr Int  ->  Expr Int ->  Expr Int
	--Boolean expressions:
	And  :: Expr Bool ->  Expr Bool ->  Expr Bool
	--List expressions:
	Con  :: Expr [a] -> Expr [a] -> Expr [a]
	--Loading something not a constant:
	GetV :: String -> Expr Int
deriving instance Show a => Show (Expr a)

-}
