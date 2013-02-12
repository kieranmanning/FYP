{-# LANGUAGE GADTs, StandaloneDeriving #-}

module LangDSL where
import qualified Data.Map as Map
import Control.Monad.State

type Result = StateT DataStore 

type DataStore = Map.Map String Expr

type Ident = String

data Program = Program StmtList
	deriving(Show, Eq)

type StmtList = [Stmt]

data Func = Func Environment Behaviour

type Environment = DataStore
type Behaviour = Stmt

data Stmt 
	= Decl Decl 
	deriving(Show, Eq)

data Decl where 
	VarDecl 	:: Ident -> Expr -> Decl
	LambdaDecl  :: Ident -> BinderList -> Expr -> Decl
	FuncDecl 	:: Ident -> ParamList -> Expr -> Decl
	deriving(Show, Eq)

type BinderList = [String]

type ParamList = [String]

data Expr where
	TypeInt :: Int -> Expr
	TypeBool :: Bool -> Expr
	TypeList :: [Expr] -> Expr
	GetV :: Ident -> Expr
	LetExp :: Decl -> Expr -> Expr
	AddOp :: Expr -> Expr -> Expr
	SubOp :: Expr -> Expr -> Expr
	AndOp :: Expr -> Expr -> Expr
	deriving(Show, Eq)



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
