{
{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification  #-}

module Main where
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
{- import qualified Data.Map as Map -}
import Data.Map


}

%name lang
%tokentype { Token }
%error { parseError }

%token 
		{- 	Primitives					-}
		int             { TokenInt $$ }
		True 			{ TokenTrue }
		False			{ TokenFalse }
		var 			{ TokenVar $$ }
		{-	Program Control				-}
		';'				{ TokenSeq }
		"let"			{ TokenLet }
		"in" 			{ TokenIn }
		{-	Type Denotation				-}
		'\\'			{ TokenLambdaOpen }
		"->"			{ TokenLambdaIs }
		'_'				{ TokenUnderscore }
		{-	List Denotation 			-}
		'['				{ TokenOpenList }
		']'				{ TokenCloseList }
		','				{ TokenSeperator }
		{-  Integer Arithmetic			-}
		'+'				{ TokenAddOp }
		'-'  			{ TokenSubOp }
		{-	Boolean Arithmetic			-}		
		'&' 			{ TokenAnd }
		'='				{ TokenAss }
		{-	List Operations				-}
		':'				{ TokenConsList }
		{- 	Misc						-}
		"print"			{ TokenPrint }

%%

Program : StmtList 						{ Program $1 }

StmtList : {- Empty -}					{ [] }
		 | Stmt 						{ [$1] }
		 | StmtList Stmt 				{ $2 : $1 }

Stmt : Decl 							{ Decl $1 }
	 | "print" Expr 					{ Print $1 }

Decl : var '=' Expr						{ VarDecl $1 $3 }
	 | var ParamList '=' Expr			{ FuncDecl $1 $2 $4 }
	 | var '=' '\\'  
	   BinderList "->" Expr				{ LambdaDecl $1 $4 $6 }

BinderList 	: '_'						{ [] }
			| var 						{ [$1] }
		   	| BinderList ',' var 		{ $3 : $1 }

ParamList 	: {- Empty -}				{ [] }
			| var 						{ [$1] }
			| ParamList var 			{ $2 : $1 }

Expr : int 								{ TypeInt $1 }
	 | var 								{ GetV $1 }
	 | True 							{ TypeBool True }
	 | False 							{ TypeBool False }
	 | '[' ListContents ']'				{ TypeList $2 }
	 | "let" Decl "in" Expr				{ LetExp $2 $4 }
	 | var '(' InitParamList ')'		{ FuncAppl $1 $3 }
	 | Expr '+' Expr					{ AddOp $1 $3 }
	 | Expr '-' Expr 					{ SubOp $1 $3 }
	 | Expr '&' Expr 					{ AndOp $1 $3 }

InitParamList 	: {- Empty -}				{ [] }
				| Expr 						{ [$1] }
				| ParamList ',' Expr 		{ $3 : $1 }

ListContents : {- Empty -}				{ [] }
			 | Expr 					{ [$1] }
			 | ListContents ',' Expr   	{ $3 : $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type SymbolTable = Data.Map Ident Expr

type Ident = String

data Program = Program StmtList
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
	FuncAppl :: Var -> [Expr] -> Expr
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
		(VarDecl i e) 		-> evalVarDecl(VarDecl i expr)
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
		LetExp x 	-> evalLetExp		(LetExp x)
		FuncAppl x 	-> evalFuncAppl		(FuncAppl x)
		AddOp x 	-> evalExprIntOp	(AddOp x)
		SubOp x 	-> evalExprIntOp	(SubOp x)
		AndOp x 	-> evalExprBoolOp	(AndOp x)


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
		_ 			-> ExprError
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
		_ 			-> ExprError
	where eval x = do 
		case x of 
			TypeBool x 	-> x
			AndOp x y 	-> (eval x) && (eval y)



data Token
	{- 	Primitives					-}
	  = TokenInt Int
	  | TokenTrue 
	  | TokenFalse
	  | TokenVar String
	{-	Program Control				-}
	  | TokenSeq
	  | TokenAss
	  | TokenLet 
	  | TokenIn
	{-	Type Denotation				-}
	  | TokenLambdaOpen
	  | TokenLambdaIs
	  |	TokenUnderscore
	{-	List Denotation 			-}
	  | TokenOpenList
	  | TokenCloseList
	  | TokenSeperator
	{-  Integer Arithmetic			-}
	  | TokenAddOp
	  | TokenSubOp
	{-	Boolean Arithmetic			-}		
	  | TokenAnd
	{-	List Operations				-}
	  | TokenConsList

 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer(c:cs)
	| isSpace c = lexer cs
	| isDigit c = lexNum(c:cs)
	| isAlpha c = lexVar(c:cs)
{-	Program Control				-}
lexer(';':cs) = TokenSeq : lexer cs
lexer('=':cs) = TokenAss : lexer cs
{-	Type Denotation				-}
lexer('\\':cs) = TokenLambdaOpen : lexer cs
lexer('-':'>':cs) = TokenLambdaIs : lexer cs
lexer('_':cs) = TokenUnderscore : lexer cs
{-	List Denotation 			-}
lexer('[':cs) = TokenOpenList : lexer cs
lexer(']':cs) = TokenCloseList : lexer cs
lexer(',':cs) = TokenSeperator : lexer cs
{-  Integer Arithmetic			-}
lexer('+':cs) = TokenAddOp : lexer cs
lexer('-':cs) = TokenSubOp : lexer cs
{-	Boolean Arithmetic			-}		
lexer('&':cs) = TokenAnd : lexer cs
{-	List Operations				-}
lexer(':':cs) = TokenConsList : lexer cs


lexNum cs = TokenInt (read num) : lexer rest
	  where (num, rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
	  ("true",rest) -> TokenTrue : lexer rest
	  ("false", rest) -> TokenFalse : lexer rest
	  ("let", rest)	-> TokenLet : lexer rest
	  ("in", rest) -> TokenIn : lexer rest
	  (var, rest)	-> TokenVar var : lexer rest


lang :: [Token] -> Program

main = getContents >>= print . lang . lexer 


}