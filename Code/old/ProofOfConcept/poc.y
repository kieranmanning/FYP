{

{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification  #-}

module Main where
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)

{-- Proof of concept --}

}

%name lang
%tokentype { Token }
%error { parseError }

%token 
		{- 	Primitives					-}
		int             { TokenInt $$ }
		True 			{ TokenTrue }
		False			{ TokenFalse }
		{-  Integer Arithmetic			-}
		'+'				{ TokenAddOp }
		'-'  			{ TokenSubOp }
		{-	Boolean Arithmetic			-}		
		'&' 			{ TokenAnd }
		printf			{ TokenPrint }

%%

LangProgram 	: StmtList 					{ LangProgram $1 }

StmtList 		:: { [Stmt] }
		 		: {- Empty -}				{ [] }
		 		| Stmt 						{ [$1] }
		 		| StmtList Stmt 			{ $2 : $1 }

Stmt 			:: { Stmt }
	 			: printf Expr 				{ Print $2 }

Expr 			:: { Expr }
				: int 						{ TypeInt $1 }
				| True 						{ TypeBool True }
				| False 					{ TypeBool False }
				| Expr '+' Expr				{ AddOp $1 $3 }
				| Expr '-' Expr 			{ SubOp $1 $3 }
				| Expr '&' Expr 			{ AndOp $1 $3 }

{

type Ident = String 

data LangProgram = LangProgram StmtList
	deriving(Show, Eq)

type StmtList = [Stmt]

data Stmt where
	Print :: Expr -> Stmt
	deriving(Show, Eq)

data Expr where
	TypeInt :: Int -> Expr
	TypeBool :: Bool -> Expr	
	AddOp :: Expr -> Expr -> Expr
	AndOp :: Expr -> Expr -> Expr
	SubOp :: Expr -> Expr -> Expr
	ExprError :: Expr
	deriving(Show, Eq)


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
	= TokenInt Int
	| TokenTrue
	| TokenBool
	| TokenFalse
	| TokenAddOp
	| TokenSubOp
	| TokenAnd
	| TokenPrint
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer(c:cs)
	| isSpace c = lexer cs
	| isDigit c = lexNum(c:cs)
	| isAlpha c = lexVar(c:cs)
{-  Integer Arithmetic			-}
lexer('+':cs) = TokenAddOp : lexer cs
lexer('-':cs) = TokenSubOp : lexer cs
{-	Boolean Arithmetic			-}		
lexer('&':cs) = TokenAnd : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
	  where (num, rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
	  ("true",rest) -> TokenTrue : lexer rest
	  ("false", rest) -> TokenFalse : lexer rest
	  ("printf", rest) -> TokenPrint : lexer rest

lang :: [Token] -> LangProgram

main = getContents >>= print . lang . lexer 

}