{
{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification  #-}

module Main where
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)

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

%%

Program : StmtList 						{ Program $1 }

StmtList : {- Empty -}					{ [] }
		 | Stmt 						{ [$1] }
		 | StmtList Stmt 				{ $2 : $1 }

Stmt : Decl 							{ Decl $1 }

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
	 | Expr '+' Expr					{ AddOp $1 $3 }
	 | Expr '-' Expr 					{ SubOp $1 $3 }
	 | Expr '&' Expr 					{ AndOp $1 $3 }

ListContents : {- Empty -}				{ [] }
			 | Expr 					{ [$1] }
			 | ListContents ',' Expr   	{ $3 : $1 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type Ident = String

data Program = Program StmtList
	deriving(Show, Eq)

type StmtList = [Stmt]

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
	GetV :: Ident -> Expr
	TypeBool :: Bool -> Expr
	TypeList :: [Expr] -> Expr
	LetExp :: Decl -> Expr -> Expr
	AddOp :: Expr -> Expr -> Expr
	SubOp :: Expr -> Expr -> Expr
	AndOp :: Expr -> Expr -> Expr
	deriving(Show, Eq)

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