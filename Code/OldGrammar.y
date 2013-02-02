{
{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts  #-}

module Grammar where
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import LangDSL

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
		{-	Type Denotation				-}
		"/"				{ TokenBackslash }
		"->"			{ TokenLambdaIs }
		{-	List Denotation 			-}
		'['				{ TokenOpenList }
		']'				{ TokenCloseList }
		','				{ TokenSeperator }
		{-  Integer Arithmetic			-}
		'+'				{ TokenAdd }
		{-	Boolean Arithmetic			-}		
		'&' 			{ TokenAnd }
		'='				{ TokenAss }
		{-	List Operations				-}
		':'				{ TokenConsList }

%%

Stmt 		: Stmt ';' Stmt 			{ Seq $1 $3 }
			| Decl 						{ }

Decl 		: var '=' int				{ Decl $1 $3 }

Term 		: Expr 						{ }
{-			| Factor 					{ } -}


Expr		: ExprInt					{ }

{-
			| ExprBool					{ }
			| ExprList 					{ }
-}

ExprInt		: ExprInt '+' ExprInt 		{ Add $1 $3 }
			| int
{-
ExprBool 	: ExprBool '&' ExprBool 	{ And $1 $3 }

ExprList 	: ExprList ':' ExprList		{ Con $1 $3 }

Lambda		: "/" Binders "->" Term 	{ Lambda $2 $4 }

Binders		: {- Empty -}				{ [] }
			| var ',' Binders			{ $3 : $1 }

List		: '[' ListConts ']'			{ List $2 }

ListConts 	: {- Empty -} 				{ [] }
			| Factor ',' ListConts		{ $3 : $1 }

-}

{-
Factor		: int 						{ N $1 }
			| var 						{ GetV $1}
			| true 						{ B True }
			| false 					{ B False }
			| List 						{ List }   
			| Lambda 					{ Lambda } 
-}
 

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
	{- 	Primitives					-}
	  = TokenInt Int
	  | TokenTrue 
	  | TokenFalse
	  | TokenVar String
	{-	Program Control				-}
	  | TokenSeq
	  | TokenAss
	{-	Type Denotation				-}
	  | TokenBackslash
	  | TokenLambdaIs
	{-	List Denotation 			-}
	  | TokenOpenList
	  | TokenCloseList
	  | TokenSeperator
	{-  Integer Arithmetic			-}
	  | TokenAdd
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
lexer('/':cs) = TokenBackslash : lexer cs
lexer('-':'>':cs) = TokenLambdaIs : lexer cs
{-	List Denotation 			-}
lexer('[':cs) = TokenOpenList : lexer cs
lexer(']':cs) = TokenCloseList : lexer cs
lexer(',':cs) = TokenSeperator : lexer cs
{-  Integer Arithmetic			-}
lexer('+':cs) = TokenAdd : lexer cs
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
	  (var, rest)	-> TokenVar var : lexer rest


lang :: [Token] -> Stmt Int

main = getContents >>= print . lang . lexer 


}