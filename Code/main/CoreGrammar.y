{

{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification  #-}

module Main where
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
import AST.hs

"""

"""


}

%name core
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