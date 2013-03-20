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
		'['				{ TokenOL }
		']'				{ TokenCL } 

%%

Expr :: int 					{ $1 }
	  | True 					{ True }
	  | False  					{ False }
	  | '[' ListContents ']'    { $2 }


ListContents : {- Empty -}				{ [] }
			 | Expr 					{ [$1] }
			 | ListContents ',' Expr   	{ $3 : $1 }


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expr where
	Int :: 

