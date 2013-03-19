{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification  #-}

module Main where
import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)
{- import qualified Data.Map as Map -}
import Data.Map
import LangDSL

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 ([Stmt])
	| HappyAbsSyn6 (Stmt)
	| HappyAbsSyn7 (Decl)
	| HappyAbsSyn8 ([String])
	| HappyAbsSyn10 (Expr)
	| HappyAbsSyn11 ([Expr])

action_0 (16) = happyShift action_5
action_0 (4) = happyGoto action_6
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 _ = happyReduce_2

action_1 (16) = happyShift action_5
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyFail

action_2 (16) = happyShift action_5
action_2 (6) = happyGoto action_10
action_2 (7) = happyGoto action_4
action_2 _ = happyReduce_1

action_3 _ = happyReduce_3

action_4 _ = happyReduce_5

action_5 (16) = happyShift action_8
action_5 (31) = happyShift action_9
action_5 (9) = happyGoto action_7
action_5 _ = happyFail

action_6 (34) = happyAccept
action_6 _ = happyFail

action_7 (16) = happyShift action_19
action_7 (31) = happyShift action_20
action_7 _ = happyFail

action_8 _ = happyReduce_13

action_9 (13) = happyShift action_12
action_9 (14) = happyShift action_13
action_9 (15) = happyShift action_14
action_9 (16) = happyShift action_15
action_9 (18) = happyShift action_16
action_9 (20) = happyShift action_17
action_9 (23) = happyShift action_18
action_9 (10) = happyGoto action_11
action_9 _ = happyFail

action_10 _ = happyReduce_4

action_11 (28) = happyShift action_29
action_11 (29) = happyShift action_30
action_11 (30) = happyShift action_31
action_11 _ = happyReduce_6

action_12 _ = happyReduce_15

action_13 _ = happyReduce_17

action_14 _ = happyReduce_18

action_15 (26) = happyShift action_28
action_15 _ = happyReduce_16

action_16 (16) = happyShift action_5
action_16 (7) = happyGoto action_27
action_16 _ = happyFail

action_17 (16) = happyShift action_25
action_17 (22) = happyShift action_26
action_17 (8) = happyGoto action_24
action_17 _ = happyFail

action_18 (13) = happyShift action_12
action_18 (14) = happyShift action_13
action_18 (15) = happyShift action_14
action_18 (16) = happyShift action_15
action_18 (18) = happyShift action_16
action_18 (23) = happyShift action_18
action_18 (10) = happyGoto action_22
action_18 (12) = happyGoto action_23
action_18 _ = happyReduce_28

action_19 _ = happyReduce_14

action_20 (13) = happyShift action_12
action_20 (14) = happyShift action_13
action_20 (15) = happyShift action_14
action_20 (16) = happyShift action_15
action_20 (18) = happyShift action_16
action_20 (23) = happyShift action_18
action_20 (10) = happyGoto action_21
action_20 _ = happyFail

action_21 (28) = happyShift action_29
action_21 (29) = happyShift action_30
action_21 (30) = happyShift action_31
action_21 _ = happyReduce_7

action_22 (28) = happyShift action_29
action_22 (29) = happyShift action_30
action_22 (30) = happyShift action_31
action_22 _ = happyReduce_29

action_23 (24) = happyShift action_40
action_23 (25) = happyShift action_41
action_23 _ = happyFail

action_24 (21) = happyShift action_38
action_24 (25) = happyShift action_39
action_24 _ = happyFail

action_25 _ = happyReduce_10

action_26 _ = happyReduce_9

action_27 (19) = happyShift action_37
action_27 _ = happyFail

action_28 (13) = happyShift action_12
action_28 (14) = happyShift action_13
action_28 (15) = happyShift action_14
action_28 (16) = happyShift action_15
action_28 (18) = happyShift action_16
action_28 (23) = happyShift action_18
action_28 (10) = happyGoto action_35
action_28 (11) = happyGoto action_36
action_28 _ = happyReduce_25

action_29 (13) = happyShift action_12
action_29 (14) = happyShift action_13
action_29 (15) = happyShift action_14
action_29 (16) = happyShift action_15
action_29 (18) = happyShift action_16
action_29 (23) = happyShift action_18
action_29 (10) = happyGoto action_34
action_29 _ = happyFail

action_30 (13) = happyShift action_12
action_30 (14) = happyShift action_13
action_30 (15) = happyShift action_14
action_30 (16) = happyShift action_15
action_30 (18) = happyShift action_16
action_30 (23) = happyShift action_18
action_30 (10) = happyGoto action_33
action_30 _ = happyFail

action_31 (13) = happyShift action_12
action_31 (14) = happyShift action_13
action_31 (15) = happyShift action_14
action_31 (16) = happyShift action_15
action_31 (18) = happyShift action_16
action_31 (23) = happyShift action_18
action_31 (10) = happyGoto action_32
action_31 _ = happyFail

action_32 (28) = happyShift action_29
action_32 (29) = happyShift action_30
action_32 (30) = happyShift action_31
action_32 _ = happyReduce_24

action_33 (28) = happyShift action_29
action_33 (29) = happyShift action_30
action_33 (30) = happyShift action_31
action_33 _ = happyReduce_23

action_34 (28) = happyShift action_29
action_34 (29) = happyShift action_30
action_34 (30) = happyShift action_31
action_34 _ = happyReduce_22

action_35 (28) = happyShift action_29
action_35 (29) = happyShift action_30
action_35 (30) = happyShift action_31
action_35 _ = happyReduce_26

action_36 (25) = happyShift action_46
action_36 (27) = happyShift action_47
action_36 _ = happyFail

action_37 (13) = happyShift action_12
action_37 (14) = happyShift action_13
action_37 (15) = happyShift action_14
action_37 (16) = happyShift action_15
action_37 (18) = happyShift action_16
action_37 (23) = happyShift action_18
action_37 (10) = happyGoto action_45
action_37 _ = happyFail

action_38 (13) = happyShift action_12
action_38 (14) = happyShift action_13
action_38 (15) = happyShift action_14
action_38 (16) = happyShift action_15
action_38 (18) = happyShift action_16
action_38 (23) = happyShift action_18
action_38 (10) = happyGoto action_44
action_38 _ = happyFail

action_39 (16) = happyShift action_43
action_39 _ = happyFail

action_40 _ = happyReduce_19

action_41 (13) = happyShift action_12
action_41 (14) = happyShift action_13
action_41 (15) = happyShift action_14
action_41 (16) = happyShift action_15
action_41 (18) = happyShift action_16
action_41 (23) = happyShift action_18
action_41 (10) = happyGoto action_42
action_41 _ = happyFail

action_42 (28) = happyShift action_29
action_42 (29) = happyShift action_30
action_42 (30) = happyShift action_31
action_42 _ = happyReduce_30

action_43 _ = happyReduce_11

action_44 (28) = happyShift action_29
action_44 (29) = happyShift action_30
action_44 (30) = happyShift action_31
action_44 _ = happyReduce_8

action_45 (28) = happyShift action_29
action_45 (29) = happyShift action_30
action_45 (30) = happyShift action_31
action_45 _ = happyReduce_20

action_46 (13) = happyShift action_12
action_46 (14) = happyShift action_13
action_46 (15) = happyShift action_14
action_46 (16) = happyShift action_15
action_46 (18) = happyShift action_16
action_46 (23) = happyShift action_18
action_46 (10) = happyGoto action_48
action_46 _ = happyFail

action_47 _ = happyReduce_21

action_48 (28) = happyShift action_29
action_48 (29) = happyShift action_30
action_48 (30) = happyShift action_31
action_48 _ = happyReduce_27

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (LangProgram happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Decl happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn7
		 (VarDecl happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (FuncDecl happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (LambdaDecl happy_var_1 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 (HappyTerminal (TokenVar happy_var_3))
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3 : happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  9 happyReduction_12
happyReduction_12  =  HappyAbsSyn8
		 ([]
	)

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyTerminal (TokenVar happy_var_2))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn10
		 (TypeInt happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 (GetV happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn10
		 (TypeBool True
	)

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn10
		 (TypeBool False
	)

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TypeList happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 4 10 happyReduction_20
happyReduction_20 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (LetExp happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 10 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (FuncAppl happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (AddOp happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (SubOp happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (AndOp happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  11 happyReduction_25
happyReduction_25  =  HappyAbsSyn11
		 ([]
	)

happyReduce_26 = happySpecReduce_1  11 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  11 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  12 happyReduction_28
happyReduction_28  =  HappyAbsSyn11
		 ([]
	)

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  12 happyReduction_30
happyReduction_30 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 13;
	TokenTrue -> cont 14;
	TokenFalse -> cont 15;
	TokenVar happy_dollar_dollar -> cont 16;
	TokenSeq -> cont 17;
	TokenLet -> cont 18;
	TokenIn -> cont 19;
	TokenLambdaOpen -> cont 20;
	TokenLambdaIs -> cont 21;
	TokenUnderscore -> cont 22;
	TokenOpenList -> cont 23;
	TokenCloseList -> cont 24;
	TokenSeperator -> cont 25;
	TokenOpenParams -> cont 26;
	TokenCloseParams -> cont 27;
	TokenAddOp -> cont 28;
	TokenSubOp -> cont 29;
	TokenAnd -> cont 30;
	TokenAss -> cont 31;
	TokenConsList -> cont 32;
	TokenPrint happy_dollar_dollar -> cont 33;
	_ -> happyError' (tk:tks)
	}

happyError_ 34 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

lang tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
	{-	Func Denotation			-}
	  | TokenOpenParams
	  | TokenCloseParams
	{-  Integer Arithmetic			-}
	  | TokenAddOp
	  | TokenSubOp
	{-	Boolean Arithmetic			-}		
	  | TokenAnd
	{-	List Operations				-}
	  | TokenConsList
	{-	Misc						-}
	  | TokenPrint Expr

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
{- 	Func Denotation				-}
lexer('(':cs) = TokenOpenParams : lexer cs
lexer(')':cs) = TokenCloseParams : lexer cs
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


lang :: [Token] -> LangProgram

main = getContents >>= print . lang . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
