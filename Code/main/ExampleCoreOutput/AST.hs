module AST where

"""
Reference to 'Implementing Funtional Languages: A Tutorial'
by SPJ and David R Lester when writing report
"""

type Name = String
type IsRec = Bool

bindersOf :: [(a,b)] -> [a]
bindersOf defns = map fst defns

rhssOf :: [(a,b)] -> [b]
rhssOf defns = map snd defns

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

isAtomicExpr :: Expr a 	-> Bool
isAtomicExpr (EVar v) 	= True
isAtomicExpr (ENum n) 	= True
isAtomicExpr e 			= False

type Program a = [ScDefn a]
type CoreProgram = Program Name

"""
Should definitely talk about [Super]Combinators
in background section of report. Link to haskell
wiki for further info
"""

{- Super Combinator definition -}
{-	(Name, Args, Body) -}
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

{- Defintion of a core Expr -}
type CoreExpr = Expr Name

data Expr a 
	= EVar Name
	| ENum Int
	| EConstr Int Int
	| EAp (Expr a) (Expr a)
	| ELet 
		IsRec
		[(a, Expr a)]
		(Expr a)
	| ECase
		(Expr a)
		[Alter a]
	| ELam [a] (Expr a)
	deriving(Show)


pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2