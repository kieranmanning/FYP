module GPrelude where
import GADT

---------------------------------------------------------------------------------
-- Prelude and tests and such.
---------------------------------------------------------------------------------

--test = eval ( compile ( idTest ) )

{-
preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x"),	-- Identity, here we come...
	 ("Id2", ["y"], EVar "y"),
	 ("K", ["x", "y"], EVar "y")]
-}

preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x")]	-- Identity, here we come...

kTest :: CoreProgram
kTest =
	[("K", ["x", "y"], EVar "x"),
	 ("main", [], EAp (EAp (EVar "K") (ENum 1)) divbyzero)]
	where
		divbyzero = EAp (EAp (EVar "/") (ENum 1)) (ENum 0)

caseTest :: CoreProgram
caseTest = 
	[("main", [], ECase (EConstrAp 1 0 []) [(1, [], ENum 1), (2, [], ENum 2)])]

arithTest :: CoreProgram
arithTest = 
	[("main", [], EAp (EAp (EVar "+") (ENum 5)) (ENum 2))]

negTest :: CoreProgram
negTest =
	[("main", [], EAp (EVar "neg") (ENum 1))]

abTest :: CoreProgram
abTest = 
	[("xeq0", ["x"], EAp(EAp(EVar"==")(ENum 0))(EVar "x")),
	 ("rec", ["x"], EAp(EAp(EAp(EVar "if")cond)(ENum 0))e2),
	 ("main", [], EAp(EVar "rec")(ENum 2))]
	 where
	 	e2 = (EAp(EVar "rec")(EAp(EAp(EVar "-")(EVar "x"))(ENum 1)))
	 	cond = (EAp(EVar "xeq0")(EVar "x"))

subTest :: CoreProgram
subTest = 
	[("main", [], EAp(EAp(EVar"-")(ENum 1))(ENum 7))]

ifTest :: CoreProgram
ifTest = 
	[("main", [], EAp(EAp( EAp(EVar "if")(EAp(EAp(EVar "==")(ENum 1))(ENum 0)) )(ENum 1))(ENum 2))] 

facTest :: CoreProgram
facTest = [ ("xeq0", ["x"], EAp(EAp(EVar"==")(ENum 0))(EVar "x")),
            ("fac", ["n"], EAp(EAp(EAp(EVar "if")cond)(ENum 1))e2),
	    ("main", [], EAp(EVar "fac")(ENum 3))]
        where 
	 	cond = (EAp(EVar "xeq0")(EVar "n"))
                e2 = EAp (EAp (EVar "*") (EVar "n"))  e3 
	 	e3 = EAp (EVar "fac") (EAp (EAp (EVar "-") (EVar "n")) (ENum 1) )
