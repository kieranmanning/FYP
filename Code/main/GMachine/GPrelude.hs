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

kTest :: CoreProgram
kTest = 
	[("K", ["x", "y"], EVar "x"),
	("main", [], (EAp (EAp (EVar "K") (ENum 1)) (ENum 9)))]

preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x")]	-- Identity, here we come...

idTest :: CoreProgram
idTest =
	[("main", [], (EAp (EVar "Id") (EAp (EVar "Id")(ENum 1))))]

arithTest :: CoreProgram
arithTest = 
	[("main", [], EAp (EAp (EVar "+") (ENum 5)) (ENum 2))]

negTest :: CoreProgram
negTest =
	[("main", [], EAp (EVar "neg") (ENum 1))]

recTest :: CoreProgram
recTest = 
	[("rec", ["x"], EAp (EVar "rec") (EAp (EVar "+") (EVar "x"))),
	 ("main", [], EAp (EVar "rec") (ENum 9))]

abTest :: CoreProgram
abTest = 
	[("xeq0", ["x"], EAp(EAp(EVar"==")(ENum 0))(EVar "x")),
	 ("rec", ["x"], EAp(EAp(EAp(EVar "if")cond)(ENum 0))e2),
	 ("main", [], EAp(EVar "rec")(ENum 3))]
	 where
	 	e2 = (EAp(EVar "rec")(EAp(EAp(EVar "-")(EVar "x"))(ENum 1)))
	 	cond = (EAp(EVar "xeq0")(EVar "x"))

subTest :: CoreProgram
subTest = 
	[("main", [], EAp(EAp(EVar"-")(ENum 1))(ENum 7))]

ifTest :: CoreProgram
ifTest = 
	[("xm1", ["x"], EAp (EAp (EVar "-") (EVar "x")) (ENum 1)),
	 ("xeq1", ["x"], EAp(EAp(EVar "==")(EVar "x"))(ENum 1)),
	 ("rec", ["x"], EAp(EAp(EAp(EVar "if")(EVar "xeq1"))(ENum 1))(ENum 2)),
	 ("main", [], EAp(EVar "rec")(ENum 1))]

if2Test :: CoreProgram
if2Test = 
	[("main", [], EAp(EAp( EAp(EVar "if")(EAp(EAp(EVar "==")(ENum 1))(ENum 0)) )(ENum 1))(ENum 2))]