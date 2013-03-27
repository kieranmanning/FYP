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

kTest :: CoreProgram
kTest = 
	[("main", [], (EAp (EAp (EVar "K") (ENum 1)) (ENum 2)))]
-}

preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x")]	-- Identity, here we come...


idTest :: CoreProgram
idTest =
	[("main", [], (EAp (EVar "Id") (ENum 1)))]

arithTest :: CoreProgram
arithTest = 
	[("main", [], EAp (EAp (EVar "+") (ENum 5)) (ENum 2))]

negTest :: CoreProgram
negTest =
	[("main", [], EAp (EVar "neg") (ENum 1))]
