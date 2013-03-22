module GPrelude where
import GADT

---------------------------------------------------------------------------------
-- Prelude and tests and such.
---------------------------------------------------------------------------------

--test = eval ( compile ( idTest ) )

preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x"),	-- Identity, here we come...
	 ("Id2", ["y"], EVar "y"),
	 ("K", ["x", "y"], EVar "y")]

kTest :: CoreProgram
kTest = 
	[("main", [], (EAp (EAp (EVar "K") (ENum 1)) (ENum 2)))]

--underSaturatedTest :: CoreProgram
--underSaturatedTest =
--	[("main", [], (EAp (EVar "Id") ()))]