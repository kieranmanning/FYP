module GPrelude where
import GADT

---------------------------------------------------------------------------------
-- Prelude and tests and such.
---------------------------------------------------------------------------------

--test = eval ( compile ( idTest ) )

preludeDefs :: CoreProgram
preludeDefs = 
	[("Id", ["x"], EVar "x"),	-- Identity, here we come...
	 ("Id2", ["y"], EVar "y")]

idTest :: CoreProgram
idTest = 
	[("Id", ["x"], EVar "x"),
	("main", [], (
		(EAp (EVar "Id") (ENum 21))))
	]

--underSaturatedTest :: CoreProgram
--underSaturatedTest =
--	[("main", [], (EAp (EVar "Id") ()))]