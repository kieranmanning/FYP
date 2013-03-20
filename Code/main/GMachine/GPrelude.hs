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
	[("main", [], (
		(EAp (EVar "Id") 
				(EAp (EVar "Id2") (ENum 21)))
		))
	]


--underSaturatedTest :: CoreProgram
--underSaturatedTest =
--	[("main", [], (EAp (EVar "Id") ()))]