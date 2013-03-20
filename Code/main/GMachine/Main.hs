module Main where
import GEval

-- runProg :: [Char] -> [Char]

--parse :: [Char] -> CoreProgram

compile :: CoreProgram -> TiState

eval :: TiState -> [TiState]

-- showResults :: [TiState] -> [Char]

-- parsing is now our concern right now 
-- runProg = showResults . eval . compile . parse 

runProg = showResults . eval . compile 

