module Javascript where

---------------------------------------------------------------------------------
-- Javascript. 
---------------------------------------------------------------------------------

{-	The general approach is to let the compiler reduce
 -	our functions as much as possible, then grab them
 -	from the heap via their tags and convert them to
 -	the most minimal JS possible. We're only on identity
 -	functions at the moment, so it'll take some more 
 -	work to deal with the Applications more complicated
 -	supercombinators (not to mention the whole "Rest 
 -	of the language") but we'll get to that
 -}

--runCore2JS :: CoreProgram -> String
runCore2JS prog = let x = eval(compile(prog)) in
	intercalate "\n" (map node2JS (interpret2JS(x)))

interpret2JS :: [TiState] -> [Node]
interpret2JS graphStates = deconState (last graphStates)

deconState :: TiState -> [Node] 
deconState (stack, dump, heap, globals, stats) =
	getNodesFromHeap heap

getNodesFromHeap :: TiHeap -> [Node]
getNodesFromHeap heap = 
	map (hLookup heap) (hAddresses heap)

node2JS :: Node -> String
node2JS (NNum x)	 		= " "
node2JS (NSuperComb s b e) 	= superComb2JS (NSuperComb s b e)
node2JS (NAp a1 a2)			= " "

superComb2JS :: Node -> String
superComb2JS (NSuperComb sId b e) =
	"function " 			++ 
	sId 					++ 
	scBinders2JSParams b 	++
	"{ \n" ++ expr2JS e  ++ " } \n"

scBinders2JSParams :: [Name] -> String
scBinders2JSParams b = "( " ++ (intercalate ", " b) ++ " )"

expr2JS :: Expr a -> String
expr2JS expr = do 
	case expr of 
		EVar name 	-> "\t return " ++ name ++ "; \n "
		ENum int 	-> "\t return " ++ (show int) ++ "; \n"
		EAp e1 e2 	-> "\t not tonight... \n"

compile :: CoreProgram -> GmState
compile program =
	(initialcode, [], heap, globals, statInitial)
	where
		(heap, globals) = buildIinitialHeap program
