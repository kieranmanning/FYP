{- TODO: Shoot yourself for forgetting this -}

module Blop where
import Control.Monad.State
import qualified Data.Map as Map

type DataStore = Map.Map String Int

type Result a = State DataStore a

set :: (String, Int) -> Result ()
set (s,i) = modify $ (\table -> Map.insert s i table)
