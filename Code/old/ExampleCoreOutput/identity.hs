module Main where

--can ignore type sigs, will
--be inferred as forall a.
--a -> a regardless.
identFunc :: Int -> Int
identFunc a =  a

main = do
    print $ identFunc 2
