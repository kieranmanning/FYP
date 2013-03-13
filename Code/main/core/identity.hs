module Main where

--can ignore type sigs, will
--be inferred as forall a.
--a -> a regardless.
identFunc a = 2 * a

main = do
    print $ identFunc 2
