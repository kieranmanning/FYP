module Test where

data Bintree a
    = Fork (Bintree a) (Bintree a)
    | Leaf a

data Agnostic f a = MkA (f a)

identityFunction :: x -> x
identityFunction aparam = aparam

letFunction aparam = 
    let xletparam = 1337 in 
        xletparam * aparam

nonFunction = 2

main = do
    print "ignore"
