module MNAME where

data BTree = Leaf Int | Branch BTree BTree

y = [1,2,3]

f :: BTree -> Int
f (Leaf a) = a
f (Branch x y) = (f x) + (f y)

main = do
    print y
