module Main where

nonFunction x y = x + y

lambdaFunc = \x -> \y -> (nonFunction x y)

main = do
    print $ lambdaFunc 2 1
