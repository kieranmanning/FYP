module Test where

f b = b'
    where
        b' | b = 1
           | otherwise = 0
