module MIdent where


fac :: Int -> Int
fac 0 = 1
fac x = x * (fac x - 1)

main = fac 4
