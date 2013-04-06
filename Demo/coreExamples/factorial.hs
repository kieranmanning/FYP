module MIdent where

fac 0 = 1
fac n = n * fac (n - 1)

main = fac 3
