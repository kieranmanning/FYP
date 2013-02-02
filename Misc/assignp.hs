{-# LANGUAGE GADTs #-}

module Alajkdf where

data Stmt a where
    Assign :: String -> b -> Stmt a


