{-# LANGUAGE GADTs, StandaloneDeriving, FlexibleInstances, FlexibleContexts, ExistentialQuantification  #-}

module Main where

data Stmt 
	= Print Expr
	deriving(Show, Eq)

data Expr where
	TypeInt :: Int -> Expr
	TypeBool :: Bool -> Expr
	AddOp :: Expr -> Expr -> Expr
	SubOp :: Expr -> Expr -> Expr
	AndOp :: Expr -> Expr -> Expr
	deriving(Show, Eq)

test :: IO ()
test = do 
	print $ show $ (Print (AddOp (TypeInt 1) (TypeInt 2)))

main = do test