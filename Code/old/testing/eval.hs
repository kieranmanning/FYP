{-# LANGUAGE GADTs #-}

module Main where

data Expr where
    TypeBool :: Bool -> Expr
    TypeInt :: Int -> Expr
    Add :: Expr -> Expr -> Expr
    And :: Expr -> Expr -> Expr
    deriving(Show)

eval :: Expr -> Expr
eval expr = do
    case expr of
        TypeBool x -> evalBool(TypeBool x)
        And x y -> evalBool(And x y)
        TypeInt x -> evalInt(TypeInt x)
        Add x y -> evalInt(Add x y)
        

evalInt :: Expr -> Expr
evalInt expr = do
    case expr of
        TypeInt x ->  TypeInt x
        x -> TypeInt (eval x)
    where 
    eval x = do
        case x of 
            TypeInt x -> x
            Add x y -> (eval x) + (eval y)
                
                

evalBool :: Expr -> Expr
evalBool expr = do
    case expr of
        TypeBool x -> TypeBool x
        x -> TypeBool (eval x)
    where 
    eval x = do
        case x of 
            TypeBool x -> x
            And x y -> (eval x) && (eval y)

     
