
module Expr where

import CoAlgebra


-- * Least Fixed Point Example: Numerical Expressions
---------------------------------------------------------------------

data Op lit x
    = Lit lit
    | Add x x
    | Sub x x
    | Mul x x

instance Functor (Op lit) where
    fmap f = \case
        Lit lit -> Lit lit
        Add a b -> Add (f a) (f b)
        Sub a b -> Sub (f a) (f b)
        Mul a b -> Mul (f a) (f b)

type Expr lit = Mu (Op lit)

eval :: Num lit => Expr lit -> lit
eval = foldMu go
    where
    go op = case op of
        Lit lit -> lit
        Add a b -> a + b
        Sub a b -> a - b
        Mul a b -> a * b
