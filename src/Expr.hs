
module Expr where

import CoInductive


data Op lit a = Lit lit | Add a a | Sub a a | Mul a a

type Expr lit = Mu (Op lit)

eval :: Num lit => Expr lit -> lit
eval = foldMu go
    where
    go op = case op of
        Lit lit -> lit
        Add a b -> a + b
        Sub a b -> a - b
        Mul a b -> a * b
