
module CoNat where

import CoAlgebra


-- * Standard Co-Natural Numbers
---------------------------------------------------------------------

-- |
-- The co-natural numbers are like the natural numbers, with
-- an extra number 'Inf' that represents infinity.
-- 
data CoNat = Zero | Succ CoNat | Inf

-- |
-- Ordinary co-induction/co-recursion on the co-natural numbers.
-- 
coNatrec :: x -> (x -> x) -> CoNat -> x
coNatrec base ind n = case n of
    Zero    -> base
    Succ n' -> ind (coNatrec base ind n')
    Inf     -> ind (coNatrec base ind n)


-- * Greatest Fixed Point Example: Co-Natural Numbers
---------------------------------------------------------------------

-- |
-- @'Nu' 'Maybe'@ is equivalent to @(x, x -> Maybe x)@, which
-- is the set of types that have a function which can choose
-- to continue if the function returns @'Just' x@, or stop if it
-- returns 'Nothing'.
-- 
-- This supports the ordinary naturals, since we can model @n@ by
-- taking @n@ steps and then stopping.
-- 
-- We can also model 'Inf' by never stopping.
-- 
type NuCoNat = Nu Maybe

-- |
-- Co-induction/co-recursion on the fixed point co-natural numbers.
-- 
nuCoNatrec :: x -> (x -> x) -> NuCoNat -> x
nuCoNatrec base ind n = case outOfNu n of
    Just n' -> ind (nuCoNatrec base ind n')
    Nothing -> base
    
coNatToNuCoNat :: CoNat -> NuCoNat
coNatToNuCoNat = Nu go
    where
    go n = case n of
        Zero    -> Nothing
        Succ n' -> Just n'
        Inf     -> Just Inf
        
