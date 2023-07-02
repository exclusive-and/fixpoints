
module Nat where

import CoInductive


-- * Standard Natural Numbers
---------------------------------------------------------------------

data Nat = Zero | Succ Nat

-- |
-- Ordinary induction/recursion on the standard natural numbers.
-- 
natrec :: x -> (x -> x) -> Nat -> x
natrec base ind = \case
    Zero    -> base
    Succ n  -> ind (natrec base ind n)


-- * Least Fixed Point Example: Natural Numbers
---------------------------------------------------------------------

type MuNat = Mu Maybe

-- |
-- Induction/recursion on the fixed point natural numbers.
-- 
muNatrec :: x -> (x -> x) -> MuNat -> x
muNatrec base ind (Mu cata) = cata (maybe base ind)

-- |
-- The standard naturals are equivalent to the fixed point 'MuNat'.
-- 
--  - @'Nat' -> 'MuNat'@: provided by this function.
--  
--  - @'MuNat' -> 'Nat'@: provided by @'muNatrec' Zero Succ@.
-- 
natToMuNat :: Nat -> MuNat
natToMuNat n = Mu (go n)
    where
    go :: Nat -> (forall a. (Maybe a -> a) -> a)

    go m = case m of
        Zero    -> \f -> f Nothing
        Succ m' -> \f -> f (Just $ go m' f)
