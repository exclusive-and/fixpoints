
module Nat where

import CoInductive


-- * Inductive Algebra Example: Natural Numbers

data Nat = Zero | Succ Nat

natrec :: x -> (x -> x) -> Nat -> x
natrec base ind = \case
    Zero    -> base
    Succ n  -> ind (natrec base ind n)

type MuNat = Mu Maybe

muNatrec :: x -> (x -> x) -> MuNat -> x
muNatrec base ind (Mu cata) = cata (maybe base ind)

natToMuNat :: Nat -> MuNat
natToMuNat n = Mu (go n)
    where
    go :: Nat -> (forall a. (Maybe a -> a) -> a)

    go m = case m of
        Zero    -> \f -> f Nothing
        Succ m' -> \f -> f (Just $ go m' f)
