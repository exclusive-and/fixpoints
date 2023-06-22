
module CoNat where

import CoInductive


data CoNat = Zero | Succ CoNat | Inf

nuCoNatrec :: x -> (x -> x) -> Nu Maybe -> x
nuCoNatrec base ind n = case outNu n of
    Just n' -> ind (nuCoNatrec base ind n')
    Nothing -> base
    
coNatToNuCoNat :: CoNat -> Nu Maybe
coNatToNuCoNat = Nu go
    where
    go n = case n of
        Zero    -> Nothing
        Succ n' -> Just n'
        Inf     -> Just Inf
        
