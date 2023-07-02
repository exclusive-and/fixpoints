
module CoAlgebra where


-- * Mu - Least Fixed Point
---------------------------------------------------------------------

-- |
-- The least fixed point of @f@ by Lambek's lemma.
--
data Mu f = Mu (forall x. (f x -> x) -> x)

-- |
-- @'Mu' f@ is algebraic (equivalently @f@-closed).
--
intoMu :: Functor f => f (Mu f) -> Mu f
intoMu x = Mu go
    where
    go f = f (fmap (foldMu f) x)

-- |
-- @'Mu' f@ is initial: given an algebraic type @(x, f x -> x)@,
-- there is a map @'foldMu' f :: 'Mu' f -> x@.
--
foldMu :: Functor f => (f x -> x) -> Mu f -> x
foldMu f (Mu cata) = cata f


-- * Nu - Greatest Fixed Point
---------------------------------------------------------------------

-- |
-- The greatest fixed point of @f@ by Lambek's lemma.
--
data Nu f = forall x. Nu (x -> f x) x

-- |
-- @'Nu' f@ is coalgebraic (equivalently @f@-consistent).
--
outOfNu :: Functor f => Nu f -> f (Nu f)
outOfNu (Nu f x) = fmap (Nu f) (f x)

-- |
-- @'Nu' f@ is terminal: given a coalgebraic type @(x, x -> f x)@,
-- there is a map @'unfoldNu' f :: x -> 'Nu' f@.
--
unfoldNu :: Functor f => (x -> f x) -> x -> Nu f
unfoldNu = Nu


-- * Categorical Properties
---------------------------------------------------------------------

refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = go
    where
    go x = f (fmap go (g x))

