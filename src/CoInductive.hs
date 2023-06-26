
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}

module CoInductive where


-- * Mu - Least Fixed Point
---------------------------------------------------------------------

-- |
-- @'Mu' f@ is the least fixed point of @f@ by Lambek's lemma.
--
data Mu f = Mu (forall a. (f a -> a) -> a)

-- |
-- @'Mu' f@ is algebraic or @f@-closed.
--
intoMu :: Functor f => f (Mu f) -> Mu f
intoMu x = Mu $ \f -> f (fmap (foldMu f) x)

-- |
-- @'Mu' f@ is initial: given an algebraic type @(x, f x -> x)@,
-- there is a map @'foldMu' f :: 'Mu' f -> x@.
--
foldMu :: (f a -> a) -> Mu f -> a
foldMu f (Mu cata) = cata f


-- * Nu - Greatest Fixed Point
---------------------------------------------------------------------

-- |
-- @'Nu' f@ is the greatest fixed point of @f@ by Lambek's lemma.
--
data Nu f = forall a. Nu (a -> f a) a

-- |
-- @'Nu' f@ is coalgebraic of @f@-consistent
--
outOfNu :: Functor f => Nu f -> f (Nu f)
outOfNu (Nu f x) = fmap (Nu f) (f x)

-- |
-- @'Nu' f@ is terminal: given a coalgebraic type @(x, x -> f x)@,
-- there is a map @'unfoldNu' f :: x -> 'Nu' f@.
--
unfoldNu :: (x -> f x) -> x -> Nu f
unfoldNu = Nu


-- * Categorical Properties
---------------------------------------------------------------------

refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = go
    where
    go x = f (fmap go (g x))

