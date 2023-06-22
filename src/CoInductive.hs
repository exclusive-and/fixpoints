
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}

module CoInductive where


-- * Mu - Smallest Fixed Point

-- |
-- @'Mu' f@ is the smallest fixed point of the functor @f@.
--
data Mu f = Mu (forall a. (f a -> a) -> a)


-- * Nu - Greatest Fixed Point

-- |
-- @'Nu' f@ is the greatest fixed point of the functor @f@.
--
data Nu f = forall a. Nu (a -> f a) a


-- * Categorical Properties

-- |
-- The unique morphism of an initial @f@-algebra which sends
-- @f a@ to @a@.
-- 
inMu :: Functor f => f (Mu f) -> Mu f
inMu x = Mu $ \f -> f (fmap (foldMu f) x)

foldMu :: (f a -> a) -> Mu f -> a
foldMu f (Mu cata) = cata f


-- |
-- The unique morphism of a terminal @f@-coalgebra which sends
-- @a@ to @f a@.
-- 
outNu :: Functor f => Nu f -> f (Nu f)
outNu (Nu f x) = fmap (Nu f) (f x)


refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = go
    where
    go x = f (fmap go (g x))


