
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}

module CoInductive where


-- * (Co-)Inductive Fixed Point Types

-- |
-- Least fixed point of a functor.
--
-- Evidence for this type is a catamorphism, equivalent to a
-- fold or recursor.
-- 
-- Programmatically, this type allows induction.
-- 
-- Categorically, this is the type of initial @f@-algebras.
--
data Mu f = Mu (forall a. (f a -> a) -> a)


-- |
-- Greatest fixed point of a functor.
--
-- Evidence for this type is a point of convergence (possibly at
-- infinity), along with an anamorphism for unfolding it.
-- 
-- Programmatically, this type allows co-induction.
-- 
-- Categorically, this is the type of final @f@-coalgebras.
--
data Nu f = forall a. Nu (a -> f a) a


-- * Categorical Morphisms

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


