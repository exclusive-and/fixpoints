
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}

module CoInductive where


-- * Mu - Least Fixed Point
---------------------------------------------------------------------

-- |
-- @'Mu' f@ is an initial @f@-algebra because it is:
--
--  - Initial: for any algebraic type @(x, f x -> x)@, there is
--    a map @'foldMu' f :: 'Mu' f -> x@.
--
--  - Algebraic or @f@-closed: there is a unique morphism
--    @'inMu' :: f ('Mu' f) -> 'Mu' f@.
--
-- By Lambek's lemma, @'Mu' f@ is the least fixed point of @f@.
--
data Mu f = Mu (forall a. (f a -> a) -> a)

-- |
-- The morphism that gives @'Mu' f@ its initiality.
--
foldMu :: (f a -> a) -> Mu f -> a
foldMu f (Mu cata) = cata f

-- |
--
--
intoMu :: Functor f => f (Mu f) -> Mu f
intoMu x = Mu $ \f -> f (fmap (foldMu f) x)


-- * Nu - Greatest Fixed Point
---------------------------------------------------------------------

-- |
-- @'Nu' f@ is a terminal @f@-coalegbra because it is:
--
--  - Final: for any coalgebraic type @(x, x -> f x)@, there is
--    a map @'Nu' out :: x -> 'Nu' f@.
--
--  - Coalgebraic or @f@-consistent: there is a unique morphism
--    @'outOfNu' :: 'Nu' f -> f ('Nu' f)@.
--
-- By Lambek's lemma, @'Nu' f@ is the greatest fixed point of @f@.
--
data Nu f = forall a. Nu (a -> f a) a

-- |
-- The unique morphism of a terminal @f@-coalgebra.
--
outOfNu :: Functor f => Nu f -> f (Nu f)
outOfNu (Nu f x) = fmap (Nu f) (f x)


-- * Categorical Properties
---------------------------------------------------------------------

refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = go
    where
    go x = f (fmap go (g x))

