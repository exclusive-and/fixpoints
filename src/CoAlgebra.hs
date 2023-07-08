
module CoAlgebra where


-- |
-- Evidence that @x ~ f x@ given a pair of inverses.
-- 
data Lambek f x = Lambek (x -> f x) (f x -> x)


-- * Mu - Least Fixed Point
---------------------------------------------------------------------

-- |
-- The least fixed point of @f@ by the lemma 'lambekMu'.
--
data Mu f = Mu (forall x. (f x -> x) -> x)

-- |
-- @'Mu' f@ is an algebra and is @f@-closed.
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

-- |
-- Lambek's proof that @'Mu' f@ is a fixed point of @f@.
-- 
lambekMu :: Functor f => Lambek f (Mu f)
lambekMu = Lambek initial algebra
    where
    algebra = intoMu
    initial = foldMu (fmap intoMu)


-- * Nu - Greatest Fixed Point
---------------------------------------------------------------------

-- |
-- The greatest fixed point of @f@ by 'lambekNu'.
--
data Nu f = forall x. Nu (x -> f x) x

-- |
-- @'Nu' f@ is a coalgebra and is @f@-consistent.
--
outOfNu :: Functor f => Nu f -> f (Nu f)
outOfNu (Nu f x) = fmap (Nu f) (f x)

-- |
-- @'Nu' f@ is terminal: given a coalgebraic type @(x, x -> f x)@,
-- there is a map @'unfoldNu' f :: x -> 'Nu' f@.
--
unfoldNu :: Functor f => (x -> f x) -> x -> Nu f
unfoldNu = Nu

-- |
-- Lambek's proof that @'Nu' f@ is a fixed point of @f@.
-- 
lambekNu :: Functor f => Lambek f (Nu f)
lambekNu = Lambek coalgebra terminal
    where
    coalgebra   = outOfNu
    terminal    = unfoldNu (fmap outOfNu)

    
-- * Categorical Properties
---------------------------------------------------------------------

refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = go
    where
    go x = f (fmap go (g x))

