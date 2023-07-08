
module CoAlgebra where


-- |
-- Evidence that @x ~ f x@ given a pair of inverses.
-- 
-- See <https://ncatlab.org/nlab/show/initial+algebra+of+an+endofunctor>
-- theorem 2.2 for details on how the lemma works.
-- 
data Lambek f x = Lambek (x -> f x) (f x -> x)


-- * Mu - Least Fixed Point
---------------------------------------------------------------------

-- |
-- The least fixed point of @f@ by the lemma 'lambekMu'.
--
data Mu f = Mu (forall x. (f x -> x) -> x)

-- |
-- Lambek's proof that @'Mu' f@ is a fixed point of @f@.
-- 
lambekMu :: Functor f => Lambek f (Mu f)
lambekMu = Lambek initial algebra
    where
    algebra = intoMu
    initial = foldMu (fmap intoMu)

-- |
-- @'Mu' f@ is an [@f@-algebra](https://en.wikipedia.org/wiki/F-algebra),
-- and it is [@f@-closed](https://en.wikipedia.org/wiki/Coinduction).
--
intoMu :: Functor f => f (Mu f) -> Mu f
intoMu x = Mu go
    where
    go f = f (fmap (foldMu f) x)

-- |
-- @'Mu' f@ is an initial object: given any @f@-algebra
-- @(x, f x -> x)@, we can create a morphism @'Mu' f -> x@:
-- 
-- @
--  muInitial :: 'Functor' f => (x, f x -> x) -> ('Mu' f -> x)
--  muInitial (_, f) = foldMu f
-- @
--
foldMu :: Functor f => (f x -> x) -> Mu f -> x
foldMu f (Mu cata) = cata f


-- * Nu - Greatest Fixed Point
---------------------------------------------------------------------

-- |
-- The greatest fixed point of @f@ by 'lambekNu'.
--
data Nu f = forall x. Nu (x -> f x) x

-- |
-- Lambek's proof that @'Nu' f@ is a fixed point of @f@.
-- 
lambekNu :: Functor f => Lambek f (Nu f)
lambekNu = Lambek coalgebra terminal
    where
    coalgebra   = outOfNu
    terminal    = unfoldNu (fmap outOfNu)

-- |
-- @'Nu' f@ is an [@f@-coalgebra](https://en.wikipedia.org/wiki/F-coalgebra),
-- and it is [@f@-consistent](https://en.wikipedia.org/wiki/Coinduction).
--
outOfNu :: Functor f => Nu f -> f (Nu f)
outOfNu (Nu f x) = fmap (Nu f) (f x)

-- |
-- @'Nu' f@ is a terminal object: given any @f@-coalgebra
-- @(x, x -> f x)@, we can create a morphism @x -> 'Nu' f@:
-- 
-- @
--  nuTerminal :: 'Functor' f => (x, x -> f x) -> (x -> 'Nu' f)
--  nuTerminal (_, f) = unfoldNu f
-- @
--
unfoldNu :: Functor f => (x -> f x) -> x -> Nu f
unfoldNu = Nu

    
-- * Categorical Properties
---------------------------------------------------------------------

refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = go
    where
    go x = f (fmap go (g x))

