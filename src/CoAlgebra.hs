
module CoAlgebra where


-- |
-- Categorical evidence that @x ~ f x@ for initial algebras and terminal
-- coalgebras.
-- 
-- === Outline of the Lemma
-- 
-- An initial algebra (dually, terminal coalgebra) on @x@ has the following
-- morphisms:
-- 
--  - An "outward" arrow: @i :: x -> f x@, by initiality.
--  
--  - An "inward" arrow: @a :: f x -> x@, from the definition of an
--    @f@-algebra.
-- 
-- Terminal coalgebras have similar arrows with their directions flipped.
-- 
-- Composing the arrows should give:
-- 
--  - An inward-facing identity: @'id' ~ a . i :: x -> x@.
-- 
--  - An outward-facing identity: @'id' ~ i . a :: f x -> f x@.
-- 
-- If both of these identities hold, the conclusion is that @x ~ f x@.
-- This type represents the outward and inward morphisms respectively, since
-- they're sufficient to complete the proof.
-- 
-- See <https://ncatlab.org/nlab/show/initial+algebra+of+an+endofunctor>
-- theorem 2.2.
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
lambekMu = Lambek outward inward
    where
    inward  = intoMu
    outward = foldMu (fmap intoMu)

-- |
-- The inward arrow on @'Mu' f@, which makes it an @f@-algebra.
-- 
-- Some other equivalent interpretations are:
-- 
--  - @'Mu' f@ is closed under applications of @f@.
--  
--  - Things that are true in @f ('Mu' f)@ are also true in @'Mu' f@, which
--    provides the basis for recursion.
--
intoMu :: Functor f => f (Mu f) -> Mu f
intoMu x = Mu go
    where
    go f = f (fmap (foldMu f) x)

-- |
-- The precursor to the outward arrow on @'Mu' f@. We can see that it is
-- initial, since given an @f@-algebra @(x, f x -> x)@ we can construct a
-- canonical morphism:
-- 
-- @
--  muInitial :: 'Functor' f => (x, f x -> x) -> ('Mu' f -> x)
--  muInitial (_, f) = foldMu f
-- @
-- 
-- The outward arrow arises as a special case where we consider the algebra
-- given by @'fmap' 'intoMu' :: f (f ('Mu' f)) -> f ('Mu' f)@.
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
-- Lambek's proof that @'Nu' f@ is a fixed point of @f@. This follows
-- the same inward/outward arrow reasoning used for 'lambekMu', but flips
-- the directions of the arrows.
-- 
lambekNu :: Functor f => Lambek f (Nu f)
lambekNu = Lambek outward inward
    where
    outward = outOfNu
    inward  = unfoldNu (fmap outOfNu)

-- |
-- The inward arrow on @'Nu' f@, which makes it an @f@-coalgebra.
-- 
-- Some other equivalent interpretations are:
-- 
--  - @'Nu' f@ is consistent under applications of @f@.
--  
--  - Things that are true in @'Nu' f@ are also true in @f ('Nu' f)@.
--
outOfNu :: Functor f => Nu f -> f (Nu f)
outOfNu (Nu f x) = fmap (Nu f) (f x)

-- |
-- The precursor to the inward arrow on @'Nu' f@. Like with 'foldMu',
-- we can construct a canonical morphism to any @f@-coalgebra
-- @(x, x -> f x)@ to see that @'Nu' f@ is terminal with the function:
-- 
-- @
--  nuTerminal :: 'Functor' f => (x, x -> f x) -> (x -> 'Nu' f)
--  nuTerminal (_, f) = unfoldNu f
-- @
-- 
-- Unlike 'foldMu', the outward arrow isn't a special case requiring extra
-- work, since by definition @'Nu' f ~ (x, x -> f x)@ already.
--
unfoldNu :: Functor f => (x -> f x) -> x -> Nu f
unfoldNu = Nu

    
-- * Categorical Properties
---------------------------------------------------------------------

refold :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
refold f g = go
    where
    go x = f (fmap go (g x))

