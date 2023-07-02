
module Unit where

import CoInductive

import Data.Functor.Identity


-- * Standard Unit Type
---------------------------------------------------------------------

-- |
-- This type has exactly one unique value.
-- 
data Unit = U
    deriving Show

-- |
-- 'Unit' is terminal: there is a function which sends anything to 'U'.
-- 
unit :: x -> Unit
unit _ = U


-- * Greatest Fixed Point Example: Unit Type
---------------------------------------------------------------------

-- |
-- @'Nu' 'Identity'@ is equivalent to @(x, x -> x)@, which is the set
-- of types with an identity function. Since all identity functions are
-- equivalent, this has one unique value.
-- 
type NuUnit = Nu Identity

-- |
-- 'NuUnit' is terminal: there is a function which sends any @x@ to
-- @(x, 'id')@.
-- 
nuUnit :: x -> NuUnit
nuUnit x = Nu Identity x

