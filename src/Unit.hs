
module Unit where

import CoInductive

import Data.Functor.Identity


-- * Co-inductive Algebra Example: Unit Type

data Unit = U
    deriving Show

unit :: a -> Unit
unit x = ()

type NuUnit = Nu Identity

nuUnit :: a -> NuUnit
nuUnit x = Nu Identity x

nuUnitToUnit :: NuUnit -> Unit
nuUnitToUnit (Nu out y) = refold runIdentity out y

