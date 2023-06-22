
module Void where

import CoInductive


-- * Inductive Algebra Example: Void Type

data Void

voidrec :: Void -> a
voidrec x = case x of {}

type MuVoid = Mu Identity

muVoidrec :: MuVoid -> a
muVoidrec (Mu cata) = cata runIdentity
