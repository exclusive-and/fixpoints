
{-# LANGUAGE EmptyCase #-}

module Void where

import CoInductive

import Data.Functor.Identity


-- * Standard Void Type
---------------------------------------------------------------------

-- |
-- This has no constructors, so you can never have a value of it.
-- 
data Void

-- |
-- Ordinary principle of explosion: given a 'Void', we can deduce any
-- other type.
-- 
voidrec :: Void -> x
voidrec x = case x of {}


-- * Least Fixed Point Example: Void Type
---------------------------------------------------------------------

-- |
-- @'Mu' 'Identity'@ is equivalent to @(x -> x) -> x@, which is the
-- type of a loop that never terminates. Since it never terminates, you
-- can never get a value from it, like 'Void'.
-- 
type MuVoid = Mu Identity

-- |
-- Principle of explosion: given a 'MuVoid', we can deduce any other
-- type.
--
muVoidrec :: MuVoid -> a
muVoidrec (Mu cata) = cata runIdentity
