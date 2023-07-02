
{-# LANGUAGE TupleSections #-}

module Stream where

import CoInductive


-- * Standard Infinite Stream
---------------------------------------------------------------------

data Stream x = x :- Stream x

-- |
-- Ordinary unstreaming: given a stream, we can split its head
-- and tail.
-- 
unstream :: Stream x -> (x, Stream x)
unstream (x :- xs) = (x, xs)


-- * Greatest Fixed Point Example: Infinite Streams
---------------------------------------------------------------------

-- |
-- @'Nu' (x,)@ is equivalent to @(x, 'Nu' (x,))@, which is the type
-- of infinite streams that always have a separable head and tail.
-- 
type NuStream x = Nu ((,) x)

-- |
-- Fixed point unstreaming: given a fixed point of @(x,)@, we can split
-- its 'fst' and 'snd'. This is equivalent to 'outOfNu' for 'NuStream'.
-- 
nuUnstream :: NuStream x -> (x, NuStream x)
nuUnstream = outOfNu

-- |
-- We can convert any 'Stream' to a 'NuStream'.
-- 
streamToNuStream :: Stream x -> NuStream x
streamToNuStream = Nu unstream

-- |
-- We can convert any 'NuStream' to a 'Stream'.
-- 
nuStreamToStream :: NuStream x -> Stream x
nuStreamToStream y = x :- (nuStreamToStream xs)
    where
    (x, xs) = outOfNu y
