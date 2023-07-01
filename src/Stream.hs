
{-# LANGUAGE TupleSections #-}

module Stream where

import CoInductive


-- * Co-inductive Algebra Example: Infinite Streams

data Stream a = a :- Stream a

unstream :: Stream a -> (a, Stream a)
unstream (x :- xs) = (x, xs)

type NuStream a = Nu ((,) a)

nuUnstream :: NuStream a -> (a, NuStream a)
nuUnstream = outOfNu

streamToNuStream :: Stream a -> NuStream a
streamToNuStream = Nu unstream

nuStreamToStream :: NuStream a -> Stream a
nuStreamToStream y = x :- (nuStreamToStream xs)
    where
    (x, xs) = outOfNu y
