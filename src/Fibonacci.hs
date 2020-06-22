module Fibonacci where
import Data.List

fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
-- fibs2 = unfoldr (\v -> Just (fib v, v+1)) 0
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a xs) = a : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) fooStream where
    interleaveStreams (Cons a s1) (Cons b s2) = Cons a (interleaveStreams s2 s1)
    fooStream = streamRepeat 3 -- tbd
