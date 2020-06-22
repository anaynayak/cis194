{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Matrix where

import Fibonacci

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (*) (Matrix m1 m2
                m3 m4)
        (Matrix n1 n2
                n3 n4) = Matrix (m1 * n1 + m2 * n3) (m1 * n2 + m2 * n4)
                                (m3 * n1 + m4 * n3) (m3 * n2 + m4 * n4)

fib4 :: Integer -> Integer
fib4 n = let (Matrix a _ _ _) =  Matrix 1 1 1 0 ^ n
        in a
