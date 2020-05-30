module Wholemeal where

fun1 :: [Integer] -> Integer
fun1 = product . map ((-) 2). filter even

fun2 :: Integer -> Integer
fun2 = 
    let fn n = if even n then n `div` 2 else 3 * n + 1
    in sum . filter even . takeWhile (>1) . iterate fn
