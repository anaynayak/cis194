module Hanoi where


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _  = []
hanoi n from to tmp = hanoi (n-1) from tmp to ++ [(from, to)] ++ hanoi (n-1) tmp to from
