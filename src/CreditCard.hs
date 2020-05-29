module CreditCard where
import Data.List
import Data.Tuple
    
toDigits :: Integer -> [Integer]
toDigits = 
    let toDigits v = if v == 0 then Nothing else Just $ swap (divMod v 10)
    in reverse . unfoldr toDigits

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs@(x: xss) | even $ length xs = x * 2 : doubleEveryOther xss
doubleEveryOther (x: xs) = x: doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . (map toDigits) 

validate :: Integer -> Bool
validate = (==0). ((flip mod) 10) . sumDigits . doubleEveryOther . toDigits
