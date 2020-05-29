module CreditCard where
    
toDigits :: Integer -> [Integer]
toDigits i | i < 10  = [i]
toDigits i = 
    let (num, e) = divMod i 10
    in toDigits num ++ [e]

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
