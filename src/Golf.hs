module Golf where
import Data.Char
import Data.List

skips :: [a] -> [[a]]
skips xs = 
    let ith i ys= [y | (j, y) <- (zip [1..] ys), j `mod` i == 0] 
    in zipWith ith [1..length xs] (repeat xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = 
    let triples = zip3 xs (drop 1 xs) (drop 2 xs)
    in [ y |(x,y,z) <- triples, y > z && y > x]

histogram :: [Int] -> String
histogram = 
    let hit = flip replicate '*'
        miss = flip replicate ' '
        draw c total = hit c ++ miss (total + 1 - c)
        maxi ns = maximum (map snd ns)
        display ns  = map (\(n, c) -> show n ++ "=" ++ (draw c $ maxi ns)) ns 
        counts xs = map (\n -> (n, length $ elemIndices n xs)) [0..9]
        in unlines . reverse . transpose . display . counts