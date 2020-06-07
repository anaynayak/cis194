{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import qualified Data.Bifunctor as B
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

eval :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
eval bf a b = let
  amoves = reverse . sort $ a
  bmoves = reverse . sort $ b
  (awins, bwins) = ((B.bimap length length) . partition id) $ zipWith (>) amoves bmoves
  in Battlefield (attackers bf - bwins) (defenders bf - awins)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = let
  attack = max 3 $ (attackers b - 1)
  defend = min 2 $ defenders b
  moves = (flip replicateM) die
  in liftM2 (eval b) (moves attack) (moves defend)

isOver :: Battlefield -> Bool
isOver (Battlefield a d) = d == 0 || a < 2

invade :: Battlefield -> Rand StdGen Battlefield
invade bf | isOver bf = return bf
invade bf = let
  bf2 = battle bf
  in bf2 >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = let
  runs = replicateM 1000 $ invade bf
  foo = ( (/1000). fromIntegral . length . filter id . map ((==0) . defenders))
  in foo <$> runs
