{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Monoid
import Data.Maybe
import qualified Data.Map as Map

newtype Score = Score Int
              deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty = Score 0    

scores = Map.fromList $ zip ['a'..'z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

score :: Char -> Score
score s = fromMaybe 0 (Map.lookup s scores)

scoreString :: String -> Score
scoreString xs = mconcat (map score xs)