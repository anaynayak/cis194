{-# LANGUAGE FlexibleInstances #-}
module JoinList where
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = mappend 

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty

instance Monoid m => Semigroup (JoinList m a) where
    (<>) a b = 
        let
            m1 = tag a
            m2 = tag b
        in Append (m1 <> m2) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

instance (Sized b, Monoid b) => Sized (JoinList b a) where
  size = size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n jl | n > (getSize $ size jl) = Nothing
indexJ n (Append m a b) | sa < n = indexJ (n - sa) b
                        | otherwise =  indexJ n a
                        where sa = getSize $ size a
indexJ 0 (Single m a) = Just a
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <=0 = jl
dropJ n (Append m a b)  | sa < n = dropJ (n - sa) b
                        | otherwise = dropJ n a
                        where sa = getSize $ size a
dropJ _ _  = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i a | i <= 0 = a
takeJ i a | i > (getSize $ size a) = a
takeJ i (Append m a b )
        | i < sa = takeJ i a
        | otherwise = a +++ takeJ ( i - sa) b
        where sa = getSize $ size a

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Foldable (JoinList m) where
    foldMap f Empty = mempty
    foldMap f (Single m a) = f a
    foldMap f (Append m a b) = mappend (foldMap f a) (foldMap f b)

instance Buffer (JoinList (Score, Size) String) where
    toString = foldr (++) [] 
    line n b = indexJ n b
    replaceLine n l b = takeJ (n-1) b +++ (Single (scoreString l, Size 1) l) +++ dropJ n b
    numLines = getSize . size
    value b = let (Score i) = (fst . tag) b in i
    fromString ls = foldr (\x acc -> acc) Empty (map (\l -> Single (scoreString l, Size 1)) (lines ls))

  