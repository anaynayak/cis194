{-# LANGUAGE FlexibleInstances #-}
module JoinList where
import Sized

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) = mappend 

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty

instance Monoid m => Semigroup (JoinList m a) where
    (<>) jl1 jl2 = 
        let
            m1 = tag jl1
            m2 = tag jl2
        in Append (m1 <> m2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n jl = undefined