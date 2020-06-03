module LogAnalysis where

import Log
import Text.Read
import Control.Monad
import Data.Maybe

parseMessage :: String -> LogMessage
parseMessage s =
    let (maybeMtype, s1) = parseType $ words s
        (maybeTs, s2) = parseTs s1
        lm = liftM3 LogMessage maybeMtype maybeTs (Just $ unwords s2)
    in fromMaybe (Unknown s) lm

parseType :: [String] -> (Maybe MessageType, [String])
parseType ("I":xs) = (Just Info, xs)
parseType ("W":xs) = (Just Warning, xs)
parseType s@("E":code:rest) = (Error <$> readMaybe code, rest)
parseType s = (Nothing, s)

parseTs :: [String] -> (Maybe TimeStamp, [String])
parseTs (x:xs) = (readMaybe x, xs)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

instance Ord LogMessage where
    compare (LogMessage _ t1 _) (LogMessage _ t2 _) = compare t1 t2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) m = m
insert lm (Leaf) = Node Leaf lm Leaf
insert lm (Node l nlm r)    | lm > nlm = Node l nlm (insert lm r)
                            | otherwise = Node (insert lm l) nlm r

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = let
    msg (LogMessage _ _ m) = m
    sev50 (LogMessage (Error n ) _ _ )= n > 50
    errors (LogMessage (Error sev) _ _ ) = True
    errors _ = False
    in map msg . filter sev50 . inOrder . build . filter errors
