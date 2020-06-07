module LogAnalysis where

import Log
import Text.Read
import Control.Monad
import Data.Maybe
import AParser
import Control.Applicative

parseMessage :: String -> LogMessage
parseMessage s = fromMaybe (Unknown s) (pure fst <*> runParser logMessage s) where
    logMessage = parseError <|> parseInfo <|> parseWarn
    parseLogMessage code = liftA3 LogMessage parseECode parseTs parseMsg
    parseError = liftA3 LogMessage parseECode parseTs parseMsg
    parseInfo = liftA3 LogMessage parseICode parseTs parseMsg
    parseWarn = liftA3 LogMessage parseWCode parseTs parseMsg
    parseECode = ( Error . fromInteger) <$> (char 'E' *> char ' ' *> posInt)
    parseICode = char 'I' *> pure Info
    parseWCode = char 'W' *> pure Warning
    parseTs = fromInteger <$> (char ' ' *> posInt)
    parseMsg = many <$> satisfy $ const True

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
