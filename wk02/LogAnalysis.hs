{-# OPTIONS_GHC -Wall #-}

-- CIS 194 -- Week 02

module LogAnalysis where

import Log
import Data.Char (isSpace)

-- Exercise 01

parseRest :: String -> (Int, String)
parseRest str =
  let [(t, rest)] = lex str
  in
   (read t :: Int, dropWhile isSpace rest)

parseMessage :: String -> LogMessage
parseMessage message =
  let [(t1, rest)] = lex message
  in
   case t1 of
     "I" -> let (ts, msg) = parseRest rest in LogMessage Info ts msg
     "W" -> let (ts, msg) = parseRest rest in LogMessage Warning ts msg
     "E" -> let (e, rest') = parseRest rest
            in
             let (ts, msg) = parseRest rest' in LogMessage (Error e) ts msg
     _   -> Unknown message

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

-- Exercise 02

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg (Leaf) = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left root@(LogMessage _ rts _) right) =
  if ts < rts then
     Node (insert msg left) root right
  else
    Node left root (insert msg right)

-- Exercise 03

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 04

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 05

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getError . inOrder . build . filter isSevereError
  where isSevereError (LogMessage (Error s) _ _) = s >= 50
        isSevereError _ = False
        getError (LogMessage (Error _) _ msg) = msg
