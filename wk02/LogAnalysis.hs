{-# OPTIONS_GHC -Wall #-}

-- CIS 194 -- Week 02

module LogAnalysis where

import Log
import Data.Char (isSpace)

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
