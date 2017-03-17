#!/usr/bin/env runghc

-- Lexer for the 'fun' language defined in cs429h

module Lexer where
import Data.Char

data Statement = Assignment String String deriving (Eq, Show)

-- Generic split
split :: (Char -> Bool) -> String -> (String, String)
split _ "" = ("", "")
split key (x:xs) | key x = let (a, b) = split key xs in (x : a, b)
                 | isSpace x = split key xs
                 | otherwise = ("", (x:xs))

-- Return (var, remaining)
splitId :: String -> (String, String)
splitId = split isAlpha

-- Split on equals sign
splitEq :: String -> (String, String)
splitEq = split (\x -> x == '=')

-- Split on number
splitNum :: String -> (String, String)
splitNum = split isDigit

-- split on semicolon
splitSemi :: String -> (String, String)
splitSemi = split (\x -> x == ';')

-- get next assignment
assignment :: String -> (Maybe Statement, String)
assignment prog = if valid then (Just (Assignment var num), remains) else (Nothing, prog)
  where
    (var, prog0)   = splitId prog
    (_, prog1)     = splitEq prog0
    (num, prog2)   = splitNum prog1
    (_, remains)   = splitSemi prog2
    valid = (num /= "") && (var /= "")




