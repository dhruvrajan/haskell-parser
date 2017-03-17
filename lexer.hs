#!/usr/bin/env runghc

-- Lexer for the 'fun' language defined in cs429h

module Lexer where
import Data.Char

data Statement = Assignment String Integer deriving (Eq, Show)

-- Generic split
split :: (Char -> Bool) -> String -> (String, String)
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

-- get next assignment


