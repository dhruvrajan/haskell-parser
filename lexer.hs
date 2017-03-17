#!/usr/bin/env runghc

-- Lexer for the 'fun' language defined in cs429h

module Lexer where
import Data.Char

data Statement = Assignment String String
  | Declaration String deriving (Eq, Show)

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

-- get next statement
statement :: String -> (Maybe Statement, String)
statement prog = result
  where
    -- Perform splits
    (var, prog0)    = splitId prog
    (eq, prog1)     = splitEq prog0
    (num, prog2)    = splitNum prog1
    (semi, remains) = splitSemi prog2

    -- Calculate result
    varExists = var /= ""
    eqExists  = eq  /= ""
    numExists = num /= ""
    semiExists = semi /= ""
    result = if (varExists && semiExists)
                then if (eqExists && numExists)
                        then (Just (Assignment var num), remains)
                        else (Just (Declaration var), remains)
                 else (Nothing, prog)
