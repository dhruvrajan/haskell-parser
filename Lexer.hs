#!/usr/bin/env runghc

-- Lexer for the 'fun' language defined in cs429h

module Lexer where
import Data.Char
import Data.Maybe

data Statement = Assignment String String
  | Declaration String deriving (Eq, Show)

type Program = [Maybe Statement]

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

-- tokenize properly
tokenize :: String -> Maybe (String, String)
tokenize "" = Just ("", "")
tokenize (x:xs)
  | isAlpha x = Just $ splitId (x:xs)
  | isDigit x = Just $ splitNum (x:xs)
  | x == ';'  = Just $ splitSemi (x:xs)
  | x == '='  = Just $ splitEq (x:xs)
  | isSpace x = tokenize xs
  | otherwise = Nothing

-- Returns an infinite list of tokens from a program string
---- Can exploit Lazy Evaluation
---- If at a certain point the 
tokens :: String -> [Maybe String]
tokens "" = []
tokens (x:xs) = t : tokens ts
  where
    res = tokenize (x:xs)

    -- y, ys are strings
    (y, ys) = if (isJust res) then fromJust res else ("", "")

    -- t is a Maybe String, ts is a string
    (t, ts) = if (isJust res) then (Just y, ys) else (Nothing, (x:xs))


-- get next token from an infinite list of tokens
nextToken :: [String] -> Maybe String
nextToken ts = x
  where
    l = take 1 ts
    x = if (length l > 0) then Just $ ts !! 0 else Nothing

    
    
-- get next statement
-- takes a list of tokens, and processes them.
statement :: [String] -> (Maybe Statement, String)
statement tokens = result
  where
    -- Perform splits
    (var, prog0)    = uncons tokens
    (eq, prog1)     = uncons prog0
    (num, prog2)    = uncons prog1
    (semi, remains) = uncons prog2

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

-- parse the whole program
program :: String -> Maybe Program
program ""   = Just []
program prog = if (isNothing rest) then Nothing else Just (statement0 : (fromJust rest))
  where
    line = statement prog
    (statement0, remaining) = line
    rest0 = program remaining
    rest = if isNothing statement0  then Nothing else rest0

