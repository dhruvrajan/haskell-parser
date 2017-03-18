#!/usr/bin/env runghc

-- Lexer for the 'fun' language defined in cs429h

module Lexer where
import Data.Char
import Data.Maybe

data Token = Variable String | Number Int
  | Equals | Semicolon | Invalid String deriving (Eq, Show)

data Statement = Assignment String Int
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
tokenize :: String -> (Token, String)
tokenize "" = (Invalid "", "")
tokenize (x:xs)
  | isAlpha x = let (tok, remaining) = splitId (x:xs) in (Variable tok, remaining)
  | isDigit x = let (tok, remaining) = splitNum (x:xs)
    in (Number ((read tok) :: Int), remaining)
  | x == ';'  = let (tok, remaining) = splitSemi (x:xs) in (Semicolon, remaining)
  | x == '='  = let (tok, remaining) = splitEq (x:xs) in (Equals, remaining)
  | isSpace x = tokenize xs
  | otherwise = (Invalid [x], xs) -- Consume error?

-- Returns an infinite list of tokens from a program string
---- Can exploit Lazy Evaluation
---- If at a certain point the
tokens :: String -> [Token]
tokens "" = []
tokens xs = t : tokens ts
  where 
    (t, ts) = tokenize xs



parse :: [Token] -> [Statement]
parse [] = []
parse (Variable var : Equals : Number val : Semicolon : toks)
  = (Assignment var val) : parse toks
parse (t:ts) = parse ts -- LLK parsing error recovery





-- statement :: String -> (Maybe Statement, String)
-- statement tokens = result
--   where
--     -- Perform splits
--     (var, prog0)    = splitId tokens
--     (eq, prog1)     = splitEq prog0
--     (num, prog2)    = splitNum prog1
--     (semi, remains) = splitSemi prog2

--     -- Calculate result
--     varExists = var /= ""
--     eqExists  = eq  /= ""
--     numExists = num /= ""
--     semiExists = semi /= ""
--     result = if (varExists && semiExists)
--                 then if (eqExists && numExists)
--                         then (Just (Assignment var num), remains)
--                         else (Just (Declaration var), remains)
--                  else (Nothing, tokens)

-- -- parse the whole program
-- program :: String -> Maybe Program
-- program ""   = Just []
-- program prog = if (isNothing rest) then Nothing else Just (statement0 : (fromJust rest))
--   where
--     line = statement prog
--     (statement0, remaining) = line
--     rest0 = program remaining
--     rest = if isNothing statement0  then Nothing else rest0

