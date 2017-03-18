#!/usr/bin/env runghc

-- Lexer for the 'fun' language defined in cs429h

module Lexer where
import Data.Char
import Data.Maybe

data Token = Variable String | Number Int
  | Equals | Semicolon | Invalid String deriving (Eq, Show)

data Statement = Assignment String Int
  | Declaration String deriving (Eq, Show)

type Program = [Statement]

-- Return (var, remaining)
splitId :: String -> (String, String)
splitId = span isAlpha

-- Split on equals sign
splitEq :: String -> (String, String)
splitEq = span (\x -> x == '=')

-- Split on number
splitNum :: String -> (String, String)
splitNum = span isDigit

-- split on semicolon
splitSemi :: String -> (String, String)
splitSemi = span (\x -> x == ';')

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


-- Parse a list of tokens
parse :: [Token] -> [Statement]
parse [] = []
parse (Variable var : Equals : Number val : Semicolon : toks)
  = (Assignment var val) : parse toks
parse (t:ts) = parse ts -- LLK parsing error recovery
