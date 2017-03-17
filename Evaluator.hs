module Evaluator where

import Lexer

-- Base function for function-based symbol table
getst :: String -> Maybe String
getst _ = Nothing

-- Add a key, value pair to a function-based symbol table. Inefficient, but cool.
addst :: String -> String -> (String -> Maybe String) -> (String -> Maybe String)
addst var val st = \ x -> if (x == var) then Just val else st x


-- Helper
evaluateHelper :: Program -> (String -> Maybe String) -> (String -> Maybe String)
evaluateHelper [] g = g
evaluateHelper ((Just (Assignment var val)) : ls) g = evaluateHelper ls (addst var val g)
evaluateHelper ((Just (Declaration var)) : ls) g = evaluateHelper ls g

-- Evaluate a Program.
evaluate :: Program -> (String -> Maybe String)
evaluate prog = evaluateHelper prog getst
