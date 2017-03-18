module Evaluator where

import Lexer
import Parser

--type SymbolTable = String -> Maybe Int
type SymbolTable = [(String, Int)]
--type SymbolTable = String -> Maybe Int

-- Base function for function-based symbol table
--getst :: SymbolTable
--getst _ = Nothing

-- Add a key, value pair to a function-based symbol table. Inefficient, but cool.
-- addst :: String -> Int -> SymbolTable -> SymbolTable
-- addst var val st = \ x -> if (x == var) then Just val else st x

addst :: String -> Int -> SymbolTable -> SymbolTable
addst var val [] = [(var, val)]
addst var val ((name, num) : ls) | var == name = (var, val) : ls
                                 | otherwise = (name, num) : (addst var val ls)

-- first level evaluation
evaluate1 :: SymbolTable -> Statement -> SymbolTable
evaluate1 st (Assignment var val) = addst var val st

evaluate :: Program -> SymbolTable
evaluate prog = foldl evaluate1 [] prog

-- interpret a program string
interpret :: String -> SymbolTable
interpret prog = evaluate $ parse $ tokens prog
