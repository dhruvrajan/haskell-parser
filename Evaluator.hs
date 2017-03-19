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

-- Add a key and value to an association-list symbol table
addst :: String -> Int -> SymbolTable -> SymbolTable
addst var val [] = [(var, val)]
addst var val ((name, num) : ls) | var == name = (var, val) : ls
                                 | otherwise = (name, num) : (addst var val ls)

-- get the value for a certain key
getst :: String -> SymbolTable -> Int
getst _ [] = 0
getst var ((name, num) : ls) | var == name = num
                             | otherwise = getst var ls

-- evaluate an expression
evaluateExpr :: SymbolTable -> Expr -> Int
evaluateExpr st (Add e1 e2) = (evaluateExpr st e1) + (evaluateExpr st e2)
evaluateExpr st (Multiply e1 e2) = (evaluateExpr st e1) * (evaluateExpr st e2)
evaluateExpr st (Variable string) = getst string st
evaluateExpr _ (Number num) = num
  
-- evaluate a statement
evaluateStatement :: SymbolTable -> Statement -> SymbolTable
evaluateStatement st (Assignment var expr) = addst var val st
  where
    val = evaluateExpr st expr

evaluate :: Program -> SymbolTable
evaluate prog = foldl evaluateStatement [] prog

-- interpret a program string
interpret :: String -> SymbolTable
interpret prog = evaluate $ parse $ tokens prog
