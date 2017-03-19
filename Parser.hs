module Parser where

import Lexer

expression :: [Token] -> (Expr, [Token])

factor :: [Token] -> (Expr, [Token])
factor [] = (InvalidExpr $ InvalidToken "", [])
factor (Identifier name : ts) = (Variable name, ts)
factor (Digits value : ts)    = (Number value, ts)
factor (x:xs) = (InvalidExpr x, (x:xs))


term :: [Token] -> (Expr, [Token])
term ts = let (expr, remainder) = factor ts in
  case remainder of
    (Operator "*" : rs) -> let (e, r) = term rs in (Multiply expr e, r)
    (Operator "/" : rs) -> let (e, r) = term rs in (Divide   expr e, r)
    _                   -> (expr, remainder)


expression ts = let (expr, remainder) = term ts in
  case remainder of
    (Operator "+" : rs) -> let (e, r) = expression rs in (Add expr e, r)
    (Operator "-" : rs) -> let (e, r) = expression rs in (Subtract expr e, r)
    _                   -> (expr, remainder)


-- Parse a list of tokens
parse :: [Token] -> [Statement]
parse [] = []
parse (Identifier var : Equals : ts) = (Assignment var expr) : parse remaining
  where
    (expr, (semi : remaining)) = expression ts
parse (t:ts) = parse ts -- LLK parsing error recovery
