module Parser where

-- Parse a list of tokens
parse :: [Token] -> [Statement]
parse [] = []
parse (Variable var : Equals : Number val : Semicolon : toks)
  = (Assignment var val) : parse toks
parse (t:ts) = parse ts -- LLK parsing error recovery
