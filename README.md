Haskell Parser
=====

Evaluate sequences of assignment statements:

Programs may be of the form "x = 3; y = 4; z = 5;"  ... etc.
Each assignment must have an equals sign and an semicolon

To run:
```
ghci > :load Evaluator
ghci > let p = program <program>
ghci > let g = evaluate (fromJust p)
ghci > -- g is a symbol-table access function. Calling it with a variable name will retrieve the variable's value. For example:
ghci > g "x"
     Just "3"
ghci > g "y"
     Just "4"
```