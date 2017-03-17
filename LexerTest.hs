module LexerTest where

import Lexer
import Test.HUnit 


test1 = TestCase (assertEqual "valid1" (Just (Assignment "x" "3"), "y = 4;") (assignment "x = 3;y = 4;"))
test2 = TestCase (assertEqual "invalid, =" (Nothing, "= 3;y = 4;") (assignment "= 3;y = 4;"))
test3 = TestCase (assertEqual "invalid, 3" (Nothing, "3;y = 4;") (assignment "3;y = 4;"))

tests = TestList [TestLabel "working test 1" test1,
                  TestLabel "invalid test 1" test2,
                  TestLabel "invalid test 3" test3]


-- shortcut to run the tests
run = runTestTT tests
