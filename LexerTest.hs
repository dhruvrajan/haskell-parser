module LexerTest where

import Lexer
import Test.HUnit 

-- Test Assignments
test1 = TestCase (assertEqual "valid assignment" (Just (Assignment "x" "3"), "y = 4;") (statement "x = 3;y = 4;"))
test2 = TestCase (assertEqual "assignment missing var" (Nothing, "= 3;y = 4;") (statement "= 3;y = 4;"))
test3 = TestCase (assertEqual "assignment missing var and =" (Nothing, "3;y = 4;") (statement "3;y = 4;"))

-- Test Declarations
test4 = TestCase (assertEqual "valid declaration" (Just (Declaration "x"), "y = 4;") (statement "x; y = 4;"))



tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4]


-- shortcut to run the tests
runTests = runTestTT tests
