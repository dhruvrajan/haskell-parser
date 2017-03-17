module EvaluatorTest where

import Evaluator
import Test.HUnit

-- Test add and get on symbol table
test1 = TestCase (assertEqual "test1" (get "x") (Just 3))
  where
    get = addst "x" 3 getst


tests = TestList [TestLabel "test1" test1]

runTests = runTestTT tests
