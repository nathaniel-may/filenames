module TypeCheckerTests (tests) where

import CustomPrelude -- import all
import Exceptions    (TypeException(..))
import Test.HUnit    -- import all
import Types         -- import all
import TypeChecker   -- import all


test1 :: Test
test1 = TestCase $ assertEqual 
  "heterogeneous lists fail to typecheck" 
  (Left Boop2)
  (typecheck (ListU [IntU 0, StringU "hello", StringU "world"]))

tests :: [Test]
tests = [test1]
