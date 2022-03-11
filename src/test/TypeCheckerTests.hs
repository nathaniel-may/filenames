module TypeCheckerTests (tests) where

import CustomPrelude -- import all
import Exceptions    (TypeException(..))
import Test.HUnit    -- import all
import Types         -- import all
import TypeChecker   -- import all


test1 :: Test
test1 = TestCase $ assertEqual 
  "heterogeneous list fails to typecheck" 
  (Left Boop2)
  (typecheck (ListU [IntU 0, StringU "hello", StringU "world"]))

test2 :: Test
test2 = TestCase $ assertEqual 
  "heterogeneous list of lists fails to typecheck" 
  (Left Boop2)
  (typecheck (ListU [
    ListU [IntU 0]
  , ListU [StringU "hello world"]
  ]))

tests :: [Test]
tests = [test1, test2]
