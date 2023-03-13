module TypeCheckerTests (tests) where

import CustomPrelude -- import all
import Data.String   (String)
import Exceptions    (TypeException(..))
import Test.HUnit    -- import all
import Types         -- import all
import TypeChecker   -- import all


assertTypeChecks :: String -> ExprU -> Assertion
assertTypeChecks name expr = assertBool (name <> "\n" <> show result) (isRight result) where
    result = typecheck expr

test1 :: Test
test1 = TestCase $ assertTypeChecks
  "schema with one part typechecks" 
  (ApplyU (ApplyU (IdentifierU (Name "schema")) (StringU "-")) (ListU [ApplyU (ApplyU (ApplyU (IdentifierU (Name "part")) (StringU "tag")) (ListU [StringU "boop"])) (ListU [IdentifierU (Name "optional")])]))

tests :: [Test]
tests = [test1]
