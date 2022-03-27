module TypeCheckerTests (tests) where

import CustomPrelude -- import all
import Exceptions    (TypeException(..))
import Test.HUnit    -- import all
import Types         -- import all
import TypeChecker   -- import all


test1 :: Test
test1 = TestCase $ assertEqual 
  "heterogeneous list fails to typecheck" 
  (Left $ ListTypeMismatch IntTag StringTag)
  (typecheck (ListU [IntU 0, StringU "hello", StringU "world"]))

test2 :: Test
test2 = TestCase $ assertEqual 
  "heterogeneous list of lists fails to typecheck" 
  (Left $ ListTypeMismatch (ListTag IntTag) (ListTag StringTag))
  (typecheck (ListU [
    ListU [IntU 0]
  , ListU [StringU "hello world"]
  ]))

test3 :: Test
test3 = TestCase $ assertEqual 
  "homogeneous list typechecks" 
  (Right $ ListT StringTag [StringT "hello", StringT "world"])
  (typecheck (ListU [StringU "hello", StringU "world"]))

test4 :: Test
test4 = TestCase $ assertEqual 
  "top-level non-assignment fails to typecheck" 
  (Left TopLevelNotAssignment)
  (typecheckFromRoot (BoolU True))

test5 :: Test
test5 = TestCase $ assertEqual 
  "normal format assignment typechecks from root" 
  (Right $ BoolT True)
  (typecheckFromRoot (BodyU [AssignmentU (Name "format") (BoolU True)]))

test6 :: Test
test6 = TestCase $ assertEqual 
  "non-partial application of builtin '<' typechecks" 
  (Right $ FnT (Name "<") BoolTag [IntT 1, IntT 2])
  (typecheck $ ApplyU (ApplyU (IntU 1) (InfixIdentifierU $ Name "<")) (IntU 2))

test7 :: Test
test7 = TestCase $ assertEqual 
  "partial application of builtin '<' typechecks" 
  (Right $ FnT (Name "<") (FnTag IntTag BoolTag) [IntT 1])
  (typecheck $ ApplyU (IntU 1) (InfixIdentifierU $ Name "<"))

tests :: [Test]
tests = [test1, test2, test3, test4, test5, test6, test7]
