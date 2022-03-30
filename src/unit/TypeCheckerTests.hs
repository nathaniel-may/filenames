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
  "left partial application of builtin '<' typechecks" 
  (Right $ FnT (Name "<") (FnTag IntTag BoolTag) [IntT 1])
  (typecheck $ ApplyU (IntU 1) (InfixIdentifierU $ Name "<"))

-- TODO this exprT can be optimized to move the application inside the FnT and remove the outer ApplyT
test8 :: Test
test8 = TestCase $ assertEqual 
  "right partial application of builtin '<' typechecks" 
  (Right $ ApplyT (FlipT (FnT (Name "<") (FnTag IntTag (FnTag IntTag BoolTag)) [])) (IntT 100))
  (typecheck $ ApplyU (InfixIdentifierU $ Name "<") (IntU 100))

test9 :: Test
test9 = TestCase $ assertEqual 
  "multiple assignment with the same identifier at the same level fails to typecheck" 
  (Left . MultipleAssignmentsWithName $ Name "boop")
  (typecheck $ BodyU [AssignmentU (Name "boop") (BoolU True), AssignmentU (Name "boop") (IntU 1)])

test10 :: Test
test10 = TestCase $ assertEqual 
  "using an identifier that's the same as a builtin fn fails to typecheck" 
  (Left . MultipleAssignmentsWithName $ Name "delim")
  (typecheckFromRoot $ BodyU [AssignmentU (Name "format") (BoolU True), AssignmentU (Name "delim") (IntU 1)])

test11 :: Test
test11 = TestCase $ assertEqual 
  "top level variables can reference each other" 
  (Right UnitT)
  (typecheck $ BodyU [AssignmentU (Name "boooop2") (IntU 1), AssignmentU (Name "boooop") (IdentifierU (Name "boooop2"))])

tests :: [Test]
tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11]
