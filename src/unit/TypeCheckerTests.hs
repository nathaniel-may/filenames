module TypeCheckerTests (tests) where

import           CustomPrelude -- import all
import           Data.String   (String)
import qualified Data.Text     as T
import           Exceptions    (CompilationException(..), TypeException(..))
import           Parsers       (parse)
import           Test.HUnit    -- import all
import           Types         -- import all
import           TypeChecker   -- import all

assertRight :: Display a => String -> Either a b -> Assertion
assertRight title (Left err) = assertString (title <> "\n" <> T.unpack (display err))
assertRight title _ = assertBool title True

typecheckSource :: Text -> Either CompilationException ExprT
typecheckSource source = do
    parsed <- mapLeft ParseErr (parse source)
    mapLeft TypeErr (typecheck parsed)

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
  "fncall to builtin '<' typechecks" 
  (Right $ FnCallT (Name "<") BoolTag [IntT 1, IntT 2])
  (typecheck $ FnCallU (Name "<") [IntU 1, IntU 2])

test5 :: Test
test5 = TestCase $ assertRight 
  "lambda with builtin '<' typechecks"
  (typecheckSource "format := []\n lambda := \\x y => x < y")

tests :: [Test]
tests = [test1, test2, test3, test4, test5]
