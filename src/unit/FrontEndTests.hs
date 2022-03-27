-- This module is for asserting when errors should be caught by parsing vs typechecking

module FrontEndTests where

import           CustomPrelude -- import all
import           Data.String   (String)
import qualified Data.Text     as T
import           Exceptions    -- import all
import           Test.HUnit    -- import all
import           Parsers       (parse)
import           TypeChecker   (typecheckFromRoot)
import           Types         -- import all


assertEqual' :: (Eq a, Eq b, Show a, Show b, Display b) => String -> Either b a -> Either b a -> Assertion
assertEqual' _ (Right _) (Left err) = assertString (T.unpack $ display err)
assertEqual' name expected v = assertEqual name expected v

assertTypeErr :: String -> TypeException -> Text -> Assertion
assertTypeErr name expected source = assertEqual name (Left $ TypeErr expected) (parseNTypecheck source)

parseNTypecheck :: Text -> Either CompilationException ExprT
parseNTypecheck source = do
    parsed <- mapLeft ParseErr (parse source)
    mapLeft TypeErr (typecheckFromRoot parsed)

test1 :: Test
test1 = TestCase $ assertTypeErr 
  "top-level must be assignment is a type error"
  TopLevelNotAssignment
  "let format := true\nfalse"

test2 :: Test
test2 = TestCase $ assertTypeErr 
  "format not found is a type error"
  FormatNotFound
  "let boop := true"

test3 :: Test
test3 = TestCase $ assertEqual' 
  "operator function application works"
  (Right $ FnT (Name "<") BoolTag [IntT 10, IntT 20])
  (parseNTypecheck "let format := {10 < 20}")

test4 :: Test
test4 = TestCase $ assertEqual' 
  "operator right partial application works"
  (Right $ ApplyT (FlipT (FnT (Name ">=") (FnTag IntTag (FnTag IntTag BoolTag)) [])) (IntT 3))
  (parseNTypecheck "let format := {>= 3}")

tests :: [Test]
tests = [test1, test2, test3, test4]
