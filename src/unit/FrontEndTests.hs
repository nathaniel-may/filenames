-- This module is for asserting when errors should be caught by parsing vs typechecking

module FrontEndTests where

import           CustomPrelude -- import all
import           Data.String   (String)
import qualified Data.Text     as T
import           Exceptions    -- import all
import           Test.HUnit    -- import all
import           Parsers       (parse)
import           TypeChecker   (typecheck)
import           Types         -- import all


assertEqual' :: (Eq a, Eq b, Show a, Show b, Display b) => String -> Either b a -> Either b a -> Assertion
assertEqual' _ (Right _) (Left err) = assertString (T.unpack $ display err)
assertEqual' name expected v = assertEqual name expected v

assertTypeErr :: String -> TypeException -> Text -> Assertion
assertTypeErr name expected source = assertEqual name (Left $ TypeErr expected) (parseNTypecheck source)

assertTypeChecks :: String -> Text -> Assertion
assertTypeChecks name input = assertBool (name <> "\n" <> show result) (isRight result) where
    result = parseNTypecheck input

parseNTypecheck :: Text -> Either CompilationException ExprT
parseNTypecheck source = do
    parsed <- mapLeft ParseErr (parse source)
    mapLeft TypeErr (typecheck parsed)

test1 :: Test
test1 = TestCase $ assertTypeErr 
  "top-level string literal is a type error"
  TopLevelMustBeSchema
  "\"boop\""

test2 :: Test
test2 = TestCase $ assertTypeChecks
  "schema with one tag part works"
  "schema \"-\" [part \"boop tag\" [\"boop\"] [optional]]"

tests :: [Test]
tests = [test1, test2]
