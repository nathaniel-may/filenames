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

parseNTypecheck :: Text -> Either CompilationException ExprT
parseNTypecheck source = do
    parsed <- mapLeft ParseErr (parse source)
    mapLeft TypeErr (typecheck parsed)

-- TODO this isn't true right now. 
test1 :: Test
test1 = TestCase $ assertTypeErr 
  "top-level must be assignment is a type error"
  (TopLevelNotAssignment BoolTag)
  "true"

tests :: [Test]
-- tests = [test1]
tests = [] -- TODO test something
