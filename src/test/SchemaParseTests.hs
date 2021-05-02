module SchemaParseTests where

import           CustomPrelude   -- import all
import           Parsers         -- import all
import           Test.HUnit      -- import all
import           Text.Megaparsec (parse)


test1 :: Test
test1 = TestCase $ assertEqual
  "Parses Strings"
  (Right $ StringU "str")
  (parse expr "unit-test" "\"str\"")

test2 :: Test
test2 = TestCase $ assertEqual
  "Parses Chars"
  (Right . CharU . Just $ 'c')
  (parse expr "unit-test" "'c'")

test3 :: Test
test3 = TestCase $ assertBool
  "single quotes with too many chars fail"
  (isLeft $ parse expr "unit-test" "'abc'")

test4 :: Test
test4 = TestCase $ assertEqual
  "Parses Empty Char"
  (Right . CharU $ Nothing)
  (parse expr "unit-test" "''")

parserTests :: [Test]
parserTests = [test1, test2, test3, test4]