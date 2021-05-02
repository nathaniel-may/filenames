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

test5 :: Test
test5 = TestCase $ assertEqual
  "Parses List of Strings"
  (Right $ ListU [StringU "hello", StringU "world"])
  (parse expr "unit-test" "[\"hello\", \"world\"]")

test6 :: Test
test6 = TestCase $ assertEqual
  "Parses List of Chars"
  (Right $ ListU [CharU (Just 'a'), CharU (Just 'b'), CharU (Just 'c')])
  (parse expr "unit-test" "['a','b','c']")

test7 :: Test
test7 = TestCase $ assertEqual
  "Parses empty List"
  (Right $ ListU [])
  (parse expr "unit-test" "[]")

parserTests :: [Test]
parserTests = [test1, test2, test3, test4, test5, test6]