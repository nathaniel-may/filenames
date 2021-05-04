module SchemaParseTests where

import           CustomPrelude          -- import all
import           Parsers                -- import all
import           Prelude                (String)
import           Test.HUnit             -- import all


testEq :: String -> Expr -> Text -> Test
testEq name expected input = TestCase $ assertEqual
  name
  (Right expected)
  (runParse "unit-test" input)

testFails :: String -> Text -> Test
testFails name input = TestCase $ assertBool
  name
  (isLeft $ runParse "unit-test" input)


test1 :: Test
test1 = testEq
  "Parses Strings"
  (StringU "str")
  "\"str\""

test2 :: Test
test2 = testEq
  "Parses Chars"
  (CharU . Just $ 'c')
  "'c'"

test3 :: Test
test3 = testFails
  "single quotes with too many chars fail"
  "'abc'"

test4 :: Test
test4 = testEq
  "Parses Empty Char"
  (CharU $ Nothing)
  "''"

test5 :: Test
test5 = testEq
  "Parses List of Strings"
  (ListU [StringU "hello", StringU "world"])
  "[\"hello\", \"world\"]"

test6 :: Test
test6 = testEq
  "Parses List of Chars"
  (ListU [CharU (Just 'a'), CharU (Just 'b'), CharU (Just 'c')])
  "['a','b','c']"

test7 :: Test
test7 = testEq
  "Parses empty List"
  (ListU [])
  "[]"

parserTests :: [Test]
parserTests = [test1, test2, test3, test4, test5, test6]