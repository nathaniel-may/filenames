module Schema.ParseTests where

import           CustomPrelude          -- import all
import           Schema.Parser          (runParse)
import           Schema.Types           -- import all
import           Prelude                (String)
import           Test.HUnit             -- import all


testEq :: String -> Expr -> Text -> Test
testEq name expected input = TestCase $ assertEqual
  name
  (Right expected)
  (runParse (Source "unit-test" input))

testFails :: String -> Text -> Test
testFails name input = TestCase $ assertBool
  name
  (isLeft $ runParse (Source "unit-test" input))


test1 :: Test
test1 = testEq
  "Parses Strings"
  (StringU "str")
  "\"str\""

test2 :: Test
test2 = testEq
  "Parses Chars"
  (CharU 'c')
  "'c'"

test3 :: Test
test3 = testFails
  "single quotes with too many chars fail"
  "'abc'"

test4 :: Test
test4 = testFails
  "fails on empty char"
  "''"

test5 :: Test
test5 = testEq
  "Parses List of Strings"
  (ListU [StringU "hello", StringU "world"])
  "[\"hello\", \"world\"]"

test6 :: Test
test6 = testEq
  "Parses List of Chars"
  (ListU [CharU 'a', CharU 'b', CharU 'c'])
  "['a','b','c']"

test7 :: Test
test7 = testEq
  "Parses empty List"
  (ListU [])
  "[]"

parserTests :: [Test]
parserTests = [test1, test2, test3, test4, test5, test6]