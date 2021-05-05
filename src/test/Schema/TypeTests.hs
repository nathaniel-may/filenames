module Schema.TypeTests where

import           CustomPrelude      -- import all
import           Schema.Exceptions  -- import all
import           Schema.Parser      (runParse)
import           Schema.TypeChecker (typecheck)
import           Schema.Types       -- import all
import           Prelude            (String)
import           Test.HUnit         -- import all


runParse' :: Source -> Either CompileException Expr
runParse' source = mapLeft ParseE $ runParse source

typecheck' :: Expr -> Either CompileException ExprT
typecheck' e = mapLeft TypeE $ typecheck e

testEq :: String -> ExprT -> Text -> Test
testEq name expected input = TestCase $ assertEqual
    name
    (Right expected)
    (typecheck' =<< runParse' (Source "unit-test" input))

testFails :: String -> Text -> Test
testFails name input = TestCase $ assertBool
  name
  (isLeft $ typecheck' =<< runParse' (Source "unit-test" input))


test1 :: Test
test1 = testEq
  "List of Strings typechecks"
  (List StringTag [String "hello", String "world"])
  "[\"hello\", \"world\"]"

test2 :: Test
test2 = testFails
  "fails to typecheck heterogeneous list"
  "[\"string\", 'c']"

test3 :: Test
test3 = testEq
  "Empty List typechecks"
  -- All empty lists are lists of strings which will need to change when I add functions
  (List StringTag [])
  "[]"

test4 :: Test
test4 = testEq
  "List of Chars typechecks"
  (List CharTag [Char 'x', Char 'y'])
  "['x', 'y']"

typeTests :: [Test]
typeTests = [test1, test2, test3, test4]