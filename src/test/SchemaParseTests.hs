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

parserTests :: [Test]
parserTests = [test1]