
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (lex)
import Test.HUnit
import Text.Megaparsec (parse)
import System.Exit

import Filenames


schema :: [Text]
schema = [
    "abc"
  , "de"
  , "fghi"
  , "j"]

test1 :: Test
test1 = TestCase $ assertEqual 
  "Lexes schema concatinated with numeric id" 
  (Right [Tag "abc",Tag "de",Tag "fghi",Tag "j",Id "123324"])
  (parse (pFilename schema) "unit-test" "abc-de-fghi-j-123324")

test2 :: Test
test2 = TestCase $ assertEqual 
  "Lexes schema concatinated with alpha id" 
  (Right [Tag "abc",Tag "fghi",Tag "j",Id "ABCDEF"])
  (parse (pFilename schema) "unit-test" "abc-fghi-j-ABCDEF")

test3 :: Test
test3 = TestCase $ assertEqual 
  "Lexes schema concatinated with alpha id and counter" 
  (Right [Tag "abc",Tag "de",Tag "fghi",Tag "de",Tag "j",Tag "j",Id "ABCDEF",Count 3])
  (parse (pFilename schema) "unit-test" "abc-de-fghi-de-j-j-ABCDEF-3")

allTests :: [Test]
allTests = [test1, test2, test3]

main :: IO ()
main = do
  results <- runTestTT $ TestList allTests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)