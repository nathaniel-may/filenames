
import           Data.Either     (isRight)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Parsers
import           Prelude         hiding (lex)
import           Test.HUnit
import           Text.Megaparsec (parse)
import           System.Exit


schema :: [Text]
schema = [
    "abc"
  , "de"
  , "fghi"
  , "j"]

test1 :: Test
test1 = TestCase $ assertEqual 
  "Lexes multiple tags with numeric id" 
  (Right [Tag "abc",Tag "de",Tag "fghi",Tag "j",Id "123324"])
  (parse (filename schema) "unit-test" "abc-de-fghi-j-123324.jpg")

test2 :: Test
test2 = TestCase $ assertEqual 
  "Lexes multiple tags with alpha id" 
  (Right [Tag "abc",Tag "fghi",Tag "j",Id "ABCDEF"])
  (parse (filename schema) "unit-test" "abc-fghi-j-ABCDEF.png")

test3 :: Test
test3 = TestCase $ assertEqual 
  "Lexes multiple tags with alpha id and counter" 
  (Right [Tag "abc",Tag "de",Tag "fghi",Tag "de",Tag "j",Tag "j",Id "ABCDEF",Count 3])
  (parse (filename schema) "unit-test" "abc-de-fghi-de-j-j-ABCDEF-3.mp4")

test4 :: Test
test4 = TestCase $ assertEqual 
  "Lexes with only one tag and one id" 
  (Right [Tag "fghi", Id "12345G"])
  (parse (filename schema) "unit-test" "fghi-12345G.pdf")

allTests :: [Test]
allTests = [test1, test2, test3, test4]

main :: IO ()
main = do
  results <- runTestTT $ TestList allTests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)