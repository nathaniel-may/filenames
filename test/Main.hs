
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (lex)
import Test.HUnit
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
  "Lexes schema concatinated" 
  (Right [(Tok "abc","abc"), (Tok "de","de"), (Tok "fghi","fghi"), (Tok "j","j")])
  (lex schema (T.intercalate "-" schema))

test2 :: Test
test2 = TestCase $ assertEqual 
  "Lexes schema concatinated with id and counter" 
  (Right [(Tok "abc","abc"),(Tok "de","de"),(Tok "fghi","fghi"),(Tok "j","j"),(Id,"ABCDEF")]) 
  (lex schema ((T.intercalate "-" schema) <> "-ABCDEF"))

test3 :: Test
test3 = TestCase $ assertEqual 
  "Lexes schema concatinated with id and counter" 
  (Right [(Tok "abc","abc"),(Tok "de","de"),(Tok "fghi","fghi"),(Tok "j","j"),(Id,"ABCDEF"),(Count 3,"3")]) 
  (lex schema ((T.intercalate "-" schema) <> "-ABCDEF-3"))

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