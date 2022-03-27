module ParserTests (tests) where

import           CustomPrelude   -- import all
import           Data.String     (String)
import qualified Data.Text       as T
import           Exceptions      (ParseException(..))
import qualified Text.Megaparsec as Mega
import           Test.HUnit      -- import all
import           Types           -- import all
import           Parsers         (assignment, parse)


assertEqual' :: (Eq a, Eq b, Show a, Show b, Display b) => String -> Either b a -> Either b a -> Assertion
assertEqual' _ (Right _) (Left err) = assertString (T.unpack $ display err)
assertEqual' name expected v = assertEqual name expected v

test1 :: Test
test1 = TestCase $ assertEqual' 
  "list of multiple values parses"
  (Right $ BodyU [AssignmentU (Name "bad_list") (ListU [
        IntU 1
      , BoolU True
      , ListU [
            IntU 2
          , BoolU False
      ]
      , IntU 3
  ])])
  (parse "let bad_list := [1, true, [2,    false], 3]  ")

test2 :: Test
test2 = TestCase $ assertEqual' 
  "top-level format parses as an assignment"
  (Right . BodyU $ [AssignmentU (Name "format") (ListU [
        IntU 1
      , IntU 2
      , IntU 3
  ])])
  (parse "let format := [1, 2, 3]")

test3 :: Test
test3 = TestCase $ assertEqual' 
  "assignment parser works properly"
  (Right $ AssignmentU (Name "name") (BoolU True))
  (mapLeft ParseException $ Mega.parse assignment "" "let name := true")

test4 :: Test
test4 = TestCase $ assertEqual' 
  "assignment parser works properly with format name"
  (Right $ AssignmentU (Name "format") (BoolU True))
  (mapLeft ParseException $ Mega.parse assignment "" "let format := true")

test5 :: Test
test5 = TestCase $ assertEqual' 
  "parses an apply statement"
  (Right . BodyU $ [
    AssignmentU 
      (Name "format")
      (ApplyU (IdentifierU $ Name "x") (IdentifierU $ Name "y"))
  ])
  (parse "let format := {x y}")

test6 :: Test
test6 = TestCase $ assertEqual' 
  "parses a source file with multiple expressions"
  (Right . BodyU $ [BoolU False, IntU 3])
  (parse "false\n3")

test7 :: Test
test7 = TestCase $ assertEqual'
  "parses operator function application with builtin `<`"
  -- Note: infix operator stays infix in the ast till typechecking
  (Right $ BodyU [ApplyU (ApplyU (IntU 1) (InfixIdentifierU (Name "<"))) (IntU 2)])
  (parse "{1 < 2}")

tests :: [Test]
tests = [test1, test2, test3, test4, test5, test6, test7]
