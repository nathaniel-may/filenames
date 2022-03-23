module ParserTests (tests) where

import           CustomPrelude   -- import all
import           Data.String     (String)
import qualified Data.Text       as T
import           Exceptions      (ParseException(..))
import qualified Text.Megaparsec as Mega
import           Test.HUnit      -- import all
import           Types           -- import all
import           Parsers         -- import all


assertEqual' :: (Eq a, Eq b, Show a, Show b, Display b) => String -> Either b a -> Either b a -> Assertion
assertEqual' title (Right _) (Left err) = assertString (title <> "\n" <> T.unpack (display err))
assertEqual' name expected v = assertEqual name expected v

assertRight :: (Eq a, Eq b, Show a, Show b, Display b) => String -> Either b a -> Assertion
assertRight title (Left err) = assertString (title <> "\n" <> T.unpack (display err))
assertRight title _ = assertBool title True

test1 :: Test
test1 = TestCase $ assertEqual' 
  "list of multiple values parses"
  (Right $ RootU [AssignmentU (Name "bad_list") (ListU [
        IntU 1
      , BoolU True
      , ListU [
            IntU 2
          , BoolU False
      ]
      , IntU 3
  ])])
  (parse "bad_list := [1, true, [2,    false], 3]  ")

test2 :: Test
test2 = TestCase $ assertEqual' 
  "top-level format parses as an assignment"
  (Right . RootU $ [AssignmentU (Name "format") (ListU [
        IntU 1
      , IntU 2
      , IntU 3
  ])])
  (parse "format := [1, 2, 3]")

test3 :: Test
test3 = TestCase $ assertEqual' 
  "assignment parser works properly"
  (Right $ AssignmentU (Name "name") (BoolU True))
  (mapLeft ParseException $ Mega.parse assignment "" "name := true")

test4 :: Test
test4 = TestCase $ assertEqual' 
  "assignment parser works properly with format name"
  (Right $ AssignmentU (Name "format") (BoolU True))
  (mapLeft ParseException $ Mega.parse assignment "" "format := true")

test5 :: Test
test5 = TestCase $ assertEqual' 
  "parses a lambda with a fn call"
  (Right $ LambdaU 
        [ IdentifierU (Name "x")
        , IdentifierU (Name "y")
        ]
        [ IdentifierU (Name "foo")
        , IdentifierU (Name "x")
        , IdentifierU (Name "y")
        ]
  )
  (mapLeft ParseException $ Mega.parse lambda "" "\\x y => foo x y")

test6 :: Test
test6 = TestCase $ assertRight 
  "parses a lambda with a fn call"
  (parse "format := []\n lambda := (\\x y => foo x y)")

-- test6 :: Test
-- test6 = TestCase $ assertEqual' 
--   "parses a lambda with an operator"
--   (Right $ AssignmentU (Name "format") (BoolU True))
--   (parse "format := [1, 2, 3]")

-- test7 :: Test
-- test7 = TestCase $ assertEqual' 
--   "parses a lambda that returns a literal"
--   (Right $ AssignmentU (Name "format") (BoolU True))
--   (mapLeft ParseException $ Mega.parse assignment "" "format := true")

tests :: [Test]
tests = [test1, test2, test3, test4, test5, test6]
