module RuntimeTests where

import           CustomPrelude -- import all
import           Runtime       (P, p, parse, ParseError(..))
import qualified Data.Set      as Set
import           Test.HUnit    -- import all


simpleParser :: P
simpleParser = p0 <> p1 where
    p0 = p "medium" (==1) (Set.fromList ["art", "photo"])
    p1 = p "subject" (>=1) (Set.fromList ["nature", "people", "architecture"])

test1 :: Test
test1 = TestCase $ assertEqual 
  "Correctly parser extracts values on valid input" 
  (Right [("medium",["photo"]),("subject",["nature","people"])])
  (parse simpleParser '-' "photo-nature-people")

test2 :: Test
test2 = TestCase $ assertEqual 
  "Correctly parser extracts values on valid input" 
  (Right [("medium",["art"]),("subject",["nature"])])
  (parse simpleParser '-' "art-nature")

test3 :: Test
test3 = TestCase $ assertEqual 
  "Parser fails on simple invalid input" 
  (Left $ BadMatch "subject" "other")
  (parse simpleParser '-' "art-other")

test4 :: Test
test4 = TestCase $ assertEqual 
  "Parser fails on input with doubled delimiters" 
  (Left $ BadMatch "subject" "")
  (parse simpleParser '-' "art--nature")

test5 :: Test
test5 = TestCase $ assertEqual 
  "Parser fails on input with extra tokens that can't be matched" 
  (Left . UnmatchedTokens $ "art" :| [])
  (parse simpleParser '-' "art-nature-art")

test6 :: Test
test6 = TestCase $ assertEqual 
  "Parser fails on input too many matching tokens" 
  (Left $ TokenMiscount "medium" 2)
  (parse simpleParser '-' "art-photo")

runtimeTests :: [Test]
runtimeTests = [test1, test2, test3, test4, test5, test6]
