module RuntimeTests where

import           CustomPrelude -- import all
import           Runtime       -- import all
import qualified Data.Set      as Set
import           Test.HUnit    -- import all


simpleParser :: Parser Filename
simpleParser = Filename <$> sequence [
    p "medium"  (==1) (Set.fromList ["art", "photo"])
  , p "subject" (>=1) (Set.fromList ["nature", "people", "architecture"])
  ]

test1 :: Test
test1 = TestCase $ assertEqual 
  "Correctly parser extracts values on valid input" 
  (Right $ Filename [
      TagGroup "medium"  ["photo"]
    , TagGroup "subject" ["nature","people"]
  ])
  (parse simpleParser '-' "photo-nature-people")

test2 :: Test
test2 = TestCase $ assertEqual 
  "Correctly parser extracts values on valid input" 
  (Right $ Filename [
      TagGroup "medium"  ["art"]
    , TagGroup "subject" ["nature"]
  ])
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
