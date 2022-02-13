module RuntimeTests where

import           CustomPrelude -- import all
import           Runtime       (P, p, parse)
import qualified Data.Set      as Set
import           Test.HUnit    -- import all


simpleParser :: P
simpleParser = p0 <> p1 where
    p0 = p "medium" (==1) (Set.fromList ["art", "photo"])
    p1 = p "subject" (>=1) (Set.fromList ["nature", "people", "architecture"])

test1 :: Test
test1 = TestCase $ assertEqual 
  "Correctly parser extracts values on valid input" 
  (Just [("medium",["photo"]),("subject",["nature","people"])])
  (parse simpleParser '-' "photo-nature-people")

test2 :: Test
test2 = TestCase $ assertEqual 
  "Correctly parser extracts values on valid input" 
  (Just [("medium",["art"]),("subject",["nature"])])
  (parse simpleParser '-' "art-nature")

runtimeTests :: [Test]
runtimeTests = [test1, test2]