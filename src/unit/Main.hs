module Main where

import           CustomPrelude -- import all
import qualified FrontEndTests
import qualified ParserTests
import qualified RuntimeTests
import qualified TypeCheckerTests
import           Test.HUnit    -- import all


allTests :: [Test]
allTests = TypeCheckerTests.tests <> RuntimeTests.tests <> ParserTests.tests <> FrontEndTests.tests

main :: IO ()
main = do
  results <- runTestTT $ TestList allTests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)
