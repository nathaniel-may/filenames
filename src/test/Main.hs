module Main where

import           CustomPrelude -- import all
import qualified RuntimeTests
import qualified TypeCheckerTests
import           Test.HUnit    -- import all

allTests :: [Test]
allTests = TypeCheckerTests.tests <> RuntimeTests.tests

main :: IO ()
main = do
  results <- runTestTT $ TestList allTests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)
