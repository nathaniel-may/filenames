module Main where

import CustomPrelude      -- import all
import Schema.ParseTests  (parserTests)
import Schema.TypeTests   (typeTests)
import Test.HUnit         -- import all

allTests :: [Test]
allTests = parserTests <> typeTests

main :: IO ()
main = do
  results <- runTestTT $ TestList allTests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)
