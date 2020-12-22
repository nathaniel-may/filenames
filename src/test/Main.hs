module Main where

import CustomPrelude -- import all
import LexerTests    (lexerTests)
import Test.HUnit    -- import all

allTests :: [Test]
allTests = lexerTests

main :: IO ()
main = do
  results <- runTestTT $ TestList allTests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)