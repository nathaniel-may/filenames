module ParserTests (tests) where

import CustomPrelude -- import all
import Test.HUnit    -- import all
import Types         -- import all
import Parsers       (parse)


test1 :: Test
test1 = TestCase $ assertEqual 
  "list of multiple values parses"
  (Right $ ListU [
        IntU 1
      , BoolU True
      , ListU [
            IntU 2
          , BoolU False
      ]
      , IntU 3
  ])
  (parse "[1, true, [2,  false], 3  ]  ")

tests :: [Test]
tests = [test1]
