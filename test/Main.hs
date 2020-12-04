import Test.HUnit
import System.Exit


exampleTest :: Test
exampleTest = TestCase (assertEqual "msg" True True)

allTests :: [Test]
allTests = [exampleTest]

main :: IO ()
main = do
  results <- runTestTT $ TestList allTests
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)