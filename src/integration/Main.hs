module Main where

import           Cli                   (Input(..), run)
import           CustomPrelude         -- import all
import           Data.String           (String)
import qualified Data.Text             as T
import           Exceptions            (CompilationException)
import           System.FilePath.Posix (dropExtension)
import           System.IO             (writeFile)
import           System.Process        (callCommand, readProcessWithExitCode)
import           Test.HUnit            -- import all


main :: IO ()
main = do
    tests <- testsIO
    results <- runTestTT $ TestList tests
    if errors results + failures results == 0
        then
            exitSuccess
        else
            exitWith (ExitFailure 1)

data SourceFile = SourceFile FilePath Text

targetDir :: Text
targetDir = "src/integration/target/"

uniqueFilepaths :: [String]
uniqueFilepaths = (<> T.unpack ".txt") . (\n -> T.unpack targetDir <> T.unpack "test" <> n) . (show :: Integer -> String) <$> [0..]

runProgram :: SourceFile -> IO (Either CompilationException (IO Text))
runProgram (SourceFile sourcePath source) = do
    -- write the source file
    writeFile sourcePath (T.unpack source)
    -- compiles source to target file
    e <- run (Input sourcePath)
    case e of
        -- compilation failed
        (Left err) -> pure $ Left err
        -- compilation passed
        (Right postCompileIO) -> do
            -- makes the executable
            postCompileIO
            let targetPath = dropExtension sourcePath
            -- run the resulting program
            let stdin' = ""
            (_, stdout', stderr') <- readProcessWithExitCode targetPath [] stdin'
            -- return stdout only if stderr is empty
            let result = T.pack (stderr' <|> stdout')
            pure . pure . pure $ result

test1 :: FilePath -> IO Test
test1 sourcePath = do
    e <- runProgram (SourceFile sourcePath "format := [\"a\", \"b\", \"c\"]")
    output <- case e of
        (Left err) -> pure (display err)
        (Right stdout) -> stdout
    pure . TestCase $ assertEqual 
        "source for printing a simple list works" 
        "[\"a\",\"b\",\"c\"]\n"
        output

-- top-level value for all tests
testsIO :: IO [Test]
testsIO = do 
    callCommand $ T.unpack $ "mkdir -p " <> targetDir
    -- add new tests here vv
    let allTests = [test1]
    zipWithM ($) allTests uniqueFilepaths
