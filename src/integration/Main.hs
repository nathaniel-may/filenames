module Main where

import           Cli                   (Input(..), run)
import           CustomPrelude         -- import all
import           Data.String           (String)
import qualified Data.Text             as T
import           Exceptions            (CompilationException)
import           System.FilePath.Posix (takeBaseName)
import           System.IO             (readFile, writeFile)
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

-- TODO split test cases out into their own files once there is enough of them

data SourceFile = SourceFile FilePath Text

targetDir :: Text
targetDir = "src/integration/target/"

uniqueFilepaths :: [String]
uniqueFilepaths = (<> T.unpack ".txt") . (\n -> T.unpack targetDir <> T.unpack "test" <> n) . show <$> [0..]

runProgram :: SourceFile -> IO (Either CompilationException (IO Text))
runProgram (SourceFile sourcePath source) = do
    -- write the source file
    writeFile sourcePath (T.unpack source)
    -- compiles source to target file
    x <- run (Input sourcePath)
    let targetPath = takeBaseName sourcePath
    -- run the resulting program
    let stdin' = ""
    (_, stdout', stderr') <- readProcessWithExitCode ("./" <> targetPath) [] stdin'
    -- return stdout only if stderr is empty
    let result = T.pack (stderr' <|> stdout')
    pure . pure . pure $ result

test1 :: FilePath -> IO Test
test1 sourcePath = do
    e <- runProgram (SourceFile sourcePath "[1, 2, 3]")
    output <- case e of
        (Left err) -> pure (display err)
        (Right stdout) -> stdout
    pure . TestCase $ assertEqual 
        "Source to code gen for simple int list works" 
        "[1, 2, 3]"
        output

testsIO :: IO [Test]
testsIO = do 
    callCommand $ T.unpack $ "mkdir -p " <> targetDir
    let allTests = [test1]
    zipWithM ($) allTests uniqueFilepaths
