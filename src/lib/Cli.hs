module Cli where

import           CodeGen               (gen)
import           CustomPrelude         -- import all
import qualified Data.Text as T
import           Exceptions            (CompilationException(..))
import           Options.Applicative   -- import all
import           Parsers               (parse)
import           System.IO             (readFile, writeFile)
import           System.FilePath.Posix (takeBaseName)
import           System.Process        (callCommand)
import           TypeChecker           (typecheck)


newtype Input = Input
  { build :: FilePath }

main :: IO ()
main = do
    e <- run =<< execParser opts
    case e of
        (Left err) -> print $ display err
        (Right x) -> x 

opts :: ParserInfo Input
opts = info (cli <**> helper)
    ( fullDesc
    <> progDesc "Compiles a value to be printed."
    <> header "this is a header." )

-- outputs an executable from source
run :: Input -> IO (Either CompilationException (IO ()))
run (Input filepath) = do
    let name = T.pack $ takeBaseName filepath
    source <- T.pack <$> readFile filepath
    pure $ ghcCompile name <$> compile source

-- outputs codegen from source
compile :: Text -> Either CompilationException Text
compile source = do
    parsed <- mapLeft ParseErr (parse source)
    checked <- mapLeft TypeErr (typecheck parsed)
    pure (gen checked)

-- writes codegen string to a file and runs system ghc on it
ghcCompile :: Text -> Text -> IO ()
ghcCompile name source = do
    let targetPath = T.unpack $ name <> ".hs"
    writeFile targetPath (T.unpack source)
    callCommand $ "ghc " <> targetPath

-- cli parser
cli :: Parser Input
cli = Input <$> argument str (metavar "SOURCE")
