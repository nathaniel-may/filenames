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
main = run =<< execParser opts
  where
    opts = info (cli <**> helper)
      ( fullDesc
     <> progDesc "Compiles a value to be printed."
     <> header "this is a header." )

run :: Input -> IO ()
run (Input filepath) = do
    let name = T.pack $ takeBaseName filepath
    source <- T.pack <$> readFile filepath
    case ghcCompile name <$> compile source of
        (Left e) -> print $ display e
        (Right x) -> x

compile :: Text -> Either CompilationException Text
compile source = do
    parsed <- mapLeft ParseErr (parse source)
    checked <- mapLeft TypeErr (typecheck parsed)
    pure (gen checked)

ghcCompile :: Text -> Text -> IO ()
ghcCompile name source = do
    let targetPath = T.unpack $ name <> ".hs"
    writeFile targetPath (T.unpack source)
    callCommand $ "ghc " <> targetPath

cli :: Parser Input
cli = Input <$> argument str (metavar "SOURCE")
