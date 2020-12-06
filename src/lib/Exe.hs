module Exe where

import           Data.Either         (isLeft)
import           Data.Text
import qualified Data.Text as T
import           Data.Text.IO
import           Filenames           hiding (Parser)
import           Options.Applicative
import           Prelude             hiding (readFile, putStrLn, lines)
import           System.Directory
import           Text.Megaparsec     (parse)

data Input = Input
  { schema :: FilePath
  , dir    :: FilePath }

-- definitions for input
input :: Parser Input
input = Input
      <$> argument str
          ( metavar "SCHEMA-FILE"
         <> help "Schema file describing name structure" )
      <*> argument str
          ( metavar "DIR"
         <> help "Directory to check filenames" )

-- where the interaction with the user takes place
main :: IO ()
main = run =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "Parse filenames according to schema file"
     <> header ":::: filename parser ::::" )

-- where file IO and parsing is done
run :: Input -> IO ()
run (Input s d) = do
    filenames' <- listDirectory d
    let filenames = T.pack <$> filenames'
    schema' <- readFile s
    let tags = lines schema'
    let result = mapM (parse (pFilename tags) "main") filenames
    if isLeft result
    then putStrLn "One of the files doesn't match the schema. Can't tell you which one yet."
    else putStrLn "Everything matches the schema!"