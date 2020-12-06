module Exe where

import           Data.Text
import qualified Data.Text as T
import           Data.Text.IO
import           Data.Void           (Void)
import           Filenames           hiding (Parser)
import           Options.Applicative
import           Prelude             hiding (readFile, putStrLn, lines)
import           System.Directory
import           Text.Megaparsec

data Input = Input
  { details :: Bool 
  , schema  :: FilePath
  , dir     :: FilePath }

-- definitions for input
input :: Parser Input
input = Input
      <$> switch
          ( long "details"
         <> short 'd'
         <> help "Whether to show individual parse errors" )
      <*> argument str
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
run (Input dFlag s d) = do
    putStrLn ""
    filenames' <- listDirectory d
    let filenames = T.pack <$> filenames'
    schema' <- readFile s
    let tags = lines schema'
    let result = mapM (parse (pFilename tags) =<< T.unpack) filenames
    if dFlag
    then either printParseError (\_ -> putStrLn "Everything matches the schema!") result
    else putStrLn "Eventually this will print a list of filenames that don't match." -- TODO

printParseError :: ParseErrorBundle Text Void -> IO()
printParseError bundle = do 
    putStrLn "-mismatching filename found-"
    putStrLn . T.pack $ errorBundlePretty bundle