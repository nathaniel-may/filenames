module Exe where

import           Data.Functor        (void)
import           Data.Maybe          (catMaybes)
import           Data.Text           hiding (null)
import qualified Data.Text           as T
import           Data.Text.IO        as T
import           Data.Void           (Void)
import           Filenames           hiding (Parser)
import qualified Filenames
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
    let pFails = catMaybes $ (filenameAndParseError (pFilename tags)) <$> filenames
    let pFailNames = snd <$> pFails
    let pFailErrs = fst <$> pFails
    if null pFails
    then putStrLn "Everything matches the schema!"
    else do
        putStrLn "-mismatching filenames found-"
        if dFlag
        then void $ mapM (putStrLn . T.pack . errorBundlePretty) pFailErrs
        else void $ mapM putStrLn pFailNames

-- parsing happens here
filenameAndParseError :: Filenames.Parser a -> Text -> Maybe (ParseErrorBundle Text Void, Text)
filenameAndParseError p fname = 
    either (\e -> Just (e, fname)) (\_ -> Nothing) (parse p (T.unpack fname) fname)