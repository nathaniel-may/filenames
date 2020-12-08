module Exe where

import           Control.Monad       (ap)
import           Data.Bitraversable  (bitraverse)
import           Data.Either         (isRight)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.IO        (putStrLn)
import qualified Data.Text.IO        as T
import           Options.Applicative
import           Parsers             hiding (Parser)
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
    schema' <- T.readFile s
    let tags = T.lines schema'
    let parsed = preserving (parse (filename tags) =<< T.unpack) <$> filenames
    -- bind is filter, mempty :: Monoid b => a -> b
    let pFails = bitraverse pure (either pure mempty) =<< parsed
    let valid = length $ filter (isRight . snd) parsed
    let invalid = length pFails
    putStrLn $ tshow valid <> " valid filenames found."
    putStrLn $ tshow invalid <> " invalid filenames found."
    if null pFails
    then putStrLn "Everything matches the schema!"
    else if dFlag
        then mapM_ (putStrLn . T.pack . errorBundlePretty) (snd <$> pFails)
        else mapM_ putStrLn (fst <$> pFails)
    putStrLn ""

tshow :: Show a => a -> Text
tshow = T.pack . show

preserving :: (a -> b) -> a -> (a, b)
preserving = ap (,)