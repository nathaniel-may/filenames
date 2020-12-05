module Main where

import Options.Applicative

data Input = Input
  { schema :: FilePath
  , dir    :: FilePath }

input :: Parser Input
input = Input
      <$> argument str
          ( metavar "SCHEMA-FILE"
         <> help "Schema file describing name structure" )
      <*> argument str
          ( metavar "DIR"
         <> help "Directory to check filenames" )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (input <**> helper)
      ( fullDesc
     <> progDesc "Parse filenames according to schema file"
     <> header ":::: filename parser ::::" )

run :: Input -> IO ()
run (Input _ _) = putStrLn "haven't hooked up the app to the interface yet"