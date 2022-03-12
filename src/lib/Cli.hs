module Cli where

import CustomPrelude -- import all
import Options.Applicative
import System.IO (readFile)


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
    _ <- readFile filepath
    pure () -- todo stub


cli :: Parser Input
cli = Input <$> argument str (metavar "SOURCE")
