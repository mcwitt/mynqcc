module Main where

import Lib
import Options.Applicative
import System.IO ( IOMode (ReadMode)
                 , withFile
                 , readFile
                 )

import System.FilePath.Posix (dropExtension)

import qualified Lexer (lex)
import Parser (parse_)
import Codegen (generate)

data CmdOptions = CmdOptions { verbose    :: Bool
                             , outputFile :: Maybe String
                             , inputFiles :: [String]
                             }

cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Print debugging information" )
  <*> optional (strOption
      ( long "outputFile"
     <> short 'o'
     <> help "Output file"))
  <*> some (argument str (metavar "FILES..."))

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (cmdOptions <**> helper)
      ( fullDesc
     <> progDesc "Compile a (small but growing) subset of C to x86 assembly"
     <> header "mynqcc - an educational C compiler in Haskell")


-- | Parse a C source file, generate code, and write to an output file.
-- | TODO: Shouldn't generate empty output file if parsing fails.
run :: CmdOptions -> IO ()
run opts = do
  src <- readFile $ firstInputFile opts
  code <- return $ (generate . parse_ . Lexer.lex) src
  writeFile (outputFile' opts) code
  where
    firstInputFile = head . inputFiles
    outputFile' = case outputFile opts of
                    Just xs -> \_ -> xs
                    Nothing -> (++".s") . dropExtension . firstInputFile
