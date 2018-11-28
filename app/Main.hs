module Main where

import Data.List ( intercalate)
import Options.Applicative
import System.IO ( IOMode (ReadMode)
                 , withFile
                 , readFile
                 )

import System.FilePath.Posix (dropExtension)

import Codegen ( generate)
import Error   ( Error ( LexerError
                       , ParserError
                       , CodegenError))
import Lexer   ( lexString)
import Parser  ( parseTokens)

data CmdOptions = CmdOptions { verbose    :: Bool
                             , outputFile :: String
                             , inputFiles :: [String]}


cmdOptions :: Parser CmdOptions
cmdOptions = CmdOptions
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "Print debugging information" )
  <*> strOption
      ( long "outputFile"
     <> short 'o'
     <> value "a.s"
     <> showDefault
     <> help "Output file" )
  <*> some (argument str (metavar "FILES..."))


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (cmdOptions <**> helper)
      ( fullDesc
     <> progDesc "Compile a (small but growing) subset of C to x86 assembly"
     <> header "mynqcc - an educational C compiler in Haskell")


-- | Parse a C source file, generate code, and write to an output file.
run :: CmdOptions -> IO ()
run opts = do
  src <- readFile $ head . inputFiles $ opts
  case compile src of
    Right code -> writeFile (outputFile opts) code
    Left (LexerError   msg) -> putStrLn $ "Lexer error: "   ++ msg
    Left (ParserError  msg) -> putStrLn $ "Parser error: "  ++ msg
    Left (CodegenError msg) -> putStrLn $ "Codegen error: " ++ msg


compile :: String -> Either Error String
compile src = do
  tokens <- lexString src
  ast <- parseTokens tokens
  code <- generate ast
  return $ intercalate "\n" code
