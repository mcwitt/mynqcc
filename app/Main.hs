module Main where

import           Control.Arrow                  ( left )
import           Data.List                      ( intercalate )
import           Options.Applicative
import qualified System.Info
import           System.IO                      ( IOMode(ReadMode)
                                                , withFile
                                                , readFile
                                                )

import           System.FilePath.Posix          ( dropExtension )

import qualified Codegen                       as C
import qualified Lexer                         as L
import qualified Parser                        as P
import qualified Target                        as T
import qualified Validation                    as V

data CmdOptions = CmdOptions { verbose    :: Bool
                             , outputFile :: String
                             , inputFiles :: [String]}

newtype UnsupportedOsError = UnsupportedOs String deriving (Eq, Show)

data CompilerError = LexerError L.LexerError
                   | ParserError P.ParserError
                   | CodegenError C.CodegenError
                   | UnsupportedOsError UnsupportedOsError
                   | ValidationError V.ValidationError
                   deriving (Eq, Show)

cmdOptions :: Parser CmdOptions
cmdOptions =
  CmdOptions
    <$> switch
          (long "verbose" <> short 'v' <> help "Print debugging information")
    <*> strOption
          (long "outputFile" <> short 'o' <> value "a.s" <> showDefault <> help
            "Output file"
          )
    <*> some (argument str (metavar "FILES..."))


main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    (cmdOptions <**> helper)
    (  fullDesc
    <> progDesc "Compile a (small but growing) subset of C to x86 assembly"
    <> header "mynqcc - an educational C compiler in Haskell"
    )


-- | Parse a C source file, generate code, and write to an output file.
run :: CmdOptions -> IO ()
run opts = do
  src <- readFile $ head . inputFiles $ opts
  case compile src of
    Right code                            -> writeFile (outputFile opts) code
    Left  (LexerError (L.LexerError msg)) -> putStrLn $ "Lexer error: " ++ msg
    Left (ParserError (P.ParserError msg)) ->
      putStrLn $ "Parser error: " ++ msg
    Left (CodegenError (C.CodegenError msg)) ->
      putStrLn $ "Codegen error: " ++ msg
    Left (UnsupportedOsError (UnsupportedOs msg)) ->
      putStrLn $ "Unsupported OS: " ++ msg
    Left (ValidationError (V.ValidationError msg)) ->
      putStrLn $ "Validation error: " ++ msg

hostOs :: Either UnsupportedOsError T.OS
hostOs = case System.Info.os of
  "linux"  -> Right T.Linux
  "darwin" -> Right T.Darwin
  name     -> Left $ UnsupportedOs name

compile :: String -> Either CompilerError String
compile src = do
  tokens <- left LexerError (L.lexString src)
  ast    <- left ParserError (P.parseTokens tokens)
  os     <- left UnsupportedOsError hostOs
  _      <- left ValidationError (V.validate ast)
  code   <- left CodegenError (C.generate (T.Target os) ast)
  return $ intercalate "\n" code
