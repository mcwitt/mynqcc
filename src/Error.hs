module Error where

-- | TODO: Add line positions to lexer and parser errors
data Error = LexerError String
           | ParserError String
           | CodegenError String
           | UnsupportedOSError String
  deriving (Eq, Show)
