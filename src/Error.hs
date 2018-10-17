module Error ( Error (LexerError, ParserError)
             ) where

-- | TODO: Add line positions to lexer and parser errors
data Error = LexerError String
           | ParserError String
  deriving (Eq, Show)
