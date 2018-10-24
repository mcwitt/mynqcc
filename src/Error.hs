module Error ( Error ( LexerError
                     , ParserError
                     , CodegenError)
             ) where

-- | TODO: Add line positions to lexer and parser errors
data Error = LexerError String
           | ParserError String
           | CodegenError String
  deriving (Eq, Show)
