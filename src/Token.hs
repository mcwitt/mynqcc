module Token (Token (..)) where

data Token = OpenBrace
           | CloseBrace
           | OpenParen
           | CloseParen
           | Semicolon
           | KWInt
           | KWReturn
           | Identifier String
           | Integer Int
           | Negation
           | BitwiseComplement
           | LogicalNegation
           | Addition
           | Multiplication
           | Division
           deriving (Show, Eq)
