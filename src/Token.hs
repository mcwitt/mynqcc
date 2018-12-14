module Token
  ( Token(..)
  )
where

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
           | LogicalAnd
           | LogicalOr
           | Equality
           | Inequality
           | LessEqual
           | GreaterEqual
           | LessThan
           | GreaterThan
           | Assignment
           | KWIf
           | KWElse
           | Colon
           | QuestionMark
           | PercentSign
           | Comma
           | KWFor
           | KWWhile
           | KWDo
           | KWBreak
           | KWContinue
           deriving (Show, Eq)
