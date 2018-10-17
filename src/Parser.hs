module Parser ( parseTokens
              ) where

import ParserCombinators ( Parser (..)
                         , item
                         , satisfy
                         , atom
                         , (<|>)
                         )

import Token
import AST
import Error

constant :: Parser Token Expression
constant = do Integer int <- satisfy $ \t ->
                case t of Integer _ -> True
                          _         -> False
              return (Constant int)

negation :: Parser Token Expression
negation = do atom Token.Negation
              expr <- expression
              return (AST.Negation expr)

bitwiseComplement = do atom Token.BitwiseComplement
                       expr <- expression
                       return (AST.BitwiseComplement expr)

logicalNegation = do atom Token.LogicalNegation
                     expr <- expression
                     return (AST.LogicalNegation expr)

expression :: Parser Token Expression
expression = constant
         <|> negation
         <|> bitwiseComplement
         <|> logicalNegation

statement :: Parser Token Statement
statement = do atom KWReturn
               expr <- expression
               atom Semicolon
               return (Return expr)

identifier :: Parser Token String
identifier = do Identifier name <- satisfy $ \t ->
                  case t of Identifier _ -> True
                            _            -> False
                return name

function :: Parser Token Function
function = do atom KWInt
              name <- identifier
              atom OpenParen
              atom CloseParen
              atom OpenBrace
              body <- statement
              atom CloseBrace
              return (Function name body)

program :: Parser Token Program
program = do f <- function
             return (Program f)

parseTokens :: [Token] -> Either Error Program
parseTokens ts = case parse program ts of
                   [(res, xs)] -> Right res
                   [] -> Left $ ParserError "Failed to parse the program."
