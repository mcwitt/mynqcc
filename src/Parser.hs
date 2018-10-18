module Parser ( parseTokens
              ) where

import ParserCombinators ( Parser (..)
                         , item
                         , satisfy
                         , atom
                         , chainl1
                         , (<|>)
                         )

import Token
import AST
import Error

factorExpr = do atom OpenParen
                expr <- expression
                atom CloseParen
                return (AST.Factor expr)

constant = do Integer int <- satisfy $ \t ->
                case t of Integer _ -> True
                          _         -> False
              return (AST.Constant int)

negation = do atom Token.Negation
              fact <- factor
              return (AST.Negation fact)

bitwiseComplement = do atom Token.BitwiseComplement
                       fact <- factor
                       return (AST.BitwiseComplement fact)

logicalNegation = do atom Token.LogicalNegation
                     fact <- factor
                     return (AST.LogicalNegation fact)

factor = factorExpr
     <|> constant
     <|> negation
     <|> bitwiseComplement
     <|> logicalNegation

multiplication = do atom Token.Multiplication
                    return AST.Multiplication

division = do atom Token.Division
              return AST.Division

term = fmap AST.Term factor
       `chainl1` (multiplication <|> division)

addition = do atom Token.Addition
              return AST.Addition

subtraction = do atom Token.Negation
                 return AST.Subtraction

expression = fmap AST.Expression term
             `chainl1` (addition <|> subtraction)

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
