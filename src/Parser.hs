module Parser ( parseTokens
              ) where

import ParserCombinators ( Parser (..)
                         , satisfy
                         , atom
                         )

import Token (Token (..))

import AST ( Expression (Constant)
           , Statement  (Return)
           , Function   (Function)
           , Program    (Program)
           )

import Error (Error (ParserError))

integer :: Parser Token Token
integer = satisfy $ \t -> case t of Integer _ -> True
                                    _         -> False

expression :: Parser Token Expression
expression = do Integer int <- integer
                return (Constant int)

statement :: Parser Token Statement
statement = do atom KWReturn
               expr <- expression
               atom Semicolon
               return (Return expr)

identifier :: Parser Token Token
identifier = satisfy $ \t -> case t of
                               Identifier _ -> True
                               _            -> False

function :: Parser Token Function
function = do atom KWInt
              Identifier name <- identifier
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
