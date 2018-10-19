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

parseTokens :: [Token] -> Either Error Program
parseTokens ts = case parse program ts of
                   [(res, xs)] -> Right res
                   [] -> Left $ ParserError "Failed to parse the program."

program :: Parser Token Program
program = do f <- function
             return (Program f)

function :: Parser Token Function
function = do atom KWInt
              name <- identifier
              atom OpenParen
              atom CloseParen
              atom OpenBrace
              body <- statement
              atom CloseBrace
              return (Function name body)

identifier :: Parser Token String
identifier = do Identifier name <- satisfy $ \t ->
                  case t of Identifier _ -> True
                            _            -> False
                return name

statement :: Parser Token Statement
statement = do atom KWReturn
               expr <- expression
               atom Semicolon
               return (Return expr)

expression :: Parser Token Expression
expression = term `chainl1` (addition <|> subtraction)

addition :: Parser Token (Expression -> Expression -> Expression)
addition = do atom Token.Addition; return (Binary AST.Addition)

subtraction :: Parser Token (Expression -> Expression -> Expression)
subtraction = do atom Token.Negation; return (Binary AST.Subtraction)

term :: Parser Token Expression
term = factor `chainl1` (multiplication <|> division)

multiplication :: Parser Token (Expression -> Expression -> Expression)
multiplication = do atom Token.Multiplication; return (Binary AST.Multiplication)

division :: Parser Token (Expression -> Expression -> Expression)
division = do atom Token.Division; return (Binary AST.Division)

parenExpr :: Parser Token Expression
parenExpr = do atom OpenParen
               expr <- expression
               atom CloseParen
               return expr

constant :: Parser Token Expression
constant = do Integer i <- satisfy $ \t ->
                case t of Integer _ -> True
                          _         -> False
              return $ AST.Constant i

negation :: Parser Token Expression
negation = do atom Token.Negation
              fact <- factor
              return $ Unary AST.Negation fact

bitwiseComplement :: Parser Token Expression
bitwiseComplement = do atom Token.BitwiseComplement
                       fact <- factor
                       return $ Unary AST.BitwiseComplement fact

logicalNegation :: Parser Token Expression
logicalNegation = do atom Token.LogicalNegation
                     fact <- factor
                     return $ Unary AST.LogicalNegation fact

unaryOperation :: Parser Token Expression
unaryOperation = negation
             <|> constant
             <|> bitwiseComplement
             <|> logicalNegation

factor :: Parser Token Expression
factor = parenExpr <|> unaryOperation <|> constant
