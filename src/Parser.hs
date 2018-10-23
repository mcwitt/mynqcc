module Parser ( parseTokens
              ) where

import ParserCombinators
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
              body <- many statement
              atom CloseBrace
              return (Function name body)


-- Statements

statement :: Parser Token Statement
statement = returnStatement <|> declaration <|> standaloneExpr

returnStatement :: Parser Token Statement
returnStatement = do atom KWReturn
                     expr <- expression
                     atom Semicolon
                     return (Return expr)

declaration :: Parser Token Statement
declaration = do atom KWInt
                 name <- identifier
                 initializer <- optional (do atom Token.Assignment; expression)
                 atom Semicolon
                 return (Declaration name initializer)

standaloneExpr :: Parser Token Statement
standaloneExpr = do expr <- expression
                    atom Semicolon
                    return (Expression expr)

-- Expressions

expression :: Parser Token Expression
expression = assignment <|> logicalOrExpr

assignment :: Parser Token Expression
assignment = do name <- identifier
                atom Token.Assignment
                expr <- expression
                return (AST.Assignment name expr)

logicalOrExpr :: Parser Token Expression
logicalOrExpr = logicalAndExpr `chainl1` logicalOr

logicalAndExpr :: Parser Token Expression
logicalAndExpr = equalityExpr `chainl1` logicalAnd

equalityExpr :: Parser Token Expression
equalityExpr = relationalExpr `chainl1` (equality <|> inequality)

relationalExpr :: Parser Token Expression
relationalExpr = additiveExpr `chainl1` ( lessThan
                                      <|> greaterThan
                                      <|> lessEqual
                                      <|> greaterEqual)

additiveExpr :: Parser Token Expression
additiveExpr = term `chainl1` (addition <|> subtraction)

term :: Parser Token Expression
term = factor `chainl1` (multiplication <|> division)

factor :: Parser Token Expression
factor = parenExpr
     <|> unaryOperation
     <|> constant
     <|> reference

parenExpr :: Parser Token Expression
parenExpr = do atom OpenParen
               expr <- expression
               atom CloseParen
               return expr

unaryOperation :: Parser Token Expression
unaryOperation = negation
             <|> bitwiseComplement
             <|> logicalNegation

constant :: Parser Token Expression
constant = do Integer i <- satisfy $ \t ->
                case t of Integer _ -> True
                          _         -> False
              return $ AST.Constant i

reference :: Parser Token Expression
reference = do name <- identifier
               return $ Reference name

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


-- Operators

multiplication :: Parser Token (Expression -> Expression -> Expression)
multiplication = do atom Token.Multiplication; return $ Binary AST.Multiplication

division :: Parser Token (Expression -> Expression -> Expression)
division = do atom Token.Division; return $ Binary AST.Division

addition :: Parser Token (Expression -> Expression -> Expression)
addition = do atom Token.Addition; return $ Binary AST.Addition

subtraction :: Parser Token (Expression -> Expression -> Expression)
subtraction = do atom Token.Negation; return $ Binary AST.Subtraction

lessThan :: Parser Token (Expression -> Expression -> Expression)
lessThan = do atom Token.LessThan; return $ Binary AST.LessThan

greaterThan :: Parser Token (Expression -> Expression -> Expression)
greaterThan = do atom Token.GreaterThan; return $ Binary AST.GreaterThan

lessEqual :: Parser Token (Expression -> Expression -> Expression)
lessEqual = do atom Token.LessEqual; return $ Binary AST.LessEqual

greaterEqual :: Parser Token (Expression -> Expression -> Expression)
greaterEqual = do atom Token.GreaterEqual; return $ Binary AST.GreaterEqual

equality :: Parser Token (Expression -> Expression -> Expression)
equality = do atom Token.Equality; return $ Binary AST.Equality

inequality :: Parser Token (Expression -> Expression -> Expression)
inequality = do atom Token.Inequality; return $ Binary AST.Inequality

logicalAnd :: Parser Token (Expression -> Expression -> Expression)
logicalAnd = do atom Token.LogicalAnd; return $ Binary AST.LogicalAnd

logicalOr :: Parser Token (Expression -> Expression -> Expression)
logicalOr = do atom Token.LogicalOr; return $ Binary AST.LogicalOr


-- Identifier

identifier :: Parser Token String
identifier = do Identifier name <- satisfy $ \t ->
                  case t of Identifier _ -> True
                            _            -> False
                return name
