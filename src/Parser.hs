module Parser ( parseTokens
              ) where

import AST
import Error
import ParserCombinators
import Token

parseTokens :: [Token] -> Either Error Program
parseTokens ts = case parse program ts of
  [(res, [])] -> Right res
  otherwise   -> Left $ ParserError "Failed to parse the program."

program :: Parser Token Program
program = pure Program <*> function

function :: Parser Token Function
function = do atom KWInt
              name <- identifier
              atom OpenParen
              atom CloseParen
              atom OpenBrace
              body <- many blockItem
              atom CloseBrace
              return (Function name body)

-- Block items

blockItem :: Parser Token BlockItem
blockItem = blockDeclaration <|> blockStatement

blockDeclaration :: Parser Token BlockItem
blockDeclaration = pure Declaration <*> declaration

blockStatement :: Parser Token BlockItem
blockStatement = pure Statement <*> statement

-- Declarations

declaration :: Parser Token Declaration
declaration = do
  atom KWInt
  name <- identifier
  initializer <- optional (atom Token.Assignment >> expression)
  atom Semicolon
  return $ Decl name initializer


-- Statements

statement :: Parser Token Statement
statement = ifStatement
            <|> returnStatement
            <|> standaloneExpr
            <|> compoundStatement
            <|> forStatement
            <|> forDeclStatement
            <|> whileStatement
            <|> doStatement
            <|> breakStatement
            <|> continueStatement

ifStatement :: Parser Token Statement
ifStatement = do atom KWIf
                 atom OpenParen
                 expr <- expression
                 atom CloseParen
                 s1 <- statement
                 s2 <- optional (atom KWElse >> statement)
                 return $ If expr s1 s2

returnStatement :: Parser Token Statement
returnStatement = do atom KWReturn
                     expr <- expression
                     atom Semicolon
                     return (Return expr)

standaloneExpr :: Parser Token Statement
standaloneExpr = do expr <- optional expression
                    atom Semicolon
                    return $ Expression expr

compoundStatement :: Parser Token Statement
compoundStatement = do atom OpenBrace
                       items <- many blockItem
                       atom CloseBrace
                       return (Compound items)

forStatement :: Parser Token Statement
forStatement = do atom KWFor
                  atom OpenParen
                  init <- optional expression
                  atom Semicolon
                  cond <- expression <|> return (Constant 1)
                  atom Semicolon
                  post <- optional expression
                  atom CloseParen
                  body <- statement
                  return $ For init cond post body

forDeclStatement :: Parser Token Statement
forDeclStatement = do atom KWFor
                      atom OpenParen
                      init <- declaration
                      cond <- expression <|> return (Constant 1)
                      atom Semicolon
                      post <- optional expression
                      atom CloseParen
                      body <- statement
                      return $ ForDecl init cond post body

whileStatement :: Parser Token Statement
whileStatement = do atom KWWhile
                    atom OpenParen
                    cond <- expression
                    atom CloseParen
                    body <- statement
                    return $ While cond body

doStatement :: Parser Token Statement
doStatement = do atom KWDo
                 body <- statement
                 atom KWWhile
                 atom OpenParen
                 cond <- expression
                 atom CloseParen
                 atom Semicolon
                 return $ Do body cond

breakStatement :: Parser Token Statement
breakStatement = atom KWBreak >> atom Semicolon >> return Break

continueStatement :: Parser Token Statement
continueStatement = atom KWContinue >> atom Semicolon >> return Continue

-- Expressions

expression :: Parser Token Expression
expression = assignment <|> conditionalExpr

assignment :: Parser Token Expression
assignment = do name <- identifier
                atom Token.Assignment
                expr <- expression
                return (AST.Assignment name expr)

conditionalExpr :: Parser Token Expression
conditionalExpr = do e1 <- logicalOrExpr
                     e23 <- optional (do atom QuestionMark
                                         e2 <- expression
                                         atom Colon
                                         e3 <- conditionalExpr
                                         return (e2, e3))
                     return (case e23 of
                               Just (e2, e3) -> Conditional e1 e2 e3
                               Nothing -> e1)

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
term = factor `chainl1` (multiplication <|> division <|> modulo)

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
constant = pure (\(Integer i) -> AST.Constant i) <*>
           satisfy (\t -> case t of Integer _ -> True
                                    _         -> False)

reference :: Parser Token Expression
reference = pure Reference <*> identifier

negation :: Parser Token Expression
negation = atom Token.Negation >>
           pure (Unary AST.Negation) <*>
           factor

bitwiseComplement :: Parser Token Expression
bitwiseComplement = atom Token.BitwiseComplement >>
                    pure (Unary AST.BitwiseComplement) <*>
                    factor

logicalNegation :: Parser Token Expression
logicalNegation = atom Token.LogicalNegation >>
                  pure (Unary AST.LogicalNegation) <*>
                  factor


multiplication :: Parser Token (Expression -> Expression -> Expression)
multiplication = atom Token.Multiplication >>
                 return (Binary AST.Multiplication)

division :: Parser Token (Expression -> Expression -> Expression)
division = atom Token.Division >> return (Binary AST.Division)

modulo :: Parser Token (Expression -> Expression -> Expression)
modulo = atom PercentSign >> return (Binary Modulo)

addition :: Parser Token (Expression -> Expression -> Expression)
addition = atom Token.Addition >> return (Binary AST.Addition)

subtraction :: Parser Token (Expression -> Expression -> Expression)
subtraction = atom Token.Negation >> return (Binary AST.Subtraction)

lessThan :: Parser Token (Expression -> Expression -> Expression)
lessThan = atom Token.LessThan >> return (Binary AST.LessThan)

greaterThan :: Parser Token (Expression -> Expression -> Expression)
greaterThan = atom Token.GreaterThan >> return (Binary AST.GreaterThan)

lessEqual :: Parser Token (Expression -> Expression -> Expression)
lessEqual = atom Token.LessEqual >> return (Binary AST.LessEqual)

greaterEqual :: Parser Token (Expression -> Expression -> Expression)
greaterEqual = atom Token.GreaterEqual >> return (Binary AST.GreaterEqual)

equality :: Parser Token (Expression -> Expression -> Expression)
equality = atom Token.Equality >> return (Binary AST.Equality)

inequality :: Parser Token (Expression -> Expression -> Expression)
inequality = atom Token.Inequality >> return (Binary AST.Inequality)

logicalAnd :: Parser Token (Expression -> Expression -> Expression)
logicalAnd = atom Token.LogicalAnd >> return (Binary AST.LogicalAnd)

logicalOr :: Parser Token (Expression -> Expression -> Expression)
logicalOr = atom Token.LogicalOr >> return (Binary AST.LogicalOr)


-- Identifier

identifier :: Parser Token String
identifier = do Identifier name <- satisfy $ \t ->
                  case t of Identifier _ -> True
                            _            -> False
                return name
