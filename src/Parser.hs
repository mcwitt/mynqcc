{-# LANGUAGE LambdaCase #-}

module Parser
  ( parseTokens
  )
where

import           AST
import           Control.Monad
import           Data.Functor
import           Data.Maybe
import           Error
import           ParserCombinators
import           Token

parseTokens :: [Token] -> Either Error Program
parseTokens ts = case parse program ts of
  [(res, [])] -> Right res
  _           -> Left $ ParserError "Failed to parse the program."

program :: Parser Token Program
program = Program <$> many function

function :: Parser Token Function
function =
  Function
    <$  atom KWInt
    <*> identifier
    <*> parenthesized (commaDelimitedList (atom KWInt *> identifier))
    <*> (optional . braced) (many blockItem)

-- Block items

blockItem :: Parser Token BlockItem
blockItem = blockDeclaration <|> blockStatement

blockDeclaration :: Parser Token BlockItem
blockDeclaration = Declaration <$> declaration

blockStatement :: Parser Token BlockItem
blockStatement = Statement <$> statement

-- Declarations
-------------------------------------------------------------------------------

declaration :: Parser Token Declaration
declaration =
  Decl
    <$  atom KWInt
    <*> identifier
    <*> optional (atom Token.Assignment *> expression)
    <*  atom Semicolon

-- Statements
-------------------------------------------------------------------------------

statement :: Parser Token Statement
statement =
  ifStatement
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
ifStatement =
  If <$ atom KWIf <*> parenthesized expression <*> statement <*> optional
    (atom KWElse *> statement)

returnStatement :: Parser Token Statement
returnStatement = Return <$> (atom KWReturn *> expression <* atom Semicolon)

standaloneExpr :: Parser Token Statement
standaloneExpr = Expression <$> (optional expression <* atom Semicolon)

compoundStatement :: Parser Token Statement
compoundStatement = Compound <$> braced (many blockItem)

forStatement :: Parser Token Statement
forStatement =
  (\(init, cond, post) body -> For init cond post body)
    <$  atom KWFor
    <*> parenthesized
          (   tuple3
          <$> optional expression
          <*  atom Semicolon
          <*> (expression <|> pure (Constant 1))
          <*  atom Semicolon
          <*> optional expression
          )
    <*> statement

forDeclStatement :: Parser Token Statement
forDeclStatement =
  (\(init, cond, post) body -> ForDecl init cond post body)
    <$  atom KWFor
    <*> parenthesized
          (   tuple3
          <$> declaration
          <*> (expression <|> pure (Constant 1))
          <*  atom Semicolon
          <*> optional expression
          )
    <*> statement

whileStatement :: Parser Token Statement
whileStatement =
  While <$ atom KWWhile <*> parenthesized expression <*> statement

doStatement :: Parser Token Statement
doStatement =
  Do
    <$  atom KWDo
    <*> statement
    <*  atom KWWhile
    <*> parenthesized expression
    <*  atom Semicolon

breakStatement :: Parser Token Statement
breakStatement = Break <$ atom KWBreak <* atom Semicolon

continueStatement :: Parser Token Statement
continueStatement = Continue <$ atom KWContinue <* atom Semicolon

-- Expressions
-------------------------------------------------------------------------------

expression :: Parser Token Expression
expression = assignment <|> conditionalExpr

assignment =
  AST.Assignment <$> identifier <* atom Token.Assignment <*> expression

conditionalExpr =
  (\e1 e23 -> case e23 of
      Just (e2, e3) -> Conditional e1 e2 e3
      Nothing       -> e1
    )
    <$> logicalOrExpr
    <*> optional
          (   tuple2
          <$  atom QuestionMark
          <*> expression
          <*  atom Colon
          <*> conditionalExpr
          )

logicalOrExpr = logicalAndExpr `chainl1` logicalOr

logicalAndExpr = equalityExpr `chainl1` logicalAnd

equalityExpr = relationalExpr `chainl1` (equality <|> inequality)

relationalExpr =
  additiveExpr
    `chainl1` (lessThan <|> greaterThan <|> lessEqual <|> greaterEqual)

additiveExpr = term `chainl1` (addition <|> subtraction)

term = factor `chainl1` (multiplication <|> division <|> modulo)

factor = parenExpr <|> unaryOperation <|> constant <|> funCall <|> reference

parenExpr = parenthesized expression

unaryOperation = negation <|> bitwiseComplement <|> logicalNegation

constant = (\(Integer i) -> AST.Constant i) <$> satisfy
  (\case
    Integer _ -> True
    _         -> False
  )

reference = Reference <$> identifier

funCall =
  FunCall <$> identifier <*> parenthesized (commaDelimitedList expression)

negation = Unary AST.Negation <$ atom Token.Negation <*> factor

bitwiseComplement =
  Unary AST.BitwiseComplement <$ atom Token.BitwiseComplement <*> factor

logicalNegation =
  Unary AST.LogicalNegation <$ atom Token.LogicalNegation <*> factor

-- Binary operators
-------------------------------------------------------------------------------

multiplication :: Parser Token (Expression -> Expression -> Expression)
multiplication = Binary AST.Multiplication <$ atom Token.Multiplication

division = Binary AST.Division <$ atom Token.Division

modulo = Binary Modulo <$ atom PercentSign

addition = Binary AST.Addition <$ atom Token.Addition

subtraction = Binary AST.Subtraction <$ atom Token.Negation

lessThan = Binary AST.LessThan <$ atom Token.LessThan

greaterThan = Binary AST.GreaterThan <$ atom Token.GreaterThan

lessEqual = Binary AST.LessEqual <$ atom Token.LessEqual

greaterEqual = Binary AST.GreaterEqual <$ atom Token.GreaterEqual

equality = Binary AST.Equality <$ atom Token.Equality

inequality = Binary AST.Inequality <$ atom Token.Inequality

logicalAnd = Binary AST.LogicalAnd <$ atom Token.LogicalAnd

logicalOr = Binary AST.LogicalOr <$ atom Token.LogicalOr

-------------------------------------------------------------------------------

identifier :: Parser Token String
identifier = (\(Identifier name) -> name) <$> satisfy
  (\case
    Identifier name -> True
    _               -> False
  )

-------------------------------------------------------------------------------

commaDelimitedList :: Parser Token a -> Parser Token [a]
commaDelimitedList = delimitedList $ void (atom Comma)

delimitedList :: Parser Token () -> Parser Token a -> Parser Token [a]
delimitedList sep elem =
  (++) <$> (maybeToList <$> optional elem) <*> many (sep *> elem)

parenthesized :: Parser Token a -> Parser Token a
parenthesized = surrounded (atom OpenParen) (atom CloseParen)

braced :: Parser Token a -> Parser Token a
braced = surrounded (atom OpenBrace) (atom CloseBrace)

tuple2 :: a -> b -> (a, b)
tuple2 x y = (x, y)

tuple3 :: a -> b -> c -> (a, b, c)
tuple3 x y z = (x, y, z)
