{-# LANGUAGE LambdaCase #-}

module Parser
  ( parseTokens
  )
where

import qualified          AST
import           Control.Monad
import           Data.Functor
import           Data.Maybe
import           Error
import           ParserCombinators
import           Token

parseTokens :: [Token] -> Either Error AST.Program
parseTokens ts = case parse program ts of
  [(res, [])] -> Right res
  _           -> Left $ ParserError "Failed to parse the program."

program :: Parser Token AST.Program
program = AST.Program <$> many function

function :: Parser Token AST.Function
function =
  AST.Function
    <$  atom KWInt
    <*> identifier
    <*> parenthesized (commaDelimitedList (atom KWInt *> identifier))
    <*> ((Just <$> braced (many blockItem)) <|> (Nothing <$ atom Semicolon))

-- Block items

blockItem :: Parser Token AST.BlockItem
blockItem = blockDeclaration <|> blockStatement

blockDeclaration :: Parser Token AST.BlockItem
blockDeclaration = AST.Declaration <$> declaration

blockStatement :: Parser Token AST.BlockItem
blockStatement = AST.Statement <$> statement

-- Declarations
-------------------------------------------------------------------------------

declaration :: Parser Token AST.Declaration
declaration =
  AST.Decl
    <$  atom KWInt
    <*> identifier
    <*> optional (atom Token.Assignment *> expression)
    <*  atom Semicolon

-- Statements
-------------------------------------------------------------------------------

statement :: Parser Token AST.Statement
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

ifStatement :: Parser Token AST.Statement
ifStatement =
  AST.if_ <$ atom KWIf <*> parenthesized expression <*> statement <*> optional
    (atom KWElse *> statement)

returnStatement :: Parser Token AST.Statement
returnStatement = AST.return_ <$> (atom KWReturn *> expression <* atom Semicolon)

standaloneExpr :: Parser Token AST.Statement
standaloneExpr = AST.expression <$> (optional expression <* atom Semicolon)

compoundStatement :: Parser Token AST.Statement
compoundStatement = AST.compound <$> braced (many blockItem)

forStatement :: Parser Token AST.Statement
forStatement =
  (\(init, cond, post) body -> AST.for init cond post body)
    <$  atom KWFor
    <*> parenthesized
          (   tuple3
          <$> optional expression
          <*  atom Semicolon
          <*> (expression <|> pure (AST.constant 1))
          <*  atom Semicolon
          <*> optional expression
          )
    <*> statement

forDeclStatement :: Parser Token AST.Statement
forDeclStatement =
  (\(init, cond, post) body -> AST.forDecl init cond post body)
    <$  atom KWFor
    <*> parenthesized
          (   tuple3
          <$> declaration
          <*> (expression <|> pure (AST.constant 1))
          <*  atom Semicolon
          <*> optional expression
          )
    <*> statement

whileStatement :: Parser Token AST.Statement
whileStatement =
  AST.while <$ atom KWWhile <*> parenthesized expression <*> statement

doStatement :: Parser Token AST.Statement
doStatement =
  AST.do_
    <$  atom KWDo
    <*> statement
    <*  atom KWWhile
    <*> parenthesized expression
    <*  atom Semicolon

breakStatement :: Parser Token AST.Statement
breakStatement = AST.break <$ atom KWBreak <* atom Semicolon

continueStatement :: Parser Token AST.Statement
continueStatement = AST.continue <$ atom KWContinue <* atom Semicolon

-- Expressions
-------------------------------------------------------------------------------

expression :: Parser Token AST.Expression
expression = assignment <|> conditionalExpr

assignment =
  AST.assignment <$> identifier <* atom Token.Assignment <*> expression

conditionalExpr =
  (\e1 e23 -> case e23 of
      Just (e2, e3) -> AST.conditional e1 e2 e3
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

constant = (\(Integer i) -> AST.constant i) <$> satisfy
  (\case
    Integer _ -> True
    _         -> False
  )

reference = AST.reference <$> identifier

funCall =
  AST.funCall <$> identifier <*> parenthesized (commaDelimitedList expression)

negation = AST.unary AST.Negation <$ atom Token.Negation <*> factor

bitwiseComplement =
  AST.unary AST.BitwiseComplement <$ atom Token.BitwiseComplement <*> factor

logicalNegation =
  AST.unary AST.LogicalNegation <$ atom Token.LogicalNegation <*> factor

-- Binary operators
-------------------------------------------------------------------------------

multiplication :: Parser Token (AST.Expression -> AST.Expression -> AST.Expression)
multiplication = AST.binary AST.Multiplication <$ atom Token.Multiplication

division = AST.binary AST.Division <$ atom Token.Division

modulo = AST.binary AST.Modulo <$ atom PercentSign

addition = AST.binary AST.Addition <$ atom Token.Addition

subtraction = AST.binary AST.Subtraction <$ atom Token.Negation

lessThan = AST.binary AST.LessThan <$ atom Token.LessThan

greaterThan = AST.binary AST.GreaterThan <$ atom Token.GreaterThan

lessEqual = AST.binary AST.LessEqual <$ atom Token.LessEqual

greaterEqual = AST.binary AST.GreaterEqual <$ atom Token.GreaterEqual

equality = AST.binary AST.Equality <$ atom Token.Equality

inequality = AST.binary AST.Inequality <$ atom Token.Inequality

logicalAnd = AST.binary AST.LogicalAnd <$ atom Token.LogicalAnd

logicalOr = AST.binary AST.LogicalOr <$ atom Token.LogicalOr

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
