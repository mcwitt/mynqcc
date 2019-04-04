module Lexer
  ( lexString
  )
where

import           Control.Monad                  ( void )
import           Data.Char
import           Error
import           ParserCombinators
import           Token

lexString :: String -> Either Error [Token]
lexString st = case parse lexer st of
  [(res, [])] -> Right res
  _           -> Left $ LexerError "Failed to lex the program."

lexer :: Parser Char [Token]
lexer = many (space *> token) <* optional space

token :: Parser Char Token
token =
  reservedWord "int" KWInt
    <|> reservedWord "return"   KWReturn
    <|> reservedWord "if"       KWIf
    <|> reservedWord "else"     KWElse
    <|> reservedWord "for"      KWFor
    <|> reservedWord "while"    KWWhile
    <|> reservedWord "do"       KWDo
    <|> reservedWord "break"    KWBreak
    <|> reservedWord "continue" KWContinue

    <|> stringToken "&&" LogicalAnd
    <|> stringToken "||" LogicalOr
    <|> stringToken "==" Equality
    <|> stringToken "!=" Inequality
    <|> stringToken "<=" LessEqual
    <|> stringToken ">=" GreaterEqual

    <|> charToken '{' OpenBrace
    <|> charToken '}' CloseBrace
    <|> charToken '(' OpenParen
    <|> charToken ')' CloseParen
    <|> charToken ';' Semicolon
    <|> charToken '-' Negation
    <|> charToken '~' BitwiseComplement
    <|> charToken '!' LogicalNegation
    <|> charToken '+' Addition
    <|> charToken '*' Multiplication
    <|> charToken '/' Division
    <|> charToken '<' LessThan
    <|> charToken '>' GreaterThan
    <|> charToken '=' Assignment
    <|> charToken ':' Colon
    <|> charToken '?' QuestionMark
    <|> charToken '%' PercentSign
    <|> charToken ',' Comma

    <|> identifier
    <|> integer

charToken :: Char -> Token -> Parser Char Token
charToken ch tok = tok <$ atom ch

stringToken :: String -> Token -> Parser Char Token
stringToken str tok = tok <$ string str

reservedWord :: String -> Token -> Parser Char Token
reservedWord st tok = do
  res <- stringToken st tok
  c   <- peek
  if isAlphaNum c then empty else return res

identifier :: Parser Char Token
identifier = (fmap . fmap) Identifier (:) <$> (letter <|> underscore) <*> many
  (letter <|> number <|> underscore)

integer :: Parser Char Token
integer = Integer . read <$> some number

number :: Parser Char Char
number = satisfy isNumber

underscore :: Parser Char Char
underscore = atom '_'

letter :: Parser Char Char
letter = satisfy isLetter

space :: Parser Char ()
space = void $ many $ satisfy isSpace
