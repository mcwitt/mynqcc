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
  otherwise   -> Left $ LexerError "Failed to lex the program."

lexer :: Parser Char [Token]
lexer =
  many
    $  space
    >> (   reservedWord "int"      KWInt
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
       )

charToken :: Char -> Token -> Parser Char Token
charToken ch tok = atom ch >> return tok

stringToken :: String -> Token -> Parser Char Token
stringToken st tok = string st >> return tok

reservedWord :: String -> Token -> Parser Char Token
reservedWord st tok = do
  res <- stringToken st tok
  c   <- peek
  if isAlphaNum c then empty else return res

identifier :: Parser Char Token
identifier = do
  s <- do
    c  <- letter
    cs <- many (letter <|> number)
    return (c : cs)
  return $ Identifier s

integer :: Parser Char Token
integer = pure (Integer . read) <*> some number

number :: Parser Char Char
number = satisfy isNumber

letter :: Parser Char Char
letter = satisfy isLetter

space :: Parser Char ()
space = void $ many (satisfy isSpace)

