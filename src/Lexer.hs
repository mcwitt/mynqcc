module Lexer
  ( Lexer.lex
  , Token (..)
  ) where

import Data.Char
import ParserCombinators ( Parser (..)
                         , peek
                         , empty
                         , some
                         , many
                         , satisfy
                         , (<|>)
                         )

char :: Char -> Parser Char Char
char c = satisfy (==c)

number :: Parser Char Char
number = satisfy isNumber

letter :: Parser Char Char
letter = satisfy isLetter

space :: Parser Char ()
space = many (satisfy isSpace) >> return ()

-- | Parser that consumes the specified string.
string :: String -> Parser Char ()
string [] = return ()
string (x:xs) = do c <- char x
                   cs <- string xs
                   return ()

-- | Parser that consumes an identifier, defined as a string that matches the
-- | regex "[a-zA-Z][a-zA-Z0-9]*".
ident :: Parser Char String
ident = do c <- letter
           cs <- many (letter <|> number)
           return (c:cs)

-- | Transforms a parser to discard leading space.
token :: Parser Char a -> Parser Char a
token p = do space
             val <- p
             return val

data Token = OpenBrace
           | CloseBrace
           | OpenParen
           | CloseParen
           | Semicolon
           | KWInt
           | KWReturn
           | Identifier String
           | Integer Int
           deriving (Show, Eq)

openBrace  = token (char '{') >> return OpenBrace
closeBrace = token (char '}') >> return CloseBrace
openParen  = token (char '(') >> return OpenParen
closeParen = token (char ')') >> return CloseParen
semicolon  = token (char ';') >> return Semicolon

keyword :: Parser Char a -> Parser Char a
keyword p = do val <- p
               c <- peek
               if isAlphaNum c
                 then empty
                 else return val

kwInt    = (token . keyword) (string "int")    >> return KWInt
kwReturn = (token . keyword) (string "return") >> return KWReturn

identifier = do s <- token ident
                return (Identifier s)

integer = do s <- token (some number)
             return $ Integer (read s)

lexer :: Parser Char [Token]
lexer = many $  openBrace
            <|> closeBrace
            <|> openParen
            <|> closeParen
            <|> semicolon
            <|> kwInt
            <|> kwReturn
            <|> identifier
            <|> integer

lex :: String -> [Token]
lex st = res
  where [(res, xs)] = parse lexer st
