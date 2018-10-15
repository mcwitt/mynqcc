module Lexer
    ( Lexer.lex
    , Token (..)
    ) where

import Data.Char

import ParserCombinators ( Parser (..)
                         , peek
                         , empty
                         , many
                         , some
                         , space
                         , char
                         , number
                         , string
                         , ident
                         , token
                         , (<|>)
                         )


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

keyword :: Parser a -> Parser a
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

lexer :: Parser [Token]
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
