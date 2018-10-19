module Lexer ( lexString
             ) where

import Data.Char
import ParserCombinators
import Token
import Error

lexString :: String -> Either Error [Token]
lexString st = case parse lexer st of
                 [(res, xs)] -> Right res
                 [] -> Left $ LexerError "Failed to lex the program."

lexer :: Parser Char [Token]
lexer = many . token $ reservedWord "int" KWInt
                   <|> reservedWord "return" KWReturn

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

                   <|> identifier
                   <|> integer

token :: Parser Char a -> Parser Char a
token p = do space
             val <- p
             return val

charToken :: Char -> Token -> Parser Char Token
charToken ch tok = do atom ch; return tok

stringToken :: String -> Token -> Parser Char Token
stringToken st tok = do string st; return tok

reservedWord :: String -> Token -> Parser Char Token
reservedWord st tok = do res <- stringToken st tok
                         c <- peek
                         if isAlphaNum c
                           then empty
                           else return res

identifier :: Parser Char Token
identifier = do s <- (do c <- letter
                         cs <- many (letter <|> number)
                         return (c:cs))
                return $ Identifier s

integer :: Parser Char Token
integer = do s <- some number
             return $ Integer (read s)

number :: Parser Char Char
number = satisfy isNumber

letter :: Parser Char Char
letter = satisfy isLetter

space :: Parser Char ()
space = do many $ satisfy isSpace
           return ()

