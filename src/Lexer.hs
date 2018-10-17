module Lexer ( lexString
             ) where

import Data.Char
import ParserCombinators ( Parser (..)
                         , peek
                         , empty
                         , some
                         , many
                         , satisfy
                         , atom
                         , string
                         , (<|>)
                         )

import Token ( Token (..) )
import Error ( Error (LexerError) )

number :: Parser Char Char
number = satisfy isNumber

letter :: Parser Char Char
letter = satisfy isLetter

space :: Parser Char ()
space = do many (satisfy isSpace)
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

openBrace  = token (atom '{') >> return OpenBrace
closeBrace = token (atom '}') >> return CloseBrace
openParen  = token (atom '(') >> return OpenParen
closeParen = token (atom ')') >> return CloseParen
semicolon  = token (atom ';') >> return Semicolon

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

lexString :: String -> Either Error [Token]
lexString st = case parse lexer st of
                 [(res, xs)] -> Right res
                 [] -> Left $ LexerError "Failed to lex the program."
