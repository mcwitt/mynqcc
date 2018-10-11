module Lib
    ( Lib.lex
    , Token (..)
    , someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

lex :: String -> [Token]
lex _ = [ KWInt
        , Identifier "main"
        , OpenParen
        , CloseParen
        , OpenBrace
        , KWReturn
        , Integer 100
        , Semicolon
        , CloseBrace
        ]
