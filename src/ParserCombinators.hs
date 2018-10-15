module ParserCombinators
  ( Parser (..)
  , peek
  , empty
  , some
  , many
  , space
  , char
  , number
  , letter
  , string
  , ident
  , token
  , (<|>)
  ) where

import Data.Char
import Control.Applicative

newtype Parser a = P { parse :: String -> [(a, String)]}

-- | Simplest parser. Just read a character from the input stream.
item :: Parser Char
item = P $ \st ->
  case st of [] -> []
             x:xs -> [(x, xs)]

-- | Peek ahead one character without removing it from the input stream.
peek :: Parser Char
peek = P $ \st -> case st of [] -> []
                             x:xs -> [(x, x:xs)]

instance Functor Parser where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f p = P $ \st ->
    case parse p st of [] -> []
                       [(x, xs)] -> [(f x, xs)]

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P $ \st -> [(x, st)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \st ->
    case parse pf st of [] -> []
                        [(f, xs)] -> parse (fmap f px) xs


-- | Example using applicative style.
-- | Consume 3 characters, return the first and third.
three :: Parser (Char, Char)
three = (pure g) <*> item <*> item <*> item
  where g = \c1 c2 c3 -> (c1, c3)


instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \st ->
    case parse p st of [] -> []
                       [(x, xs)] -> parse (f x) xs


-- | Example using monadic style.
mthree :: Parser (Char, Char)
mthree = item >>= \ x ->
                    item >>
                    item >>= \ z ->
                               return (x, z)


-- | Same as above, using `do` notation.
mthree' :: Parser (Char, Char)
mthree' = do x <- item
             item
             z <- item
             return (x, z)

-- | Allows chaining parsers such that if the first fails, we try the next, etc.
instance Alternative Parser where
  -- empty :: Parser a
  empty = P $ \st -> []

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \st ->
    case parse p st of []  -> parse q st
                       res -> res


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- item
               if p c then return c else empty

char :: Char -> Parser Char
char c = satisfy (==c)

number :: Parser Char
number = satisfy isNumber

letter :: Parser Char
letter = satisfy isLetter

space :: Parser String
space = many (satisfy isSpace)

string :: String -> Parser String
string [] = return []
string (x:xs) = do c <- char x
                   cs <- string xs
                   return (c:cs)

ident :: Parser String
ident = do c <- letter
           cs <- many (letter <|> number)
           return (c:cs)

token :: Parser a -> Parser a
token p = do space
             val <- p
             return val
