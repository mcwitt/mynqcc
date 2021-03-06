{-# LANGUAGE LambdaCase #-}

{-|
  This module implements a parser-combinator strategy for lexing input strings
  into lists of tokens, and for parsing lists of tokens into an AST. If this
  weren't an educational project, we should instead use an existing library like
  [Parsec](http://hackage.haskell.org/package/parsec).

  The implementation here basically follows the one found in Chapter 13 of
  Graham Hutton's "Programming in Haskell", except we have generalized the
  parser type to handle input streams of arbitrary type (not just Char) so
  the same infrastructure can be used in separate lexing and parsing steps.
-}

module ParserCombinators
  ( Parser(..)
  , item
  , peek
  , empty
  , optional
  , some
  , many
  , satisfy
  , atom
  , string
  , chainl1
  , surrounded
  , (<|>)
  )
where

import           Control.Applicative

-- | A parser is a function from a list of atoms (e.g. character strings) to
-- | zero or more possible (parsed, rest) pairs.
newtype Parser a b = P { parse :: [a] -> [(b, [a])] }

-- | Simplest parser. Just read a single atom from the input stream.
item :: Parser a a
item = P $ \case
  []     -> []
  x : xs -> [(x, xs)]

-- | Peek ahead one atom without removing it from the input stream.
peek :: Parser a a
peek = P $ \case
  []     -> []
  x : xs -> [(x, x : xs)]

instance Functor (Parser a) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f p = P $ \st ->
    case parse p st of [] -> []
                       [(x, xs)] -> [(f x, xs)]

instance Applicative (Parser a) where
  -- pure :: a -> Parser a
  pure x = P $ \st -> [(x, st)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \st ->
    case parse pf st of [] -> []
                        [(f, xs)] -> parse (fmap f px) xs

instance Monad (Parser a) where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \st ->
    case parse p st of [] -> []
                       [(x, xs)] -> parse (f x) xs


-- | Allows chaining parsers such that if the first fails, we try the next, etc.
instance Alternative (Parser a) where
  -- empty :: Parser a
  empty = P $ const []

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \st ->
    case parse p st of []  -> parse q st
                       res -> res

-- | Parser that consumes an atom x for which p x is true.
satisfy :: (a -> Bool) -> Parser a a
satisfy p = do
  x <- item
  if p x then return x else empty

-- | Parser that consumes one of the specified atom.
atom :: (Eq a) => a -> Parser a a
atom x = satisfy (== x)

-- | Parser that consumes the specified string of atoms.
string :: (Eq a) => [a] -> Parser a ()
string []       = return ()
string (x : xs) = do
  atom x
  string xs

-- | Parser that consumes a string of binary operations, combining
-- | left-associatively
chainl1 :: Parser a b -> Parser a (b -> b -> b) -> Parser a b
p `chainl1` op = do
  a <- p
  rest a
 where
  rest a =
    (do
        f <- op
        b <- p
        rest (f a b)
      )
      <|> return a

surrounded :: Parser a a -> Parser a a -> Parser a b -> Parser a b
surrounded left right parser = left *> parser <* right
