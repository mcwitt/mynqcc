{-# LANGUAGE RecordWildCards #-}

module ParserSpec (spec) where

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)

import Lexer (Token (..), lex)
import Parser ( parse_
              , Program (Program)
              , Function (Function)
              , Statement (Return)
              , Expression (Constant)
              )

spec :: Spec
spec = do describe "parser: valid" $ for_ validCases test
  where
    test Case {..} = it description $ parse_ input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: [Token]
                 , expected    :: Program
                 }

validCases :: [Case]
validCases = [
  Case { description = "missing_paren.c"
       , input    = [ KWInt
                    , Identifier "main"
                    , OpenParen
                    , CloseParen
                    , OpenBrace
                    , KWReturn
                    , Integer 100
                    , Semicolon
                    , CloseBrace]
       , expected = Program (Function "main" (Return (Constant 100)))
       }
  ]
