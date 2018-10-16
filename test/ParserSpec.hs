module ParserSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec        ( Spec
                         , describe
                         , it
                         , shouldBe
                         , shouldThrow
                         , errorCall
                         )

import Token  (Token (..))
import Parser (parse_)

import AST ( Program (Program)
           , Function (Function)
           , Statement (Return)
           , Expression (Constant)
           )

spec :: Spec
spec = do
  it "should parse tokens from multi_digit.c" $ do
    parse_ [ KWInt
           , Identifier "main"
           , OpenParen
           , CloseParen
           , OpenBrace
           , KWReturn
           , Integer 100
           , Semicolon
           , CloseBrace] `shouldBe`
      Program (Function "main" (Return (Constant 100)))

  it "should fail to parse tokens from missing_paren.c" $ do
    evaluate (parse_ [ KWInt
                     , Identifier "main"
                     , OpenParen
                     , OpenBrace
                     , KWReturn
                     , Integer 0
                     , Semicolon
                     , CloseBrace]) `shouldThrow`
      errorCall "Failed to parse the program."
