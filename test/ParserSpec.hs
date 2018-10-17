module ParserSpec (spec) where

import Test.Hspec ( Spec
                  , it
                  , shouldBe)

import Token  ( Token (..))
import Parser ( parseTokens)
import Error  ( Error (ParserError))

import AST ( Program (Program)
           , Function (Function)
           , Statement (Return)
           , Expression (Constant)
           )

spec :: Spec
spec = do
  it "should parse tokens from multi_digit.c" $ do
    parseTokens [ KWInt
                , Identifier "main"
                , OpenParen
                , CloseParen
                , OpenBrace
                , KWReturn
                , Integer 100
                , Semicolon
                , CloseBrace]
      `shouldBe`
      Right (Program (Function "main" (Return (Constant 100))))

  it "should fail to parse tokens from missing_paren.c" $ do
    parseTokens [ KWInt
                , Identifier "main"
                , OpenParen
                , OpenBrace
                , KWReturn
                , Integer 0
                , Semicolon
                , CloseBrace]
      `shouldBe`
      Left (ParserError "Failed to parse the program.")
