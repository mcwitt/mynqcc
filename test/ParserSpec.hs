module ParserSpec (spec) where

import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe)

import AST
import Token
import Parser
import Error


spec :: Spec
spec = do
  describe "Stage 1" $ do

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
        Right (
        Program (
            Function "main" (
                Return (
                    Constant 100))))

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

  describe "Stage 2" $ do

    it "should parse tokens from bitwise.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LogicalNegation
                  , Integer 12
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.LogicalNegation (Constant 12)))))

    it "should parse tokens from bitwise_zero.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.BitwiseComplement
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.BitwiseComplement (Constant 0)))))

    it "should parse tokens from neg.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.Negation
                  , Integer 5
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.Negation (Constant 5)))))

    it "should parse tokens from nested_ops.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LogicalNegation
                  , Token.Negation
                  , Integer 3
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.LogicalNegation(
                        AST.Negation (Constant 3))))))

    it "should parse tokens from nested_ops_2.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.Negation
                  , Token.BitwiseComplement
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.Negation(
                        AST.BitwiseComplement (Constant 0))))))

    it "should parse tokens from not_5.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LogicalNegation
                  , Integer 5
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.LogicalNegation (Constant 5)))))

    it "should parse tokens from not_0.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LogicalNegation
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.LogicalNegation (Constant 0)))))

    it "should fail to parse tokens from missing_const.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LogicalNegation
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_semicolon.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LogicalNegation
                  , Integer 5
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from nested_missing_const.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LogicalNegation
                  , Token.BitwiseComplement
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from wrong_order.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 4
                  , Token.Negation
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")
