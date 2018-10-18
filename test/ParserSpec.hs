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
                    Expression (
                        Term (
                            Constant 100))))))

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
                    Expression (
                        Term (
                            AST.LogicalNegation (
                                Constant 12)))))))

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
                    Expression (
                        Term (
                            AST.BitwiseComplement (
                                Constant 0)))))))

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
                    Expression (
                        Term (
                            AST.Negation (
                                Constant 5)))))))

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
                    Expression (
                        Term (
                            AST.LogicalNegation(
                                AST.Negation (
                                    Constant 3))))))))

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
                    Expression (
                        Term (
                            AST.Negation(
                                AST.BitwiseComplement (
                                    Constant 0))))))))

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
                    Expression (
                        Term (
                            AST.LogicalNegation (
                                Constant 5)))))))

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
                    Expression (
                        Term (
                            AST.LogicalNegation (
                                Constant 0)))))))

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

  describe "Stage 3" $ do

    it "should parse tokens from add.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.Addition
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.Addition (
                        Expression (Term (Constant 1)))(
                        Expression (Term (Constant 2)))))))

    it "should parse tokens from associativity.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.Negation
                  , Integer 2
                  , Token.Negation
                  , Integer 3
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.Subtraction (
                        AST.Subtraction (
                            Expression (Term (Constant 1)))(
                            Expression (Term (Constant 2))))(
                    Expression (Term (Constant 3)))))))

    it "should parse tokens from associativity_2.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 6
                  , Token.Division
                  , Integer 3
                  , Token.Division
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    Expression (
                        AST.Division (
                            AST.Division (
                                Term (Constant 6))(
                                Term (Constant 3)))(
                            Term (Constant 2)))))))

    it "should parse tokens from div.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 4
                  , Token.Division
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    Expression (
                        AST.Division (
                            Term (Constant 4))(
                            Term (Constant 2)))))))

    it "should parse tokens from mult.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.Multiplication
                  , Integer 3
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    Expression (
                        AST.Multiplication (
                            Term (Constant 2))(
                            Term (Constant 3)))))))

    it "should parse tokens from parens.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.Multiplication
                  , OpenParen
                  , Integer 3
                  , Token.Addition
                  , Integer 4
                  , CloseParen
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    Expression (
                        AST.Multiplication (
                            Term (Constant 2))(
                            Term (
                                Factor (
                                    AST.Addition (
                                        Expression (Term (Constant 3)))(
                                        Expression (Term (Constant 4)))))))))))

    it "should parse tokens from precedence.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.Addition
                  , Integer 3
                  , Token.Multiplication
                  , Integer 4
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.Addition (
                        Expression (Term (Constant 2)))(
                        Expression (
                            AST.Multiplication (
                                Term (Constant 3))(
                                Term (Constant 4))))))))

    it "should parse tokens from sub_neg.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.Negation
                  , Token.Negation
                  , Integer 1
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.Subtraction (
                        Expression (Term (Constant 2)))(
                        Expression (Term (AST.Negation (Constant 1))))))))

    it "should parse tokens from unop_add.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.BitwiseComplement
                  , Integer 2
                  , Token.Addition
                  , Integer 3
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    AST.Addition (
                        Expression (Term (AST.BitwiseComplement (Constant 2))))(
                        Expression (Term (Constant 3)))))))

    it "should parse tokens from unop_parens.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.BitwiseComplement
                  , OpenParen
                  , Integer 1
                  , Token.Addition
                  , Integer 1
                  , CloseParen
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" (
                Return (
                    Expression (
                        Term (
                            AST.BitwiseComplement (
                                Factor (
                                    AST.Addition (
                                        Expression (Term (Constant 1)))(
                                        Expression (Term (Constant 1)))))))))))

    it "should fail to parse tokens from malformed_parens.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , OpenParen
                  , Token.Negation
                  , Integer 3
                  , CloseParen
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_first.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.Division
                  , Integer 3
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from no_semicolon.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.Multiplication
                  , Integer 2
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")
