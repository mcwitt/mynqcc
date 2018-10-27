module ParserSpec (spec) where

import AST
import Error
import Parser
import Test
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe)
import Token

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
            Function "main" [
                returnStatement (
                    Constant 100)]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.LogicalNegation)(
                        Constant 12))]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.BitwiseComplement)(
                        Constant 0))]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.Negation)(
                        Constant 5))]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.LogicalNegation)(
                        Unary (
                            AST.Negation)(
                            Constant 3)))]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.Negation)(
                        Unary (
                            AST.BitwiseComplement)(
                            Constant 0)))]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.LogicalNegation)(
                        Constant 5))]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.LogicalNegation)(
                        Constant 0))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Addition)(
                        Constant 1)(
                        Constant 2))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Subtraction)(
                        Binary (
                            AST.Subtraction)(
                            Constant 1)(
                            Constant 2))(
                        Constant 3))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Division)(
                        Binary (
                            AST.Division)(
                            Constant 6)(
                            Constant 3))(
                        Constant 2))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Division)(
                        Constant 4)(
                        Constant 2))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Multiplication)(
                        Constant 2)(
                        Constant 3))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Multiplication)(
                        Constant 2)(
                        Binary (
                            AST.Addition)(
                            Constant 3)(
                            Constant 4)))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Addition)(
                        Constant 2)(
                        Binary (
                            AST.Multiplication)(
                            Constant 3)(
                            Constant 4)))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Subtraction)(
                        Constant 2)(
                        Unary (
                            AST.Negation)(
                            Constant 1)))]))

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
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Addition)(
                        Unary (
                            AST.BitwiseComplement)(
                            Constant 2))(
                        Constant 3))]))

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
            Function "main" [
                returnStatement (
                    Unary (
                        AST.BitwiseComplement)(
                        Binary (
                            AST.Addition)(
                            Constant 1)(
                            Constant 1)))]))

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

  describe "Stage 4" $ do

    it "should parse tokens from and_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.LogicalAnd
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LogicalAnd)(
                        Constant 1)(
                        Constant 0))]))

    it "should parse tokens from and_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.LogicalAnd
                  , Token.Negation
                  , Integer 1
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LogicalAnd)(
                        Constant 1)(
                        Unary (
                            AST.Negation)(
                            Constant 1)))]))

    it "should parse tokens from eq_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.Equality
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Equality)(
                        Constant 1)(
                        Constant 2))]))

    it "should parse tokens from eq_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.Equality
                  , Integer 1
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Equality)(
                        Constant 1)(
                        Constant 1))]))

    it "should parse tokens from ge_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.GreaterEqual
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.GreaterEqual)(
                        Constant 1)(
                        Constant 2))]))

    it "should parse tokens from ge_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.GreaterEqual
                  , Integer 1
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.GreaterEqual)(
                        Constant 1)(
                        Constant 1))]))

    it "should parse tokens from gt_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.GreaterThan
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.GreaterThan)(
                        Constant 1)(
                        Constant 2))]))

    it "should parse tokens from gt_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.GreaterThan
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.GreaterThan)(
                        Constant 1)(
                        Constant 0))]))

    it "should parse tokens from le_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.LessEqual
                  , Token.Negation
                  , Integer 1
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LessEqual)(
                        Constant 1)(
                        Unary (
                            AST.Negation)(
                            Constant 1)))]))

    it "should parse tokens from le_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 0
                  , Token.LessEqual
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LessEqual)(
                        Constant 0)(
                        Constant 2))]))

    it "should parse tokens from lt_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.LessThan
                  , Integer 1
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LessThan)(
                        Constant 2)(
                        Constant 1))]))

    it "should parse tokens from lt_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.LessThan
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LessThan)(
                        Constant 1)(
                        Constant 2))]))

    it "should parse tokens from ne_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 0
                  , Token.Inequality
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Inequality)(
                        Constant 0)(
                        Constant 0))]))

    it "should parse tokens from ne_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.Negation
                  , Integer 1
                  , Token.Inequality
                  , Token.Negation
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Inequality)(
                        Unary (
                            AST.Negation)(
                            Constant 1))(
                        Unary (
                            AST.Negation)(
                            Constant 2)))]))

    it "should parse tokens from or_false.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 0
                  , Token.LogicalOr
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LogicalOr)(
                        Constant 0)(
                        Constant 0))]))

    it "should parse tokens from or_true.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.LogicalOr
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LogicalOr)(
                        Constant 1)(
                        Constant 0))]))

    it "should parse tokens from precedence.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.LogicalOr
                  , Integer 0
                  , Token.LogicalAnd
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LogicalOr)(
                        Constant 1)(
                        Binary (
                            AST.LogicalAnd)(
                            Constant 0)(
                            Constant 2)))]))


    it "should parse tokens from precedence_2.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , OpenParen
                  , Integer 1
                  , Token.LogicalOr
                  , Integer 0
                  , CloseParen
                  , Token.LogicalAnd
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LogicalAnd)(
                        Binary (
                            AST.LogicalOr)(
                            Constant 1)(
                            Constant 0))(
                        Constant 0))]))

    it "should parse tokens from precedence_3.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.Equality
                  , Integer 2
                  , Token.GreaterThan
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.Equality)(
                        Constant 2)(
                        Binary (
                            AST.GreaterThan)(
                            Constant 2)(
                            Constant 0)))]))

    it "should parse tokens from precedence_4.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.Equality
                  , Integer 2
                  , Token.LogicalOr
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (
                    Binary (
                        AST.LogicalOr)(
                        Binary (
                            AST.Equality)(
                            Constant 2)(
                            Constant 2))(
                        Constant 0))]))

    it "should fail to parse tokens from missing_first_op.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Token.LessEqual
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_mid_op.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Token.LessThan
                  , Token.GreaterThan
                  , Integer 3
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_second_op.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 2
                  , Token.LogicalAnd
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

  describe "Stage 5" $ do

    it "should parse tokens from assign.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Semicolon
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , expressionStatement (AST.Assignment "a" (Constant 2))
                , returnStatement (Reference "a")]))

    it "should parse tokens from assign_val.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Semicolon
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , Declaration "b" (
                    Just (AST.Assignment "a" (Constant 0)))
                , returnStatement (Reference "b")]))

    it "should parse tokens from exp_return_val.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Semicolon
                  , KWInt
                  , Identifier "b"
                  , Semicolon
                  , Identifier "a"
                  , Token.Assignment
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 4
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Token.Negation
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , Declaration "b" Nothing
                , expressionStatement (
                    AST.Assignment "a" (
                        AST.Assignment "b" (Constant 4)))
                , returnStatement (
                    Binary (
                        AST.Subtraction)(
                        Reference "a")(
                        Reference "b"))]))

    it "should parse tokens from initialize.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 2))
                , returnStatement (Constant 0)]))

    it "should parse tokens from missing_return.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , CloseBrace]
        `shouldBe`
        Right (Program (Function "main" []))

    it "should parse tokens from multiple_vars.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Token.Addition
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 1))
                , Declaration "b" (Just (Constant 2))
                , returnStatement (
                    Binary (
                        AST.Addition)(
                        Reference "a")(
                        Reference "b"))]))

    it "should parse tokens from no_initialize.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Semicolon
                  , KWReturn
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , returnStatement (Constant 0)]))

    it "should parse tokens from refer.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 2))
                , returnStatement (Reference "a")]))

    it "should parse tokens from unused_exp.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , Integer 2
                  , Token.Addition
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Integer 0
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  expressionStatement (
                      Binary (
                          AST.Addition)(
                          Constant 2)(
                          Constant 2))
                , returnStatement (Constant 0)]))

    it "should parse tokens from redefine.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 1))
                , Declaration "a" (Just (Constant 2))
                , returnStatement (Reference "a")]))

    it "should fail to parse tokens from syntax_err_bad_decl.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , Identifier "ints"
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_bad_decl_2.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "foo"
                  , Identifier "bar"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWReturn
                  , Identifier "bar"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_bad_lvalue.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , Identifier "a"
                  , Token.Addition
                  , Integer 3
                  , Token.Assignment
                  , Integer 4
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_bad_lvalue_2.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , Token.LogicalNegation
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_no_semicolon.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Identifier "a"
                  , Token.Assignment
                  , Identifier "a"
                  , Token.Addition
                  , Integer 4
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should parse tokens from undeclared_var.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                returnStatement (Reference "a")]))

    it "should parse tokens from var_declared_late.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Token.Addition
                  , Integer 2
                  , Semicolon
                  , KWInt
                  , Identifier "a"
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  expressionStatement (
                      AST.Assignment "a" (
                          Binary (
                              AST.Addition)(
                              Constant 1)(
                              Constant 2)))
                , Declaration "a" Nothing
                , returnStatement (Reference "a")]))

  describe "Stage 6" $ do

    it "should parse tokens from assign_ternary.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , QuestionMark
                  , Integer 2
                  , Colon
                  , Integer 3
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 0))
                , expressionStatement (
                    AST.Assignment "a" (
                        Conditional (
                            Constant 1)(
                            Constant 2)(
                            Constant 3)))
                , returnStatement (Reference "a")]))
