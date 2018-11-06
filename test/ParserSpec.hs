module ParserSpec (spec) where

import           AST
import           Error
import           Parser
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Token

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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                Statement . Return $ (
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
                , Statement . Expression $ (AST.Assignment "a" (Constant 2))
                , Statement . Return $ (Reference "a")]))

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
                , Statement . Return $ (Reference "b")]))

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
                , Statement . Expression $ (
                    AST.Assignment "a" (
                        AST.Assignment "b" (Constant 4)))
                , Statement . Return $ (
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
                , Statement . Return $ (Constant 0)]))

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
                , Statement . Return $ (
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
                , Statement . Return $ (Constant 0)]))

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
                , Statement . Return $ (Reference "a")]))

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
                  Statement . Expression $ (
                      Binary (
                          AST.Addition)(
                          Constant 2)(
                          Constant 2))
                , Statement . Return $ (Constant 0)]))

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
                , Statement . Return $ (Reference "a")]))

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
                Statement . Return $ (Reference "a")]))

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
                  Statement . Expression $ (
                      AST.Assignment "a" (
                          Binary (
                              AST.Addition)(
                              Constant 1)(
                              Constant 2)))
                , Declaration "a" Nothing
                , Statement . Return $ (Reference "a")]))

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
                , Statement . Expression $ (
                    AST.Assignment "a" (
                        Conditional (
                            Constant 1)(
                            Constant 2)(
                            Constant 3)))
                , Statement . Return $ (Reference "a")]))

    it "should parse tokens from multiple_ternary.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Token.GreaterThan
                  , Integer 2
                  , QuestionMark
                  , Integer 3
                  , Colon
                  , Integer 4
                  , Semicolon
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Token.GreaterThan
                  , Integer 2
                  , QuestionMark
                  , Integer 5
                  , Colon
                  , Integer 6
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
                Declaration "a" (
                    Just (
                        Conditional (
                            Binary AST.GreaterThan (Constant 1) (Constant 2))(
                              Constant 3)(
                              Constant 4)))
              , Declaration "b" (
                    Just (
                        Conditional (
                            Binary AST.GreaterThan (Constant 1) (Constant 2))(
                            Constant 5)(
                            Constant 6)))
              , Statement . Return $ (
                    Binary AST.Addition
                    (Reference "a")
                    (Reference "b"))]))

    it "should parse tokens from nested_ternary.c" $ do
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
                  , KWInt
                  , Identifier "flag"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Token.GreaterThan
                  , Identifier "b"
                  , QuestionMark
                  , Integer 5
                  , Colon
                  , Identifier "flag"
                  , QuestionMark
                  , Integer 6
                  , Colon
                  , Integer 7
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
           [Declaration "a" (Just (Constant 1))
          , Declaration "b" (Just (Constant 2))
          , Declaration "flag" (Just (Constant 0))
          , Statement . Return $
            (Conditional
              (Binary
                AST.GreaterThan
                (Reference "a")
                (Reference "b"))
              (Constant 5)
              (Conditional
                (Reference "flag")
                (Constant 6)
                (Constant 7)))]))

    it "should parse tokens from nested_ternary_2.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , QuestionMark
                  , Integer 2
                  , QuestionMark
                  , Integer 3
                  , Colon
                  , Integer 4
                  , Colon
                  , Integer 5
                  , Semicolon
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 0
                  , QuestionMark
                  , Integer 2
                  , QuestionMark
                  , Integer 3
                  , Colon
                  , Integer 4
                  , Colon
                  , Integer 5
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Token.Multiplication
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
           [Declaration "a"
            (Just
              (Conditional
                (Constant 1)
                (Conditional
                  (Constant 2)
                  (Constant 3)
                  (Constant 4))
                (Constant 5)))
          , Declaration "b"
            (Just
              (Conditional
                (Constant 0)
                (Conditional
                  (Constant 2)
                  (Constant 3)
                  (Constant 4))
                (Constant 5)))
          , (Statement
              (Return
                (Binary
                 AST.Multiplication
                 (Reference "a")
                 (Reference "b"))))]))

    it "should parse tokens from rh_assignment.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "flag"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , Identifier "flag"
                  , QuestionMark
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Colon
                  , OpenParen
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 0
                  , CloseParen
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "flag" (Just (Constant 1))
            , Declaration "a" (Just (Constant 0))
            , (Statement
                (Expression
                  (Conditional
                    (Reference "flag")
                    (AST.Assignment "a" (Constant 1))
                    (AST.Assignment "a" (Constant 0)))))
            , Statement (Return (Reference "a"))]))

    it "should parse tokens from ternary.c" $ do
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
                  , KWReturn
                  , Identifier "a"
                  , Token.GreaterThan
                  , Token.Negation
                  , Integer 1
                  , QuestionMark
                  , Integer 4
                  , Colon
                  , Integer 5
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (Return
                (Conditional
                  (Binary
                    AST.GreaterThan
                    (Reference "a")
                    (Unary AST.Negation (Constant 1)))
                  (Constant 4)
                  (Constant 5)))]))

    it "should fail to parse tokens from incomplete_ternary.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , QuestionMark
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from malformed_ternary.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , QuestionMark
                  , Integer 2
                  , Colon
                  , Integer 3
                  , Colon
                  , Integer 4
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from malformed_ternary_2.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , QuestionMark
                  , Integer 2
                  , QuestionMark
                  , Integer 3
                  , Colon
                  , Integer 4
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from ternary_assign.c" $ do
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
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , Identifier "a"
                  , Token.GreaterThan
                  , Identifier "b"
                  , QuestionMark
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Colon
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should parse tokens from else.c" $ do
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
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , CloseParen
                  , KWReturn
                  , Integer 1
                  , Semicolon
                  , KWElse
                  , KWReturn
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Return (Constant 1))
                (Just (Return (Constant 2))))]))

    it "should parse tokens from if_nested.c" $ do
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
                  , Integer 0
                  , Semicolon
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , CloseParen
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWElse
                  , KWIf
                  , OpenParen
                  , Identifier "b"
                  , CloseParen
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 1))
            , Declaration "b" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Expression (AST.Assignment "b" (Constant 1)))
                (Just
                  (If
                    (Reference "b")
                    (Expression (AST.Assignment "b" (Constant 2)))
                    Nothing)))
            , Statement (Return (Reference "b"))]))

    it "should parse tokens from if_nested_2.c" $ do
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
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , CloseParen
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWElse
                  , KWIf
                  , OpenParen
                  , Identifier "b"
                  , CloseParen
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Declaration "b" (Just (Constant 1))
            , Statement
              (If
                (Reference "a")
                (Expression (AST.Assignment "b" (Constant 1)))
                (Just
                  (If
                    (Reference "b")
                    (Expression (AST.Assignment "b" (Constant 2)))
                    Nothing)))
            , Statement (Return (Reference "b"))]))

    it "should parse tokens from if_nested_3.c" $ do
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
                  , KWIf
                  , OpenParen
                  , Integer 1
                  , CloseParen
                  , KWIf
                  , OpenParen
                  , Integer 2
                  , CloseParen
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWElse
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 4
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (If
                (Constant 1)
                (If
                  (Constant 2)
                  (Expression (AST.Assignment "a" (Constant 3)))
                  (Just (Expression (AST.Assignment "a" (Constant 4)))))
                Nothing)
            , Statement (Return (Reference "a"))]))

    it "should parse tokens from if_nested_4.c" $ do
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
                  , KWIf
                  , OpenParen
                  , Integer 1
                  , CloseParen
                  , KWIf
                  , OpenParen
                  , Integer 0
                  , CloseParen
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWElse
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 4
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (If
                (Constant 1)
                (If
                  (Constant 0)
                  (Expression (AST.Assignment "a" (Constant 3)))
                  (Just (Expression (AST.Assignment "a" (Constant 4)))))
                Nothing)
            , Statement (Return (Reference "a"))]))

    it "should parse tokens from if_nested_5.c" $ do
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
                  , KWIf
                  , OpenParen
                  , Integer 0
                  , CloseParen
                  , KWIf
                  , OpenParen
                  , Integer 0
                  , CloseParen
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWElse
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 4
                  , Semicolon
                  , KWElse
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (If
                (Constant 0)
                (If
                  (Constant 0)
                  (Expression (AST.Assignment "a" (Constant 3)))
                  (Just (Expression (AST.Assignment "a" (Constant 4)))))
                (Just (Expression (AST.Assignment "a" (Constant 1)))))
            , Statement (Return (Reference "a"))]))

    it "should parse tokens from if_not_taken.c" $ do
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
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , CloseParen
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Declaration "b" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Expression (AST.Assignment "b" (Constant 1)))
                Nothing)
            , Statement (Return (Reference "b"))]))

    it "should parse tokens from if_taken.c" $ do
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
                  , Integer 0
                  , Semicolon
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , CloseParen
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 1))
            , Declaration "b" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Expression (AST.Assignment "b" (Constant 1)))
                Nothing)
            , Statement (Return (Reference "b"))]))

    it "should parse tokens from multiple_if.c" $ do
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
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , CloseParen
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWElse
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWIf
                  , OpenParen
                  , Identifier "b"
                  , CloseParen
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 4
                  , Semicolon
                  , KWElse
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 5
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Token.Addition
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Declaration "b" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Expression (AST.Assignment "a" (Constant 2)))
                (Just (Expression (AST.Assignment "a" (Constant 3)))))
            , Statement
              (If
                (Reference "b")
                (Expression (AST.Assignment "b" (Constant 4)))
                (Just (Expression (AST.Assignment "b" (Constant 5)))))
            , Statement
              (Return
                (Binary
                  AST.Addition
                  (Reference "a")
                  (Reference "b")))]))

  describe "Stage 7" $ do

    it "should parse tokens from consecutive_blocks.c" $ do
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
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , CloseBrace
                  , OpenBrace
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 1))
            , Statement (Compound [Declaration "a" (Just (Constant 2))])
            , Statement (Compound [Statement (Return (Reference "a"))])
            ]))

    it "should parse tokens from consecutive_declarations.c" $ do
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
                  , OpenBrace
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , Identifier "a"
                  , Token.Assignment
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace
                  , OpenBrace
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , Identifier "a"
                  , Token.Assignment
                  , Identifier "a"
                  , Token.Addition
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (Compound
                [ Declaration "b" (Just (Constant 1))
                , Statement (Expression (AST.Assignment "a" (Reference "b")))])
            , Statement
              (Compound
                [ Declaration "b" (Just (Constant 2))
                , Statement
                  (Expression
                    (AST.Assignment "a"
                      (Binary AST.Addition
                        (Reference "a")
                        (Reference "b"))))])
            , Statement (Return (Reference "a"))]))

    it "should parse tokens from declare_after_block.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "i"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , CloseBrace
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "i" (Just (Constant 0))
            , Statement
              (Compound [Declaration "a" (Just (Constant 2))])
            , Declaration "b" (Just (Constant 3))
            , Statement (Return (Reference "b"))]))

    it "should parse tokens from declare_block.c" $ do
      parseTokens [KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWIf
                  , OpenParen
                  , Integer 5
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "i"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , KWReturn
                  , Identifier "i"
                  , Semicolon
                  , CloseBrace
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Statement
              (If
                (Constant 5)
                (Compound
                  [ Declaration "i" (Just (Constant 0))
                  , Statement (Return (Reference "i"))])
                Nothing)]))

    it "should parse tokens from declare_late.c" $ do
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
                  , OpenBrace
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 0
                  , Semicolon
                  , CloseBrace
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 2))
            , Statement
              (Compound
                [ Statement
                  (Expression
                    (AST.Assignment "a" (Constant 3)))
                , Declaration "a" (Just (Constant 0))])
            , Statement (Return (Reference "a"))]))

    it "should parse tokens from multi_nesting.c" $ do
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
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , Token.LessThan
                  , Integer 3
                  , CloseParen
                  , OpenBrace
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 2))
            , Statement
              (If
                (Binary AST.LessThan
                  (Reference "a")
                  (Constant 3))
                (Compound
                 [ Statement
                   (Compound
                     [ Declaration "a" (Just (Constant 3))
                     , Statement (Return (Reference "a"))])
                 , Statement (Return (Reference "a"))])
              Nothing)]))

    it "should parse tokens from nested_if.c" $ do
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
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , CloseParen
                  , OpenBrace
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace
                  , KWElse
                  , OpenBrace
                  , KWInt
                  , Identifier "c"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , KWIf
                  , OpenParen
                  , Identifier "a"
                  , Token.LessThan
                  , Identifier "c"
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 4
                  , Semicolon
                  , CloseBrace
                  , KWElse
                  , OpenBrace
                  , KWReturn
                  , Integer 5
                  , Semicolon
                  , CloseBrace
                  , CloseBrace
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Compound
                  [ Declaration "b" (Just (Constant 2))
                  , Statement (Return (Reference "b"))])
                (Just
                  (Compound
                    [ Declaration "c" (Just (Constant 3))
                    , Statement
                      (If
                        (Binary AST.LessThan
                          (Reference "a")
                          (Reference "c"))
                        (Compound [Statement (Return (Constant 4))])
                        (Just (Compound [Statement (Return (Constant 5))])))])))
            , Statement (Return (Reference "a"))]))

    it "should parse tokens from nested_scope.c" $ do
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
                  , KWInt
                  , Identifier "b"
                  , Token.Assignment
                  , Integer 3
                  , Semicolon
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 1
                  , Semicolon
                  , Identifier "b"
                  , Token.Assignment
                  , Identifier "b"
                  , Token.Addition
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace
                  , KWReturn
                  , Identifier "b"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 2))
            , Declaration "b" (Just (Constant 3))
            , Statement
              (Compound
                [ Declaration "a" (Just (Constant 1))
                , Statement
                  (Expression
                    (AST.Assignment "b"
                      (Binary AST.Addition
                        (Reference "b")
                        (Reference "a"))))])
            , Statement (Return (Reference "b"))]))

    it "should parse tokens from double_define.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Semicolon
                  , KWInt
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [Statement
              (Compound
                [ Declaration "a" Nothing
                , Declaration "a" Nothing])]))

    it "should parse tokens from out_of_scope.c" $ do
      parseTokens [KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , OpenBrace
                  , KWInt
                  , Identifier "a"
                  , Token.Assignment
                  , Integer 2
                  , Semicolon
                  , CloseBrace
                  , KWReturn
                  , Identifier "a"
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Right
        (Program
          (Function "main"
            [ Statement (Compound [Declaration "a" (Just (Constant 2))])
            , Statement (Return (Reference "a"))]))

    it "should fail to parse tokens from syntax_err_extra_brace.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWIf
                  , OpenParen
                  , Integer 0
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Semicolon
                  , CloseBrace
                  , CloseBrace
                  , KWReturn
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_missing_brace.c" $ do
      parseTokens [ KWInt
                  , Identifier "main"
                  , OpenParen
                  , CloseParen
                  , OpenBrace
                  , KWIf
                  , OpenParen
                  , Integer 0
                  , CloseParen
                  , OpenBrace
                  , KWReturn
                  , Integer 1
                  , Semicolon
                  , KWReturn
                  , Integer 2
                  , Semicolon
                  , CloseBrace]
        `shouldBe`
        Left (ParserError "Failed to parse the program.")
