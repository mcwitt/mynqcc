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
                    Unary (
                        AST.LogicalNegation)(
                        Constant 12)))))

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
                    Unary (
                        AST.BitwiseComplement)(
                        Constant 0)))))

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
                    Unary (
                        AST.Negation)(
                        Constant 5)))))

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
                    Unary (
                        AST.LogicalNegation)(
                        Unary (
                            AST.Negation)(
                            Constant 3))))))

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
                    Unary (
                        AST.Negation)(
                        Unary (
                            AST.BitwiseComplement)(
                            Constant 0))))))

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
                    Unary (
                        AST.LogicalNegation)(
                        Constant 5)))))

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
                    Unary (
                        AST.LogicalNegation)(
                        Constant 0)))))

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
                    Binary (
                        AST.Addition)(
                        Constant 1)(
                        Constant 2)))))

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
                    Binary (
                        AST.Subtraction)(
                        Binary (
                            AST.Subtraction)(
                            Constant 1)(
                            Constant 2))(
                        Constant 3)))))

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
                    Binary (
                        AST.Division)(
                        Binary (
                            AST.Division)(
                            Constant 6)(
                            Constant 3))(
                        Constant 2)))))

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
                    Binary (
                        AST.Division)(
                        Constant 4)(
                        Constant 2)))))

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
                    Binary (
                        AST.Multiplication)(
                        Constant 2)(
                        Constant 3)))))

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
                    Binary (
                        AST.Multiplication)(
                        Constant 2)(
                        Binary (
                            AST.Addition)(
                            Constant 3)(
                            Constant 4))))))

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
                    Binary (
                        AST.Addition)(
                        Constant 2)(
                        Binary (
                            AST.Multiplication)(
                            Constant 3)(
                            Constant 4))))))

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
                    Binary (
                        AST.Subtraction)(
                        Constant 2)(
                        Unary (
                            AST.Negation)(
                            Constant 1))))))

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
                    Binary (
                        AST.Addition)(
                        Unary (
                            AST.BitwiseComplement)(
                            Constant 2))(
                        Constant 3)))))

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
                    Unary (
                        AST.BitwiseComplement)(
                        Binary (
                            AST.Addition)(
                            Constant 1)(
                            Constant 1))))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LogicalAnd)(
                        Constant 1)(
                        Constant 0)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LogicalAnd)(
                        Constant 1)(
                        Unary (
                            AST.Negation)(
                            Constant 1))))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.Equality)(
                        Constant 1)(
                        Constant 2)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.Equality)(
                        Constant 1)(
                        Constant 1)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.GreaterEqual)(
                        Constant 1)(
                        Constant 2)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.GreaterEqual)(
                        Constant 1)(
                        Constant 1)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.GreaterThan)(
                        Constant 1)(
                        Constant 2)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.GreaterThan)(
                        Constant 1)(
                        Constant 0)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LessEqual)(
                        Constant 1)(
                        Unary (
                            AST.Negation)(
                            Constant 1))))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LessEqual)(
                        Constant 0)(
                        Constant 2)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LessThan)(
                        Constant 2)(
                        Constant 1)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LessThan)(
                        Constant 1)(
                        Constant 2)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.Inequality)(
                        Constant 0)(
                        Constant 0)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.Inequality)(
                        Unary (
                            AST.Negation)(
                            Constant 1))(
                        Unary (
                            AST.Negation)(
                            Constant 2))))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LogicalOr)(
                        Constant 0)(
                        Constant 0)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LogicalOr)(
                        Constant 1)(
                        Constant 0)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LogicalOr)(
                        Constant 1)(
                        Binary (
                            AST.LogicalAnd)(
                            Constant 0)(
                            Constant 2))))))


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
            Function "main" (
                Return (
                    Binary (
                        AST.LogicalAnd)(
                        Binary (
                            AST.LogicalOr)(
                            Constant 1)(
                            Constant 0))(
                        Constant 0)))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.Equality)(
                        Constant 2)(
                        Binary (
                            AST.GreaterThan)(
                            Constant 2)(
                            Constant 0))))))

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
            Function "main" (
                Return (
                    Binary (
                        AST.LogicalOr)(
                        Binary (
                            AST.Equality)(
                            Constant 2)(
                            Constant 2))(
                        Constant 0)))))

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
