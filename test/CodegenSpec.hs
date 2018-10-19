module CodegenSpec (spec) where

import Test.Hspec        ( Spec
                         , describe
                         , it
                         , shouldBe
                         )

import Codegen
import Parser
import AST

spec :: Spec
spec = do
  describe "Stage 1" $ do

    it "should generate code for multi_digit.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    Constant 100))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $100, %eax\n\
        \ret"

  describe "Stage 2" $ do

    it "should generate code for bitwise.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    Unary (
                        AST.LogicalNegation)(
                        Constant 12)))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $12, %eax\n\
        \cmpl $0, %eax\n\
        \xorl %eax, %eax\n\
        \sete %al\n\
        \ret"

  describe "Stage 3" $ do

    it "should generate code for add.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    Binary (
                        AST.Addition)(
                        Constant 1)(
                        Constant 2)))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \addl %ecx, %eax\n\
        \ret"

    it "should generate code for associativity.c" $ do
      generate (
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
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $3, %eax\n\
        \push %eax\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \subl %ecx, %eax\n\
        \pop %ecx\n\
        \subl %ecx, %eax\n\
        \ret"

    it "should generate code for associativity_2.c" $ do
      generate (
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
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $3, %eax\n\
        \push %eax\n\
        \movl $6, %eax\n\
        \pop %ecx\n\
        \xorl %edx, %edx\n\
        \idivl %ecx\n\
        \pop %ecx\n\
        \xorl %edx, %edx\n\
        \idivl %ecx\n\
        \ret"

    it "should generate code for div.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    Binary (
                        AST.Division)(
                        Constant 4)(
                        Constant 2)))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $4, %eax\n\
        \pop %ecx\n\
        \xorl %edx, %edx\n\
        \idivl %ecx\n\
        \ret"

    it "should generate code for mult.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    Binary (
                        AST.Multiplication)(
                        Constant 2)(
                        Constant 3)))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $3, %eax\n\
        \push %eax\n\
        \movl $2, %eax\n\
        \pop %ecx\n\
        \imul %ecx, %eax\n\
        \ret"

    it "should generate code for parens.c" $ do
      generate (
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
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $4, %eax\n\
        \push %eax\n\
        \movl $3, %eax\n\
        \pop %ecx\n\
        \addl %ecx, %eax\n\
        \push %eax\n\
        \movl $2, %eax\n\
        \pop %ecx\n\
        \imul %ecx, %eax\n\
        \ret"
