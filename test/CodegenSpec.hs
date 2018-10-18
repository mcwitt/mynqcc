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
                    Expression (
                        Term (
                            Constant 100))))))
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
                    Expression (
                        Term (
                            AST.LogicalNegation (
                                Constant 12)))))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $12, %eax\n\
        \cmpl $0, %eax\n\
        \movl $0, %eax\n\
        \sete %al\n\
        \ret"

  describe "Stage 3" $ do

    it "should generate code for add.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    AST.Addition (
                        Expression (Term (Constant 1)))(
                        Expression (Term (Constant 2)))))))
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
                    AST.Subtraction (
                        AST.Subtraction (
                            Expression (Term (Constant 1)))(
                            Expression (Term (Constant 2))))(
                        Expression (Term (Constant 3)))))))
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
      generate (Program (
                   Function "main" (
                       Return (
                           Expression (
                               AST.Division (
                                   AST.Division (
                                       Term (Constant 6))(
                                       Term (Constant 3)))(
                                   Term (Constant 2)))))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $3, %eax\n\
        \push %eax\n\
        \movl $6, %eax\n\
        \pop %ecx\n\
        \movl $0, %edx\n\
        \idivl %ecx\n\
        \pop %ecx\n\
        \movl $0, %edx\n\
        \idivl %ecx\n\
        \ret"

    it "should generate code for div.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    Expression (
                        AST.Division (
                            Term (Constant 4))(
                            Term (Constant 2)))))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $4, %eax\n\
        \pop %ecx\n\
        \movl $0, %edx\n\
        \idivl %ecx\n\
        \ret"

    it "should generate code for mult.c" $ do
      generate (
        Program (
            Function "main" (
                Return (
                    Expression (
                        AST.Multiplication (
                            Term (Constant 2))(
                            Term (Constant 3)))))))
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
                    Expression (
                        AST.Multiplication (
                            Term (Constant 2))(
                            Term (
                                Factor (
                                    AST.Addition (
                                        Expression (Term (Constant 3)))(
                                        Expression (Term (Constant 4)))))))))))
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
