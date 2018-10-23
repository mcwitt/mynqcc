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
            Function "main" [
                Return (
                    Constant 100)]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $100, %eax\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

  describe "Stage 2" $ do

    it "should generate code for bitwise.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Unary (
                        AST.LogicalNegation)(
                        Constant 12))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $12, %eax\n\
        \cmpl $0, %eax\n\
        \movl $0, %eax\n\
        \sete %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

  describe "Stage 3" $ do

    it "should generate code for add.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Addition)(
                        Constant 1)(
                        Constant 2))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \addl %ecx, %eax\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for associativity.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Subtraction)(
                        Binary (
                            AST.Subtraction)(
                            Constant 1)(
                            Constant 2))(
                        Constant 3))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $3, %eax\n\
        \push %eax\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \subl %ecx, %eax\n\
        \pop %ecx\n\
        \subl %ecx, %eax\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for associativity_2.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Division)(
                        Binary (
                            AST.Division)(
                            Constant 6)(
                            Constant 3))(
                        Constant 2))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
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
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for div.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Division)(
                        Constant 4)(
                        Constant 2))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $4, %eax\n\
        \pop %ecx\n\
        \movl $0, %edx\n\
        \idivl %ecx\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for mult.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Multiplication)(
                        Constant 2)(
                        Constant 3))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $3, %eax\n\
        \push %eax\n\
        \movl $2, %eax\n\
        \pop %ecx\n\
        \imul %ecx, %eax\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for parens.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Multiplication)(
                        Constant 2)(
                        Binary (
                            AST.Addition)(
                            Constant 3)(
                            Constant 4)))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $4, %eax\n\
        \push %eax\n\
        \movl $3, %eax\n\
        \pop %ecx\n\
        \addl %ecx, %eax\n\
        \push %eax\n\
        \movl $2, %eax\n\
        \pop %ecx\n\
        \imul %ecx, %eax\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

  describe "Stage 4" $ do

    it "should generate code for and_false.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.LogicalAnd)(
                        Constant 1)(
                        Constant 0))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $0, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \cmpl $0, %ecx\n\
        \setne %cl\n\
        \cmpl $0, %eax\n\
        \movl $0, %eax\n\
        \setne %al\n\
        \andb %cl, %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for and_true.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.LogicalAnd)(
                        Constant 1)(
                        Unary (
                            AST.Negation)(
                            Constant 1)))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $1, %eax\n\
        \neg %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \cmpl $0, %ecx\n\
        \setne %cl\n\
        \cmpl $0, %eax\n\
        \movl $0, %eax\n\
        \setne %al\n\
        \andb %cl, %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for eq_false.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Equality)(
                        Constant 1)(
                        Constant 2))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \cmpl %ecx, %eax\n\
        \movl $0, %eax\n\
        \sete %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for eq_true.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.Equality)(
                        Constant 1)(
                        Constant 1))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $1, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \cmpl %ecx, %eax\n\
        \movl $0, %eax\n\
        \sete %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for ge_false.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.GreaterEqual)(
                        Constant 1)(
                        Constant 2))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \cmpl %ecx, %eax\n\
        \movl $0, %eax\n\
        \setge %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for ge_true.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.GreaterEqual)(
                        Constant 1)(
                        Constant 1))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $1, %eax\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \cmpl %ecx, %eax\n\
        \movl $0, %eax\n\
        \setge %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

    it "should generate code for precedence.c" $ do
      generate (
        Program (
            Function "main" [
                Return (
                    Binary (
                        AST.LogicalOr)(
                        Constant 1)(
                        Binary (
                            AST.LogicalAnd)(
                            Constant 0)(
                            Constant 2)))]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl $0, %eax\n\
        \pop %ecx\n\
        \cmpl $0, %ecx\n\
        \setne %cl\n\
        \cmpl $0, %eax\n\
        \movl $0, %eax\n\
        \setne %al\n\
        \andb %cl, %al\n\
        \push %eax\n\
        \movl $1, %eax\n\
        \pop %ecx\n\
        \orl %ecx, %eax\n\
        \movl $0, %eax\n\
        \setne %al\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"

  describe "Stage 5" $ do

    it "should generate code for assign.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , Expression (AST.Assignment "a" (Constant 2))
                , Return (Reference "a")]))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \push %ebp\n\
        \movl %esp, %ebp\n\
        \movl $2, %eax\n\
        \push %eax\n\
        \movl %ebp, %eax\n\
        \movl %ebp, %esp\n\
        \pop %ebp\n\
        \ret"
