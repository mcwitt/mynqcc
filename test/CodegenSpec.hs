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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $100, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $12, %eax"
              , "cmpl $0, %eax"
              , "movl $0, %eax"
              , "sete %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "addl %ecx, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $3, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "subl %ecx, %eax"
              , "pop %ecx"
              , "subl %ecx, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $3, %eax"
              , "push %eax"
              , "movl $6, %eax"
              , "pop %ecx"
              , "movl $0, %edx"
              , "idivl %ecx"
              , "pop %ecx"
              , "movl $0, %edx"
              , "idivl %ecx"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $4, %eax"
              , "pop %ecx"
              , "movl $0, %edx"
              , "idivl %ecx"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $3, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "pop %ecx"
              , "imul %ecx, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $4, %eax"
              , "push %eax"
              , "movl $3, %eax"
              , "pop %ecx"
              , "addl %ecx, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "pop %ecx"
              , "imul %ecx, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "cmpl $0, %ecx"
              , "setne %cl"
              , "cmpl $0, %eax"
              , "movl $0, %eax"
              , "setne %al"
              , "andb %cl, %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "neg %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "cmpl $0, %ecx"
              , "setne %cl"
              , "cmpl $0, %eax"
              , "movl $0, %eax"
              , "setne %al"
              , "andb %cl, %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "sete %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "sete %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "setge %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "setge %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

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
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "pop %ecx"
              , "cmpl $0, %ecx"
              , "setne %cl"
              , "cmpl $0, %eax"
              , "movl $0, %eax"
              , "setne %al"
              , "andb %cl, %al"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "orl %ecx, %eax"
              , "movl $0, %eax"
              , "setne %al"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

  describe "Stage 5" $ do

    it "should generate code for assign.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , Expression (AST.Assignment "a" (Constant 2))
                , Return (Reference "a")]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl %ebp, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]
