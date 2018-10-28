module CodegenSpec (spec) where

import           AST
import           Codegen
import           Error      (Error (CodegenError))
import           Parser
import           Test
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Stage 1" $ do

    it "should generate code for multi_digit.c" $ do
      generate (
        Program (
            Function "main" [
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                returnStatement (
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
                , expressionStatement (AST.Assignment "a" (Constant 2))
                , returnStatement (Reference "a")]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "push %eax"
              , "movl $2, %eax"
              , "movl %eax, -4(%ebp)"
              , "movl -4(%ebp), %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for assign_val.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , Declaration "b" (
                    Just (AST.Assignment "a" (Constant 0)))
                , returnStatement (Reference "b")]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "push %eax"
              , "movl $0, %eax"
              , "movl %eax, -4(%ebp)"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for exp_return_val.c" $ do
      generate (
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
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "push %eax"
              , "push %eax"
              , "movl $4, %eax"
              , "movl %eax, -8(%ebp)"
              , "movl %eax, -4(%ebp)"
              , "movl -8(%ebp), %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "subl %ecx, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for initialize.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 2))
                , returnStatement (Constant 0)]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for missing_return.c" $ do
      generate (Program (Function "main" []))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for multiple_vars.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 1))
                , Declaration "b" (Just (Constant 2))
                , returnStatement (
                    Binary (
                        AST.Addition)(
                        Reference "a")(
                        Reference "b"))]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "addl %ecx, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for no_initialize.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , returnStatement (Constant 0)]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "push %eax"
              , "movl $0, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for refer.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 2))
                , returnStatement (Reference "a")]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for unused_exp.c" $ do
      generate (
        Program (
            Function "main" [
                  expressionStatement (
                      Binary (
                          AST.Addition)(
                          Constant 2)(
                          Constant 2))
                , returnStatement (Constant 0)]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "pop %ecx"
              , "addl %ecx, %eax"
              , "movl $0, %eax"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should fail to generate code for redefine.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 1))
                , Declaration "a" (Just (Constant 2))
                , returnStatement (Reference "a")]))
        `shouldBe`
        Left (CodegenError "Variable `a` was declared more than once.")

    it "should fail to generate code for undeclared_var.c" $ do
      generate (
        Program (
            Function "main" [
                returnStatement (Reference "a")]))
        `shouldBe`
        Left (CodegenError "Reference to undeclared variable, `a`.")

    it "should fail to generate code for var_declared_late.c" $ do
      generate (
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
        `shouldBe`
        Left (CodegenError "Assignment to undeclared variable, `a`.")
