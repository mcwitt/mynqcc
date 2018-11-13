module CodegenSpec (spec) where

import           AST
import           Codegen
import           Error      (Error (CodegenError))
import           Parser
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Stage 1" $ do

    it "should generate code for multi_digit.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
                    Constant 100)]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $100, %eax"
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

  describe "Stage 2" $ do

    it "should generate code for bitwise.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

  describe "Stage 3" $ do

    it "should generate code for add.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for associativity.c" $ do
      generate (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for associativity_2.c" $ do
      generate (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for div.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for mult.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for parens.c" $ do
      generate (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

  describe "Stage 4" $ do

    it "should generate code for and_false.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for and_true.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for eq_false.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for eq_true.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for ge_false.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for ge_true.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for precedence.c" $ do
      generate (
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

  describe "Stage 5" $ do

    it "should generate code for assign.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , Statement . Expression $ (AST.Assignment "a" (Constant 2))
                , Statement . Return $ (Reference "a")]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "push %eax"
              , "movl $2, %eax"
              , "movl %eax, -4(%ebp)"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
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
                , Statement . Return $ (Reference "b")]))
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
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for exp_return_val.c" $ do
      generate (
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
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for initialize.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 2))
                , Statement . Return $ (Constant 0)]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for multiple_vars.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 1))
                , Declaration "b" (Just (Constant 2))
                , Statement . Return $ (
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
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for no_initialize.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" Nothing
                , Statement . Return $ (Constant 0)]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "push %eax"
              , "movl $0, %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for refer.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 2))
                , Statement . Return $ (Reference "a")]))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for unused_exp.c" $ do
      generate (
        Program (
            Function "main" [
                  Statement . Expression $ (
                      Binary (
                          AST.Addition)(
                          Constant 2)(
                          Constant 2))
                , Statement . Return $ (Constant 0)]))
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
              , "addl $0, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should fail to generate code for redefine.c" $ do
      generate (
        Program (
            Function "main" [
                  Declaration "a" (Just (Constant 1))
                , Declaration "a" (Just (Constant 2))
                , Statement . Return $ (Reference "a")]))
        `shouldBe`
        Left (CodegenError "Variable `a` was declared more than once.")

    it "should fail to generate code for undeclared_var.c" $ do
      generate (
        Program (
            Function "main" [
                Statement . Return $ (Reference "a")]))
        `shouldBe`
        Left (CodegenError "Reference to undeclared variable, `a`.")

    it "should fail to generate code for var_declared_late.c" $ do
      generate (
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
        `shouldBe`
        Left (CodegenError "Assignment to undeclared variable, `a`.")

  describe "Stage 6" $ do
    it "should generate code for assign_ternary.c" $ do
      (generate
        (Program
          (Function "main"
           [ Declaration "a" (Just (Constant 0))
           , (Statement
               (Expression
                 (AST.Assignment "a"
                   (Conditional
                     (Constant 1)
                     (Constant 2)
                     (Constant 3)))))
           , (Statement (Return (Reference "a")))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "cmpl $0, %eax"
              , "je _main__e3__0"
              , "movl $2, %eax"
              , "jmp _main__post_conditional__1"
              , "_main__e3__0:"
              , "movl $3, %eax"
              , "_main__post_conditional__1:"
              , "movl %eax, -4(%ebp)"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for multiple_ternary.c" $ do
      (generate
        (Program
          (Function "main"
           [ Declaration "a"
             (Just
               (Conditional
                 (Binary AST.GreaterThan (Constant 1) (Constant 2))
                 (Constant 3)
                 (Constant 4)))
           , Declaration "b"
             (Just
               (Conditional
                 (Binary AST.GreaterThan (Constant 1) (Constant 2))
                 (Constant 5)
                 (Constant 6)))
           , (Statement
               (Return
                 (Binary AST.Addition
                  (Reference "a")
                  (Reference "b"))))])))
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
              , "setg %al"
              , "cmpl $0, %eax"
              , "je _main__e3__0"
              , "movl $3, %eax"
              , "jmp _main__post_conditional__1"
              , "_main__e3__0:"
              , "movl $4, %eax"
              , "_main__post_conditional__1:"
              , "push %eax"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "setg %al"
              , "cmpl $0, %eax"
              , "je _main__e3__2"
              , "movl $5, %eax"
              , "jmp _main__post_conditional__3"
              , "_main__e3__2:"
              , "movl $6, %eax"
              , "_main__post_conditional__3:"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "addl %ecx, %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for nested_ternary.c" $ do
      (generate
        (Program
          (Function "main"
           [ Declaration "a" (Just (Constant 1))
           , Declaration "b" (Just (Constant 2))
           , Declaration "flag" (Just (Constant 0))
           , (Statement
               (Return
                 (Conditional
                   (Binary
                     AST.GreaterThan
                     (Reference "a")
                     (Reference "b"))
                   (Constant 5)
                   (Conditional
                     (Reference "flag")
                     (Constant 6)
                     (Constant 7)))))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "setg %al"
              , "cmpl $0, %eax"
              , "je _main__e3__0"
              , "movl $5, %eax"
              , "jmp _main__post_conditional__1"
              , "_main__e3__0:"
              , "movl -12(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__e3__2"
              , "movl $6, %eax"
              , "jmp _main__post_conditional__3"
              , "_main__e3__2:"
              , "movl $7, %eax"
              , "_main__post_conditional__3:"
              , "_main__post_conditional__1:"
              , "addl $12, %esp"
              , "jmp _main__end"
              , "addl $12, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for nested_ternary_2.c" $ do
      (generate
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
                 (Reference "b"))))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "cmpl $0, %eax"
              , "je _main__e3__0"
              , "movl $2, %eax"
              , "cmpl $0, %eax"
              , "je _main__e3__1"
              , "movl $3, %eax"
              , "jmp _main__post_conditional__2"
              , "_main__e3__1:"
              , "movl $4, %eax"
              , "_main__post_conditional__2:"
              , "jmp _main__post_conditional__3"
              , "_main__e3__0:"
              , "movl $5, %eax"
              , "_main__post_conditional__3:"
              , "push %eax"
              , "movl $0, %eax"
              , "cmpl $0, %eax"
              , "je _main__e3__4"
              , "movl $2, %eax"
              , "cmpl $0, %eax"
              , "je _main__e3__5"
              , "movl $3, %eax"
              , "jmp _main__post_conditional__6"
              , "_main__e3__5:"
              , "movl $4, %eax"
              , "_main__post_conditional__6:"
              , "jmp _main__post_conditional__7"
              , "_main__e3__4:"
              , "movl $5, %eax"
              , "_main__post_conditional__7:"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "imul %ecx, %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for rh_assignment.c" $ do
      (generate
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
            , Statement (Return (Reference "a"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__e3__0"
              , "movl $1, %eax"
              , "movl %eax, -8(%ebp)"
              , "jmp _main__post_conditional__1"
              , "_main__e3__0:"
              , "movl $0, %eax"
              , "movl %eax, -8(%ebp)"
              , "_main__post_conditional__1:"
              , "movl -8(%ebp), %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for ternary.c" $ do
      (generate
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
                  (Constant 5)))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "neg %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "setg %al"
              , "cmpl $0, %eax"
              , "je _main__e3__0"
              , "movl $4, %eax"
              , "jmp _main__post_conditional__1"
              , "_main__e3__0:"
              , "movl $5, %eax"
              , "_main__post_conditional__1:"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for ternary.c" $ do
      (generate
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
                  (Constant 5)))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "neg %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "setg %al"
              , "cmpl $0, %eax"
              , "je _main__e3__0"
              , "movl $4, %eax"
              , "jmp _main__post_conditional__1"
              , "_main__e3__0:"
              , "movl $5, %eax"
              , "_main__post_conditional__1:"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for else.c" $ do
      (generate
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Return (Constant 1))
                (Just (Return (Constant 2))))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $1, %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "jmp _main__endif__1"
              , "_main__else__0:"
              , "movl $2, %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "_main__endif__1:"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for if_nested.c" $ do
      (generate
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
            , Statement (Return (Reference "b"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $1, %eax"
              , "movl %eax, -8(%ebp)"
              , "jmp _main__endif__1"
              , "_main__else__0:"
              , "movl -8(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__else__2"
              , "movl $2, %eax"
              , "movl %eax, -8(%ebp)"
              , "jmp _main__endif__3"
              , "_main__else__2:"
              , "_main__endif__3:"
              , "_main__endif__1:"
              , "movl -8(%ebp), %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for if_nested_2.c" $ do
      (generate
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
            , Statement (Return (Reference "b"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $1, %eax"
              , "movl %eax, -8(%ebp)"
              , "jmp _main__endif__1"
              , "_main__else__0:"
              , "movl -8(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__else__2"
              , "movl $2, %eax"
              , "movl %eax, -8(%ebp)"
              , "jmp _main__endif__3"
              , "_main__else__2:"
              , "_main__endif__3:"
              , "_main__endif__1:"
              , "movl -8(%ebp), %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for if_nested_3.c" $ do
      (generate
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
            , Statement (Return (Reference "a"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $2, %eax"
              , "cmpl $0, %eax"
              , "je _main__else__1"
              , "movl $3, %eax"
              , "movl %eax, -4(%ebp)"
              , "jmp _main__endif__2"
              , "_main__else__1:"
              , "movl $4, %eax"
              , "movl %eax, -4(%ebp)"
              , "_main__endif__2:"
              , "jmp _main__endif__3"
              , "_main__else__0:"
              , "_main__endif__3:"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for if_nested_4.c" $ do
      (generate
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
            , Statement (Return (Reference "a"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $0, %eax"
              , "cmpl $0, %eax"
              , "je _main__else__1"
              , "movl $3, %eax"
              , "movl %eax, -4(%ebp)"
              , "jmp _main__endif__2"
              , "_main__else__1:"
              , "movl $4, %eax"
              , "movl %eax, -4(%ebp)"
              , "_main__endif__2:"
              , "jmp _main__endif__3"
              , "_main__else__0:"
              , "_main__endif__3:"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for if_nested_5.c" $ do
      (generate
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
            , Statement (Return (Reference "a"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $0, %eax"
              , "cmpl $0, %eax"
              , "je _main__else__1"
              , "movl $3, %eax"
              , "movl %eax, -4(%ebp)"
              , "jmp _main__endif__2"
              , "_main__else__1:"
              , "movl $4, %eax"
              , "movl %eax, -4(%ebp)"
              , "_main__endif__2:"
              , "jmp _main__endif__3"
              , "_main__else__0:"
              , "movl $1, %eax"
              , "movl %eax, -4(%ebp)"
              , "_main__endif__3:"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for if_not_taken.c" $ do
      (generate
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 0))
            , Declaration "b" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Expression (AST.Assignment "b" (Constant 1)))
                Nothing)
            , Statement (Return (Reference "b"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $1, %eax"
              , "movl %eax, -8(%ebp)"
              , "jmp _main__endif__1"
              , "_main__else__0:"
              , "_main__endif__1:"
              , "movl -8(%ebp), %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for if_taken.c" $ do
      (generate
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 1))
            , Declaration "b" (Just (Constant 0))
            , Statement
              (If
                (Reference "a")
                (Expression (AST.Assignment "b" (Constant 1)))
                Nothing)
            , Statement (Return (Reference "b"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $0, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $1, %eax"
              , "movl %eax, -8(%ebp)"
              , "jmp _main__endif__1"
              , "_main__else__0:"
              , "_main__endif__1:"
              , "movl -8(%ebp), %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

  describe "Stage 7" $ do

    it "should generate code for consecutive_blocks.c" $ do
      (generate
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 1))
            , Statement (Compound [Declaration "a" (Just (Constant 2))])
            , Statement (Compound [Statement (Return (Reference "a"))])
            ])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $1, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "push %eax"
              , "addl $4, %esp"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for consecutive_declarations.c" $ do
      (generate
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
            , Statement (Return (Reference "a"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $1, %eax"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "movl %eax, -4(%ebp)"
              , "addl $4, %esp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "addl %ecx, %eax"
              , "movl %eax, -4(%ebp)"
              , "addl $4, %esp"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for declare_after_block.c" $ do
      (generate
        (Program
          (Function "main"
            [ Declaration "i" (Just (Constant 0))
            , Statement
              (Compound [Declaration "a" (Just (Constant 2))])
            , Declaration "b" (Just (Constant 3))
            , Statement (Return (Reference "b"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $0, %eax"
              , "push %eax"
              , "movl $2, %eax"
              , "push %eax"
              , "addl $4, %esp"
              , "movl $3, %eax"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $8, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for declare_block.c" $ do
      (generate
        (Program
          (Function "main"
            [ Statement
              (If
                (Constant 5)
                (Compound
                  [ Declaration "i" (Just (Constant 0))
                  , Statement (Return (Reference "i"))])
                Nothing)])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $5, %eax"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $0, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "jmp _main__endif__1"
              , "_main__else__0:"
              , "_main__endif__1:"
              , "addl $0, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for declare_late.c" $ do
      (generate
        (Program
          (Function "main"
            [ Declaration "a" (Just (Constant 2))
            , Statement
              (Compound
                [ Statement
                  (Expression
                    (AST.Assignment "a" (Constant 3)))
                , Declaration "a" (Just (Constant 0))])
            , Statement (Return (Reference "a"))])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $3, %eax"
              , "movl %eax, -4(%ebp)"
              , "movl $0, %eax"
              , "push %eax"
              , "addl $4, %esp"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]

    it "should generate code for multi_nesting.c" $ do
      (generate
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
              Nothing)])))
        `shouldBe`
        Right [ ".globl main"
              , "main:"
              , "push %ebp"
              , "movl %esp, %ebp"
              , "movl $2, %eax"
              , "push %eax"
              , "movl $3, %eax"
              , "push %eax"
              , "movl -4(%ebp), %eax"
              , "pop %ecx"
              , "cmpl %ecx, %eax"
              , "movl $0, %eax"
              , "setl %al"
              , "cmpl $0, %eax"
              , "je _main__else__0"
              , "movl $3, %eax"
              , "push %eax"
              , "movl -8(%ebp), %eax"
              , "addl $8, %esp"
              , "jmp _main__end"
              , "addl $4, %esp"
              , "movl -4(%ebp), %eax"
              , "addl $4, %esp"
              , "jmp _main__end"
              , "addl $0, %esp"
              , "jmp _main__endif__1"
              , "_main__else__0:"
              , "_main__endif__1:"
              , "addl $4, %esp"
              , "_main__end:"
              , "movl %ebp, %esp"
              , "pop %ebp"
              , "ret"]
