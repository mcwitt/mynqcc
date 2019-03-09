module CodegenSpec
  ( spec
  )
where

import           AST
import           Codegen
import           Error                          ( Error(CodegenError) )
import           Parser
import Target
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

testGenerate = generate (Target Linux)

spec :: Spec
spec = do
  describe "Stage 1"
    $          it "should generate code for multi_digit.c"
    $          testGenerate
                 (Program (Function "main" [] (Just [Statement (Return (Constant 100))]))
                 )
    `shouldBe` Right
                 [ ".globl main"
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
                 , "ret"
                 ]

  describe "Stage 2"
    $          it "should generate code for bitwise.c"
    $          testGenerate
                 (Program
                   (Function
                     "main"
                     []
                     (Just [Statement (Return (Unary AST.LogicalNegation (Constant 12)))]
                     )
                   )
                 )
    `shouldBe` Right
                 [ ".globl main"
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
                 , "ret"
                 ]

  describe "Stage 3" $ do

    it "should generate code for add.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return (Binary AST.Addition (Constant 1) (Constant 2)))
                         ]
                       )
                     )
                   )

      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for associativity.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return
                               (Binary
                                 AST.Subtraction
                                 (Binary AST.Subtraction (Constant 1) (Constant 2))
                                 (Constant 3)
                               )
                             )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for associativity_2.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return
                               (Binary AST.Division
                                       (Binary AST.Division (Constant 6) (Constant 3))
                                       (Constant 2)
                               )
                             )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for div.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return (Binary AST.Division (Constant 4) (Constant 2)))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for mult.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return
                               (Binary AST.Multiplication (Constant 2) (Constant 3))
                             )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for parens.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return
                               (Binary AST.Multiplication
                                       (Constant 2)
                                       (Binary AST.Addition (Constant 3) (Constant 4))
                               )
                             )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

  describe "Stage 4" $ do

    it "should generate code for and_false.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return (Binary AST.LogicalAnd (Constant 1) (Constant 0)))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for and_true.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return
                               (Binary AST.LogicalAnd
                                       (Constant 1)
                                       (Unary AST.Negation (Constant 1))
                               )
                             )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for eq_false.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return (Binary AST.Equality (Constant 1) (Constant 2)))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for eq_true.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return (Binary AST.Equality (Constant 1) (Constant 1)))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for ge_false.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return (Binary AST.GreaterEqual (Constant 1) (Constant 2)))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for ge_true.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return (Binary AST.GreaterEqual (Constant 1) (Constant 1)))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for precedence.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (Return
                               (Binary
                                 AST.LogicalOr
                                 (Constant 1)
                                 (Binary AST.LogicalAnd (Constant 0) (Constant 2))
                               )
                             )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

  describe "Stage 5" $ do

    it "should generate code for assign.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" Nothing)
                         , Statement
                           (Expression (Just (AST.Assignment "a" (Constant 2))))
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for assign_val.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" Nothing)
                         , Declaration
                           (Decl "b" (Just (AST.Assignment "a" (Constant 0))))
                         , Statement (Return (Reference "b"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for exp_return_val.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" Nothing)
                         , Declaration (Decl "b" Nothing)
                         , Statement
                           (Expression
                             (Just (AST.Assignment "a" (AST.Assignment "b" (Constant 4)))
                             )
                           )
                         , Statement
                           (Return
                             (Binary AST.Subtraction (Reference "a") (Reference "b"))
                           )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for initialize.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 2)))
                         , Statement (Return (Constant 0))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for missing_return.c"
      $          testGenerate (Program (Function "main" [] (Just [])))
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for multiple_vars.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 1)))
                         , Declaration (Decl "b" (Just (Constant 2)))
                         , Statement
                           (Return (Binary AST.Addition (Reference "a") (Reference "b")))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for no_initialize.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" Nothing)
                         , Statement (Return (Constant 0))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for refer.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 2)))
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for unused_exp.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                           (Expression
                             (Just (Binary AST.Addition (Constant 2) (Constant 2)))
                           )
                         , Statement (Return (Constant 0))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should fail to generate code for redefine.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 1)))
                         , Declaration (Decl "a" (Just (Constant 2)))
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Left
                   (CodegenError
                     "Multiple declarations of `a` in the same block."
                   )

    it "should fail to generate code for undeclared_var.c"
      $          testGenerate
                   (Program
                     (Function "main" [] (Just [Statement (Return (Reference "a"))]))
                   )
      `shouldBe` Left (CodegenError "Reference to undeclared variable, `a`.")

    it "should fail to generate code for var_declared_late.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                           (Expression
                             (Just
                               (AST.Assignment
                                 "a"
                                 (Binary AST.Addition (Constant 1) (Constant 2))
                               )
                             )
                           )
                         , Declaration (Decl "a" Nothing)
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Left (CodegenError "Assignment to undeclared variable, `a`.")

  describe "Stage 6" $ do
    it "should generate code for assign_ternary.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (Expression
                             (Just
                               (AST.Assignment
                                 "a"
                                 (Conditional (Constant 1) (Constant 2) (Constant 3))
                               )
                             )
                           )
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for multiple_ternary.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration
                           (Decl
                             "a"
                             (Just
                               (Conditional
                                 (Binary AST.GreaterThan (Constant 1) (Constant 2))
                                 (Constant 3)
                                 (Constant 4)
                               )
                             )
                           )
                         , Declaration
                           (Decl
                             "b"
                             (Just
                               (Conditional
                                 (Binary AST.GreaterThan (Constant 1) (Constant 2))
                                 (Constant 5)
                                 (Constant 6)
                               )
                             )
                           )
                         , Statement
                           (Return (Binary AST.Addition (Reference "a") (Reference "b")))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for nested_ternary.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 1)))
                         , Declaration (Decl "b" (Just (Constant 2)))
                         , Declaration (Decl "flag" (Just (Constant 0)))
                         , Statement
                           (Return
                             (Conditional
                               (Binary AST.GreaterThan (Reference "a") (Reference "b"))
                               (Constant 5)
                               (Conditional (Reference "flag") (Constant 6) (Constant 7))
                             )
                           )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for nested_ternary_2.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration
                           (Decl
                             "a"
                             (Just
                               (Conditional
                                 (Constant 1)
                                 (Conditional (Constant 2) (Constant 3) (Constant 4))
                                 (Constant 5)
                               )
                             )
                           )
                         , Declaration
                           (Decl
                             "b"
                             (Just
                               (Conditional
                                 (Constant 0)
                                 (Conditional (Constant 2) (Constant 3) (Constant 4))
                                 (Constant 5)
                               )
                             )
                           )
                         , Statement
                           (Return
                             (Binary AST.Multiplication (Reference "a") (Reference "b"))
                           )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for rh_assignment.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "flag" (Just (Constant 1)))
                         , Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (Expression
                             (Just
                               (Conditional (Reference "flag")
                                            (AST.Assignment "a" (Constant 1))
                                            (AST.Assignment "a" (Constant 0))
                               )
                             )
                           )
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for ternary.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (Return
                             (Conditional
                               (Binary AST.GreaterThan
                                       (Reference "a")
                                       (Unary AST.Negation (Constant 1))
                               )
                               (Constant 4)
                               (Constant 5)
                             )
                           )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for else.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (If (Reference "a")
                               (Return (Constant 1))
                               (Just (Return (Constant 2)))
                           )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for if_nested.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 1)))
                         , Declaration (Decl "b" (Just (Constant 0)))
                         , Statement
                           (If
                             (Reference "a")
                             (Expression (Just (AST.Assignment "b" (Constant 1))))
                             (Just
                               (If
                                 (Reference "b")
                                 (Expression (Just (AST.Assignment "b" (Constant 2))))
                                 Nothing
                               )
                             )
                           )
                         , Statement (Return (Reference "b"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for if_nested_2.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Declaration (Decl "b" (Just (Constant 1)))
                         , Statement
                           (If
                             (Reference "a")
                             (Expression (Just (AST.Assignment "b" (Constant 1))))
                             (Just
                               (If
                                 (Reference "b")
                                 (Expression (Just (AST.Assignment "b" (Constant 2))))
                                 Nothing
                               )
                             )
                           )
                         , Statement (Return (Reference "b"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for if_nested_3.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (If
                             (Constant 1)
                             (If
                               (Constant 2)
                               (Expression (Just (AST.Assignment "a" (Constant 3))))
                               (Just
                                 (Expression (Just (AST.Assignment "a" (Constant 4))))
                               )
                             )
                             Nothing
                           )
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for if_nested_4.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (If
                             (Constant 1)
                             (If
                               (Constant 0)
                               (Expression (Just (AST.Assignment "a" (Constant 3))))
                               (Just
                                 (Expression (Just (AST.Assignment "a" (Constant 4))))
                               )
                             )
                             Nothing
                           )
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for if_nested_5.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (If
                             (Constant 0)
                             (If
                               (Constant 0)
                               (Expression (Just (AST.Assignment "a" (Constant 3))))
                               (Just
                                 (Expression (Just (AST.Assignment "a" (Constant 4))))
                               )
                             )
                             (Just (Expression (Just (AST.Assignment "a" (Constant 1)))))
                           )
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for if_not_taken.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Declaration (Decl "b" (Just (Constant 0)))
                         , Statement
                           (If (Reference "a")
                               (Expression (Just (AST.Assignment "b" (Constant 1))))
                               Nothing
                           )
                         , Statement (Return (Reference "b"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for if_taken.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 1)))
                         , Declaration (Decl "b" (Just (Constant 0)))
                         , Statement
                           (If (Reference "a")
                               (Expression (Just (AST.Assignment "b" (Constant 1))))
                               Nothing
                           )
                         , Statement (Return (Reference "b"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

  describe "Stage 7" $ do

    it "should generate code for consecutive_blocks.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 1)))
                         , Statement
                           (Compound [Declaration (Decl "a" (Just (Constant 2)))])
                         , Statement (Compound [Statement (Return (Reference "a"))])
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for consecutive_declarations.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 0)))
                         , Statement
                           (Compound
                             [ Declaration (Decl "b" (Just (Constant 1)))
                             , Statement
                               (Expression (Just (AST.Assignment "a" (Reference "b"))))
                             ]
                           )
                         , Statement
                           (Compound
                             [ Declaration (Decl "b" (Just (Constant 2)))
                             , Statement
                               (Expression
                                 (Just
                                   (AST.Assignment
                                     "a"
                                     (Binary AST.Addition (Reference "a") (Reference "b")
                                     )
                                   )
                                 )
                               )
                             ]
                           )
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for declare_after_block.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "i" (Just (Constant 0)))
                         , Statement
                           (Compound [Declaration (Decl "a" (Just (Constant 2)))])
                         , Declaration (Decl "b" (Just (Constant 3)))
                         , Statement (Return (Reference "b"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for declare_block.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Statement
                             (If
                               (Constant 5)
                               (Compound
                                 [ Declaration (Decl "i" (Just (Constant 0)))
                                 , Statement (Return (Reference "i"))
                                 ]
                               )
                               Nothing
                             )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for declare_late.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 2)))
                         , Statement
                           (Compound
                             [ Statement
                               (Expression (Just (AST.Assignment "a" (Constant 3))))
                             , Declaration (Decl "a" (Just (Constant 0)))
                             ]
                           )
                         , Statement (Return (Reference "a"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

    it "should generate code for multi_nesting.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "a" (Just (Constant 2)))
                         , Statement
                           (If
                             (Binary AST.LessThan (Reference "a") (Constant 3))
                             (Compound
                               [ Statement
                                 (Compound
                                   [ Declaration (Decl "a" (Just (Constant 3)))
                                   , Statement (Return (Reference "a"))
                                   ]
                                 )
                               , Statement (Return (Reference "a"))
                               ]
                             )
                             Nothing
                           )
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
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
                   , "ret"
                   ]

  describe "Stage 8" $ do

    it "should generate code for break.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "sum" (Just (Constant 0)))
                         , Statement
                           (ForDecl
                             (Decl "i" (Just (Constant 0)))
                             (Binary AST.LessThan (Reference "i") (Constant 10))
                             (Just
                               (AST.Assignment
                                 "i"
                                 (Binary AST.Addition (Reference "i") (Constant 1))
                               )
                             )
                             (Compound
                               [ Statement
                                 (Expression
                                   (Just
                                     (AST.Assignment
                                       "sum"
                                       (Binary AST.Addition
                                               (Reference "sum")
                                               (Reference "i")
                                       )
                                     )
                                   )
                                 )
                               , Statement
                                 (If
                                   (Binary AST.GreaterThan
                                           (Reference "sum")
                                           (Constant 10)
                                   )
                                   Break
                                   Nothing
                                 )
                               ]
                             )
                           )
                         , Statement (Return (Reference "sum"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
                   , "main:"
                   , "push %ebp"
                   , "movl %esp, %ebp"
                   , "movl $0, %eax"
                   , "push %eax"
                   , "movl $0, %eax"
                   , "push %eax"
                   , "_main__for_begin__0:"
                   , "movl $10, %eax"
                   , "push %eax"
                   , "movl -8(%ebp), %eax"
                   , "pop %ecx"
                   , "cmpl %ecx, %eax"
                   , "movl $0, %eax"
                   , "setl %al"
                   , "cmpl $0, %eax"
                   , "je _main__for_end__1"
                   , "movl -8(%ebp), %eax"
                   , "push %eax"
                   , "movl -4(%ebp), %eax"
                   , "pop %ecx"
                   , "addl %ecx, %eax"
                   , "movl %eax, -4(%ebp)"
                   , "movl $10, %eax"
                   , "push %eax"
                   , "movl -4(%ebp), %eax"
                   , "pop %ecx"
                   , "cmpl %ecx, %eax"
                   , "movl $0, %eax"
                   , "setg %al"
                   , "cmpl $0, %eax"
                   , "je _main__else__3"
                   , "jmp _main__for_end__1"
                   , "jmp _main__endif__4"
                   , "_main__else__3:"
                   , "_main__endif__4:"
                   , "addl $0, %esp"
                   , "_main__for_post__2:"
                   , "movl $1, %eax"
                   , "push %eax"
                   , "movl -8(%ebp), %eax"
                   , "pop %ecx"
                   , "addl %ecx, %eax"
                   , "movl %eax, -8(%ebp)"
                   , "jmp _main__for_begin__0"
                   , "_main__for_end__1:"
                   , "addl $4, %esp"
                   , "movl -4(%ebp), %eax"
                   , "addl $4, %esp"
                   , "jmp _main__end"
                   , "addl $4, %esp"
                   , "_main__end:"
                   , "movl %ebp, %esp"
                   , "pop %ebp"
                   , "ret"
                   ]

    it "should generate code for continue.c"
      $          testGenerate
                   (Program
                     (Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "sum" (Just (Constant 0)))
                         , Statement
                           (ForDecl
                             (Decl "i" (Just (Constant 0)))
                             (Binary AST.LessThan (Reference "i") (Constant 10))
                             (Just
                               (AST.Assignment
                                 "i"
                                 (Binary AST.Addition (Reference "i") (Constant 1))
                               )
                             )
                             (Compound
                               [ Statement
                                 (If (Binary Modulo (Reference "sum") (Constant 2))
                                     Continue
                                     Nothing
                                 )
                               , Statement
                                 (Expression
                                   (Just
                                     (AST.Assignment
                                       "sum"
                                       (Binary AST.Addition
                                               (Reference "sum")
                                               (Reference "i")
                                       )
                                     )
                                   )
                                 )
                               ]
                             )
                           )
                         , Statement (Return (Reference "sum"))
                         ]
                       )
                     )
                   )
      `shouldBe` Right
                   [ ".globl main"
                   , "main:"
                   , "push %ebp"
                   , "movl %esp, %ebp"
                   , "movl $0, %eax"
                   , "push %eax"
                   , "movl $0, %eax"
                   , "push %eax"
                   , "_main__for_begin__0:"
                   , "movl $10, %eax"
                   , "push %eax"
                   , "movl -8(%ebp), %eax"
                   , "pop %ecx"
                   , "cmpl %ecx, %eax"
                   , "movl $0, %eax"
                   , "setl %al"
                   , "cmpl $0, %eax"
                   , "je _main__for_end__1"
                   , "movl $2, %eax"
                   , "push %eax"
                   , "movl -4(%ebp), %eax"
                   , "pop %ecx"
                   , "movl $0, %edx"
                   , "idivl %ecx"
                   , "movl %edx, %eax"
                   , "cmpl $0, %eax"
                   , "je _main__else__3"
                   , "jmp _main__for_post__2"
                   , "jmp _main__endif__4"
                   , "_main__else__3:"
                   , "_main__endif__4:"
                   , "movl -8(%ebp), %eax"
                   , "push %eax"
                   , "movl -4(%ebp), %eax"
                   , "pop %ecx"
                   , "addl %ecx, %eax"
                   , "movl %eax, -4(%ebp)"
                   , "addl $0, %esp"
                   , "_main__for_post__2:"
                   , "movl $1, %eax"
                   , "push %eax"
                   , "movl -8(%ebp), %eax"
                   , "pop %ecx"
                   , "addl %ecx, %eax"
                   , "movl %eax, -8(%ebp)"
                   , "jmp _main__for_begin__0"
                   , "_main__for_end__1:"
                   , "addl $4, %esp"
                   , "movl -4(%ebp), %eax"
                   , "addl $4, %esp"
                   , "jmp _main__end"
                   , "addl $4, %esp"
                   , "_main__end:"
                   , "movl %ebp, %esp"
                   , "pop %ebp"
                   , "ret"
                   ]
