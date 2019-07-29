module CodegenSpec
  ( spec
  )
where

import           AST
import           Codegen
import           Parser
import           Target
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
                 (Program [Function "main" [] (Just [Statement (return_ (constant 100))])]
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
                   [ Function
                       "main"
                       []
                       (Just
                         [Statement (return_ (unary AST.LogicalNegation (constant 12)))]
                       )
                   ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_ (binary AST.Addition (constant 1) (constant 2)))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary
                                   AST.Subtraction
                                   (binary AST.Subtraction (constant 1) (constant 2))
                                   (constant 3)
                                 )
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary
                                   AST.Division
                                   (binary AST.Division (constant 6) (constant 3))
                                   (constant 2)
                                 )
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_ (binary AST.Division (constant 4) (constant 2)))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary AST.Multiplication (constant 2) (constant 3))
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary
                                   AST.Multiplication
                                   (constant 2)
                                   (binary AST.Addition (constant 3) (constant 4))
                                 )
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_ (binary AST.LogicalAnd (constant 1) (constant 0)))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary AST.LogicalAnd
                                         (constant 1)
                                         (unary AST.Negation (constant 1))
                                 )
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_ (binary AST.Equality (constant 1) (constant 2)))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_ (binary AST.Equality (constant 1) (constant 1)))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary AST.GreaterEqual (constant 1) (constant 2))
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary AST.GreaterEqual (constant 1) (constant 1))
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (return_
                                 (binary
                                   AST.LogicalOr
                                   (constant 1)
                                   (binary AST.LogicalAnd (constant 0) (constant 2))
                                 )
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Statement
                             (expression (Just (AST.assignment "a" (constant 2))))
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Declaration
                             (Decl "b" (Just (AST.assignment "a" (constant 0))))
                           , Statement (return_ (reference "b"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Declaration (Decl "b" Nothing)
                           , Statement
                             (expression
                               (Just
                                 (AST.assignment "a" (AST.assignment "b" (constant 4)))
                               )
                             )
                           , Statement
                             (return_
                               (binary AST.Subtraction (reference "a") (reference "b"))
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement (return_ (constant 0))
                           ]
                         )
                     ]
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
      $          testGenerate (Program [Function "main" [] (Just [])])
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 2)))
                           , Statement
                             (return_
                               (binary AST.Addition (reference "a") (reference "b"))
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Statement (return_ (constant 0))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                             (expression
                               (Just (binary AST.Addition (constant 2) (constant 2)))
                             )
                           , Statement (return_ (constant 0))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "a" (Just (constant 2)))
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
                   )
      `shouldBe` Left
                   (CodegenError
                     "Multiple declarations of `a` in the same block."
                   )

    it "should fail to generate code for undeclared_var.c"
      $          testGenerate
                   (Program
                     [Function "main" [] (Just [Statement (return_ (reference "a"))])]
                   )
      `shouldBe` Left (CodegenError "Reference to undeclared variable, `a`.")

    it "should fail to generate code for var_declared_late.c"
      $          testGenerate
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                             (expression
                               (Just
                                 (AST.assignment
                                   "a"
                                   (binary AST.Addition (constant 1) (constant 2))
                                 )
                               )
                             )
                           , Declaration (Decl "a" Nothing)
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
                   )
      `shouldBe` Left (CodegenError "Assignment to undeclared variable, `a`.")

  describe "Stage 6" $ do
    it "should generate code for assign_ternary.c"
      $          testGenerate
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (expression
                               (Just
                                 (AST.assignment
                                   "a"
                                   (conditional (constant 1) (constant 2) (constant 3))
                                 )
                               )
                             )
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration
                             (Decl
                               "a"
                               (Just
                                 (conditional
                                   (binary AST.GreaterThan (constant 1) (constant 2))
                                   (constant 3)
                                   (constant 4)
                                 )
                               )
                             )
                           , Declaration
                             (Decl
                               "b"
                               (Just
                                 (conditional
                                   (binary AST.GreaterThan (constant 1) (constant 2))
                                   (constant 5)
                                   (constant 6)
                                 )
                               )
                             )
                           , Statement
                             (return_
                               (binary AST.Addition (reference "a") (reference "b"))
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 2)))
                           , Declaration (Decl "flag" (Just (constant 0)))
                           , Statement
                             (return_
                               (conditional
                                 (binary AST.GreaterThan (reference "a") (reference "b"))
                                 (constant 5)
                                 (conditional (reference "flag")
                                              (constant 6)
                                              (constant 7)
                                 )
                               )
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration
                             (Decl
                               "a"
                               (Just
                                 (conditional
                                   (constant 1)
                                   (conditional (constant 2) (constant 3) (constant 4))
                                   (constant 5)
                                 )
                               )
                             )
                           , Declaration
                             (Decl
                               "b"
                               (Just
                                 (conditional
                                   (constant 0)
                                   (conditional (constant 2) (constant 3) (constant 4))
                                   (constant 5)
                                 )
                               )
                             )
                           , Statement
                             (return_
                               (binary AST.Multiplication (reference "a") (reference "b")
                               )
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "flag" (Just (constant 1)))
                           , Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (expression
                               (Just
                                 (conditional (reference "flag")
                                              (AST.assignment "a" (constant 1))
                                              (AST.assignment "a" (constant 0))
                                 )
                               )
                             )
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (return_
                               (conditional
                                 (binary AST.GreaterThan
                                         (reference "a")
                                         (unary AST.Negation (constant 1))
                                 )
                                 (constant 4)
                                 (constant 5)
                               )
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_ (reference "a")
                                 (return_ (constant 1))
                                 (Just (return_ (constant 2)))
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 0)))
                           , Statement
                             (if_
                               (reference "a")
                               (expression (Just (AST.assignment "b" (constant 1))))
                               (Just
                                 (if_
                                   (reference "b")
                                   (expression (Just (AST.assignment "b" (constant 2))))
                                   Nothing
                                 )
                               )
                             )
                           , Statement (return_ (reference "b"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Declaration (Decl "b" (Just (constant 1)))
                           , Statement
                             (if_
                               (reference "a")
                               (expression (Just (AST.assignment "b" (constant 1))))
                               (Just
                                 (if_
                                   (reference "b")
                                   (expression (Just (AST.assignment "b" (constant 2))))
                                   Nothing
                                 )
                               )
                             )
                           , Statement (return_ (reference "b"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_
                               (constant 1)
                               (if_
                                 (constant 2)
                                 (expression (Just (AST.assignment "a" (constant 3))))
                                 (Just
                                   (expression (Just (AST.assignment "a" (constant 4))))
                                 )
                               )
                               Nothing
                             )
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_
                               (constant 1)
                               (if_
                                 (constant 0)
                                 (expression (Just (AST.assignment "a" (constant 3))))
                                 (Just
                                   (expression (Just (AST.assignment "a" (constant 4))))
                                 )
                               )
                               Nothing
                             )
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_
                               (constant 0)
                               (if_
                                 (constant 0)
                                 (expression (Just (AST.assignment "a" (constant 3))))
                                 (Just
                                   (expression (Just (AST.assignment "a" (constant 4))))
                                 )
                               )
                               (Just
                                 (expression (Just (AST.assignment "a" (constant 1))))
                               )
                             )
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Declaration (Decl "b" (Just (constant 0)))
                           , Statement
                             (if_ (reference "a")
                                 (expression (Just (AST.assignment "b" (constant 1))))
                                 Nothing
                             )
                           , Statement (return_ (reference "b"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 0)))
                           , Statement
                             (if_ (reference "a")
                                 (expression (Just (AST.assignment "b" (constant 1))))
                                 Nothing
                             )
                           , Statement (return_ (reference "b"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Statement
                             (compound [Declaration (Decl "a" (Just (constant 2)))])
                           , Statement (compound [Statement (return_ (reference "a"))])
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (compound
                               [ Declaration (Decl "b" (Just (constant 1)))
                               , Statement
                                 (expression (Just (AST.assignment "a" (reference "b"))))
                               ]
                             )
                           , Statement
                             (compound
                               [ Declaration (Decl "b" (Just (constant 2)))
                               , Statement
                                 (expression
                                   (Just
                                     (AST.assignment
                                       "a"
                                       (binary AST.Addition
                                               (reference "a")
                                               (reference "b")
                                       )
                                     )
                                   )
                                 )
                               ]
                             )
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "i" (Just (constant 0)))
                           , Statement
                             (compound [Declaration (Decl "a" (Just (constant 2)))])
                           , Declaration (Decl "b" (Just (constant 3)))
                           , Statement (return_ (reference "b"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (if_
                                 (constant 5)
                                 (compound
                                   [ Declaration (Decl "i" (Just (constant 0)))
                                   , Statement (return_ (reference "i"))
                                   ]
                                 )
                                 Nothing
                               )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement
                             (compound
                               [ Statement
                                 (expression (Just (AST.assignment "a" (constant 3))))
                               , Declaration (Decl "a" (Just (constant 0)))
                               ]
                             )
                           , Statement (return_ (reference "a"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement
                             (if_
                               (binary AST.LessThan (reference "a") (constant 3))
                               (compound
                                 [ Statement
                                   (compound
                                     [ Declaration (Decl "a" (Just (constant 3)))
                                     , Statement (return_ (reference "a"))
                                     ]
                                   )
                                 , Statement (return_ (reference "a"))
                                 ]
                               )
                               Nothing
                             )
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "sum" (Just (constant 0)))
                           , Statement
                             (forDecl
                               (Decl "i" (Just (constant 0)))
                               (binary AST.LessThan (reference "i") (constant 10))
                               (Just
                                 (AST.assignment
                                   "i"
                                   (binary AST.Addition (reference "i") (constant 1))
                                 )
                               )
                               (compound
                                 [ Statement
                                   (expression
                                     (Just
                                       (AST.assignment
                                         "sum"
                                         (binary AST.Addition
                                                 (reference "sum")
                                                 (reference "i")
                                         )
                                       )
                                     )
                                   )
                                 , Statement
                                   (if_
                                     (binary AST.GreaterThan
                                             (reference "sum")
                                             (constant 10)
                                     )
                                     AST.break
                                     Nothing
                                   )
                                 ]
                               )
                             )
                           , Statement (return_ (reference "sum"))
                           ]
                         )
                     ]
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
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "sum" (Just (constant 0)))
                           , Statement
                             (forDecl
                               (Decl "i" (Just (constant 0)))
                               (binary AST.LessThan (reference "i") (constant 10))
                               (Just
                                 (AST.assignment
                                   "i"
                                   (binary AST.Addition (reference "i") (constant 1))
                                 )
                               )
                               (compound
                                 [ Statement
                                   (if_ (binary Modulo (reference "sum") (constant 2))
                                       continue
                                       Nothing
                                   )
                                 , Statement
                                   (expression
                                     (Just
                                       (AST.assignment
                                         "sum"
                                         (binary AST.Addition
                                                 (reference "sum")
                                                 (reference "i")
                                         )
                                       )
                                     )
                                   )
                                 ]
                               )
                             )
                           , Statement (return_ (reference "sum"))
                           ]
                         )
                     ]
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

  describe "Stage 9" $ do

    it "should generate code for expression_args.c" $ do
      testGenerate
          (Program
            [ Function
              "add"
              ["a", "b"]
              (Just
                [ Statement
                    (return_
                      (binary AST.Addition (reference "a") (reference "b"))
                    )
                ]
              )
            , Function
              "main"
              []
              (Just
                [ Declaration
                  (Decl
                    "sum"
                    (Just
                      (funCall
                        "add"
                        [ binary AST.Addition (constant 1) (constant 2)
                        , constant 4
                        ]
                      )
                    )
                  )
                , Statement
                  (return_
                    (binary AST.Addition (reference "sum") (reference "sum"))
                  )
                ]
              )
            ]
          )
        `shouldBe` Right
                     [ ".globl add"
                     , "add:"
                     , "push %ebp"
                     , "movl %esp, %ebp"
                     , "movl 12(%ebp), %eax"
                     , "push %eax"
                     , "movl 8(%ebp), %eax"
                     , "pop %ecx"
                     , "addl %ecx, %eax"
                     , "addl $0, %esp"
                     , "jmp _add__end"
                     , "addl $0, %esp"
                     , "_add__end:"
                     , "movl %ebp, %esp"
                     , "pop %ebp"
                     , "ret"
                     , ".globl main"
                     , "main:"
                     , "push %ebp"
                     , "movl %esp, %ebp"
                     , "movl $4, %eax"
                     , "push %eax"
                     , "movl $2, %eax"
                     , "push %eax"
                     , "movl $1, %eax"
                     , "pop %ecx"
                     , "addl %ecx, %eax"
                     , "push %eax"
                     , "call add"
                     , "add $0x8, %esp"
                     , "push %eax"
                     , "movl -4(%ebp), %eax"
                     , "push %eax"
                     , "movl -4(%ebp), %eax"
                     , "pop %ecx"
                     , "addl %ecx, %eax"
                     , "addl $4, %esp"
                     , "jmp _main__end"
                     , "addl $4, %esp"
                     , "_main__end:"
                     , "movl %ebp, %esp"
                     , "pop %ebp"
                     , "ret"
                     ]

    it "should generate code for forward_decl.c" $ do
      testGenerate
          (Program
            [ Function "foo" [] Nothing
            , Function "main"
                       []
                       (Just [(Statement (return_ (funCall "foo" [])))])
            , Function "foo" [] (Just [(Statement (return_ (constant 3)))])
            ]
          )
        `shouldBe` Right
                     [ ".globl main"
                     , "main:"
                     , "push %ebp"
                     , "movl %esp, %ebp"
                     , "call foo"
                     , "add $0x0, %esp"
                     , "addl $0, %esp"
                     , "jmp _main__end"
                     , "addl $0, %esp"
                     , "_main__end:"
                     , "movl %ebp, %esp"
                     , "pop %ebp"
                     , "ret"
                     , ".globl foo"
                     , "foo:"
                     , "push %ebp"
                     , "movl %esp, %ebp"
                     , "movl $3, %eax"
                     , "addl $0, %esp"
                     , "jmp _foo__end"
                     , "addl $0, %esp"
                     , "_foo__end:"
                     , "movl %ebp, %esp"
                     , "pop %ebp"
                     , "ret"
                     ]

