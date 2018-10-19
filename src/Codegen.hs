module Codegen (generate) where

import AST

class Generable a where
  generate :: a -> String

instance Generable Program where
  generate (Program func) = generate func

instance Generable Function where
  generate (Function name stat)
    = ".globl " ++ name ++ "\n" ++
      name ++ ":\n" ++
      generate stat

instance Generable Statement where
  generate (Return expr) = generate expr ++ "ret"


instance Generable Expression where

  generate (Constant i) = "movl $" ++ show i ++ ", %eax\n"

  generate (Unary op expr)
    = generate expr
   ++ (case op of Negation -> "neg %eax"
                  BitwiseComplement -> "not %eax"
                  LogicalNegation -> "cmpl $0, %eax\n\
                                     \xorl %eax, %eax\n\
                                     \sete %al")
   ++ "\n"

  generate (Binary op e1 e2)
    = generate e2
   ++ "push %eax\n"
   ++ generate e1
   ++ "pop %ecx\n"
   ++ (case op of Addition -> "addl %ecx, %eax"
                  Subtraction -> "subl %ecx, %eax"
                  Multiplication -> "imul %ecx, %eax"
                  Division -> "xorl %edx, %edx\n\
                              \idivl %ecx")
   ++ "\n"
