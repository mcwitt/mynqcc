module Codegen (generate) where

import AST

class Generable a where
  generate :: a -> String

instance Generable Program where
  generate (Program func) = generate func

instance Generable Function where
  generate (Function name body)
    = ".globl " ++ name ++ "\n" ++
      name ++ ":\n" ++
      generate (head body)

instance Generable Statement where
  generate (Return expr) = generate expr ++ "ret"


instance Generable Expression where

  generate (Constant i) = "movl $" ++ show i ++ ", %eax\n"

  generate (Unary op expr)
    = generate expr
   ++ (case op of Negation -> "neg %eax"
                  BitwiseComplement -> "not %eax"
                  LogicalNegation ->
                    "cmpl $0, %eax\n\
                    \movl $0, %eax\n\
                    \sete %al")
   ++ "\n"

  -- | General strategy for evaluating binary operators on
  --   sub-expressions e1, e2:
  --   1. Evaluate e1, push result onto the stack
  --   2. Evaluate e2
  --   3. Pop the result of e1 back into a register
  --   4. Perform the binary operation on e1, e2
  generate (Binary op e1 e2)
    = generate e2
   ++ "push %eax\n"
   ++ generate e1
   ++ "pop %ecx\n"
   ++ (case op of Addition ->
                    "addl %ecx, %eax"
                  Subtraction ->
                    "subl %ecx, %eax"
                  Multiplication ->
                    "imul %ecx, %eax"
                  Division ->
                    "movl $0, %edx\n\
                    \idivl %ecx"
                  Equality ->
                    "cmpl %ecx, %eax\n\
                    \movl $0, %eax\n\
                    \sete %al"
                  Inequality ->
                    "cmpl %ecx, %eax\n\
                    \movl $0, %eax\n\
                    \setne %al"
                  LessThan ->
                    "cmpl %ecx, %eax\n\
                    \movl $0, %eax\n\
                    \setl %al"
                  GreaterThan ->
                    "cmpl %ecx, %eax\n\
                    \movl $0, %eax\n\
                    \setg %al"
                  LessEqual ->
                    "cmpl %ecx, %eax\n\
                    \movl $0, %eax\n\
                    \setle %al"
                  GreaterEqual ->
                    "cmpl %ecx, %eax\n\
                    \movl $0, %eax\n\
                    \setge %al"
                  LogicalOr ->
                    "orl %ecx, %eax\n\
                    \movl $0, %eax\n\
                    \setne %al"
                  LogicalAnd ->
                    "cmpl $0, %ecx\n\
                    \setne %cl\n\
                    \cmpl $0, %eax\n\
                    \movl $0, %eax\n\
                    \setne %al\n\
                    \andb %cl, %al")
   ++ "\n"
