module Codegen (generate) where

import AST

factor :: Factor -> String
factor fact = case fact of
  Factor e -> expression e
  Constant i -> "movl $" ++ show i ++ ", %eax\n"
  Negation f -> factor f ++ "neg %eax\n"
  BitwiseComplement f -> factor f ++ "not %eax\n"
  LogicalNegation f -> factor f ++
                       "cmpl $0, %eax\n\
                       \movl $0, %eax\n\
                       \sete %al\n"

term :: Term -> String
term t = case t of
  Term fact -> factor fact

  Multiplication l r -> term r ++
                        "push %eax\n" ++
                        term l ++
                        "pop %ecx\n\
                        \imul %ecx, %eax\n"

  Division l r -> term r ++
                  "push %eax\n" ++
                  term l ++
                  "pop %ecx\n\
                  \movl $0, %edx\n\
                  \idivl %ecx\n"

expression :: Expression -> String
expression expr = case expr of
  Expression t -> term t

  Addition l r -> expression r ++
                  "push %eax\n" ++
                  expression l ++
                  "pop %ecx\n\
                  \addl %ecx, %eax\n"

  Subtraction l r -> expression r ++
                     "push %eax\n" ++
                     expression l ++
                     "pop %ecx\n\
                     \subl %ecx, %eax\n"

statement :: Statement -> String
statement (Return expr) = expression expr ++ "ret"

function :: Function -> String
function (Function name body) =
  ".globl " ++ name ++ "\n" ++
  name ++ ":" ++ "\n" ++
  (statement body)


-- | Generate assembly code from an AST.
generate :: Program -> String
generate (Program func) = function func
