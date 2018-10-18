module Codegen (generate) where

import AST
import Text.Printf

factor :: Factor -> String
factor fact = case fact of
  Constant ival           -> printf "movl $%d, %%eax\n" ival
  Negation fact'          -> factor fact' ++ "neg %eax\n"
  BitwiseComplement fact' -> factor fact' ++ "not %eax\n"
  LogicalNegation fact'   -> factor fact' ++
                             "cmpl $0, %eax\n\
                             \movl $0, %eax\n\
                             \sete %al\n"

term :: Term -> String
term t = case t of
  Term fact -> factor fact

expression :: Expression -> String
expression expr = case expr of
  Expression t -> term t

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
