module Codegen (generate) where

import AST
import Text.Printf

expression :: Expression -> String
expression expr = case expr of
  Constant ival          -> printf "movl $%d, %%eax\n" ival
  Negation expr          -> expression expr ++ "neg %eax\n"
  BitwiseComplement expr -> expression expr ++ "not %eax\n"
  LogicalNegation expr   -> expression expr ++
                            "cmpl $0, %eax\n\
                            \movl $0, %eax\n\
                            \sete %al\n"

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
