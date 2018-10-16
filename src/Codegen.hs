module Codegen (generate) where

import AST ( Program    (Program)
           , Function   (Function)
           , Statement  (Return)
           , Expression (Constant))

statement :: Statement -> String
statement (Return (Constant ival)) =
  "movl $" ++ show ival ++ ", %eax\nret"

function :: Function -> String
function (Function name body) =
  ".globl _" ++ name ++ "\n" ++
  "_" ++ name ++ ":" ++ "\n" ++
  (statement body)


-- | Generate assembly code from an AST.
generate :: Program -> String
generate (Program func) = function func
