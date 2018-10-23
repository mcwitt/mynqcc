module Codegen (generate) where

import AST
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map, empty)


data Context = Context { stackIndex :: Int
                       , varOffsets :: Map String Int} deriving Show

type Generator = StateT Context (Writer [String])


generate :: Program -> [String]
generate prog = lines
  where lines = execWriter $ runStateT (program prog) emptyContext
        emptyContext = Context { stackIndex = 0
                               , varOffsets = empty}

program :: Program -> Generator ()
program (Program func) = function func

function :: Function -> Generator ()
function (Function name body) = do
  emit $ ".globl " ++ name
  emit $ name ++ ":"
  emit "push %ebp"
  emit "movl %esp, %ebp"
  mapM statement body
  emit "movl %ebp, %esp"
  emit "pop %ebp"
  emit "ret"

statement :: Statement -> Generator ()
statement (Return expr) = expression expr

expression :: Expression -> Generator ()
expression (Constant i) = emit $ "movl $" ++ show i ++ ", %eax"
expression (Unary op expr) = do
  expression expr
  case op of Negation -> emit "neg %eax"
             BitwiseComplement -> emit "not %eax"
             LogicalNegation -> do
               emit "cmpl $0, %eax"
               emit "movl $0, %eax"
               emit "sete %al"

  -- | General strategy for evaluating binary operators on
  --   sub-expressions e1, e2:
  --   1. Evaluate e1, push result onto the stack
  --   2. Evaluate e2
  --   3. Pop the result of e1 back into a register
  --   4. Perform the binary operation on e1, e2
expression (Binary op e1 e2) = do
  expression e2
  emit "push %eax"
  expression e1
  emit "pop %ecx"
  case op of
    Addition ->
      emit "addl %ecx, %eax"
    Subtraction ->
      emit "subl %ecx, %eax"
    Multiplication ->
      emit "imul %ecx, %eax"
    Division -> do
      emit "movl $0, %edx"
      emit "idivl %ecx"
    Equality -> do
      emit "cmpl %ecx, %eax"
      emit "movl $0, %eax"
      emit "sete %al"
    Inequality -> do
      emit "cmpl %ecx, %eax"
      emit "movl $0, %eax"
      emit "setne %al"
    LessThan -> do
      emit "cmpl %ecx, %eax"
      emit "movl $0, %eax"
      emit "setl %al"
    GreaterThan -> do
      emit "cmpl %ecx, %eax"
      emit "movl $0, %eax"
      emit "setg %al"
    LessEqual -> do
      emit "cmpl %ecx, %eax"
      emit "movl $0, %eax"
      emit "setle %al"
    GreaterEqual -> do
      emit "cmpl %ecx, %eax"
      emit "movl $0, %eax"
      emit "setge %al"
    LogicalOr -> do
      emit "orl %ecx, %eax"
      emit "movl $0, %eax"
      emit "setne %al"
    LogicalAnd -> do
      emit "cmpl $0, %ecx"
      emit "setne %cl"
      emit "cmpl $0, %eax"
      emit "movl $0, %eax"
      emit "setne %al"
      emit "andb %cl, %al"

emit :: String -> Generator ()
emit s = lift $ writer ((), [s])
