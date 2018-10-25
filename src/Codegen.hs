module Codegen (generate) where

import AST
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Error
import Text.Printf


data Context = Context { stackIndex :: Int
                       , varOffsets :: Map.Map String Int} deriving Show

type M = StateT Context (WriterT [String] (Except Error))

generate :: Program -> Either Error [String]
generate prog = lines
  where lines =  runExcept . execWriterT $ runStateT (program prog) emptyContext
        emptyContext = Context { stackIndex = -4
                               , varOffsets = Map.empty}

program :: Program -> M ()
program (Program func) = function func

function :: Function -> M ()
function (Function name body) = do
  emit $ ".globl " ++ name
  emit $ name ++ ":"
  emit "push %ebp"
  emit "movl %esp, %ebp"
  mapM statement body
  emit "movl %ebp, %esp"
  emit "pop %ebp"
  emit "ret"

statement :: Statement -> M ()
statement st = case st of

  Return expr -> expression expr

  Expression expr -> expression expr

  Declaration name maybeExpr -> do
    vars <- gets varOffsets
    if Map.member name vars
      then throwError
         . CodegenError
         $ printf "Variable `%s` was declared more than once." name
      else do
        case maybeExpr of
          Just expr -> expression expr
          Nothing -> return ()
        emit "push %eax"
        sind <- gets stackIndex
        let newStackIndex = sind - 4
            newVarOffsets = Map.insert name sind vars
          in put Context { stackIndex = newStackIndex
                         , varOffsets = newVarOffsets}


expression :: Expression -> M ()
expression expr = case expr of

  Constant i -> emit $ "movl $" ++ show i ++ ", %eax"

  Assignment name expr -> do
    expression expr
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emit $ "movl %eax, " ++ show offset ++ "(%ebp)"
      Nothing -> throwError
               . CodegenError
               $ printf "Assignment to undeclared variable, `%s`." name

  Reference name -> do
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emit $ "movl " ++ show offset ++ "(%ebp), %eax"
      Nothing -> throwError
               . CodegenError
               $ printf "Reference to undeclared variable, `%s`." name

  Unary op expr -> do
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
  Binary op e1 e2 -> do
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

emit :: String -> M ()
emit s = lift $ writer ((), [s])
