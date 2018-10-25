module Codegen (generate) where

import           AST
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map as Map
import           Error
import           Text.Printf


data Context = Context { stackIndex :: Int
                       , varOffsets :: Map.Map String Int} deriving Show

type M = WriterT [String] (Except Error)
type MS = StateT Context M

generate :: Program -> Either Error [String]
generate prog = lines
  where lines =  runExcept . execWriterT $ program prog

program :: Program -> M ()
program (Program func) = function func

function :: Function -> M ()
function (Function name body) =
  let emptyContext = Context { stackIndex = -4
                             , varOffsets = Map.empty}
  in do
    emit $ ".globl " ++ name
    emit $ name ++ ":"
    emit "push %ebp"
    emit "movl %esp, %ebp"
    execStateT (mapM statement body) emptyContext
    emit "movl %ebp, %esp"
    emit "pop %ebp"
    emit "ret"

statement :: Statement -> MS ()
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
        emitL "push %eax"
        sind <- gets stackIndex
        let newStackIndex = sind - 4
            newVarOffsets = Map.insert name sind vars
          in put Context { stackIndex = newStackIndex
                         , varOffsets = newVarOffsets}


expression :: Expression -> MS ()
expression expr = case expr of

  Constant i -> emitL $ "movl $" ++ show i ++ ", %eax"

  Assignment name expr -> do
    expression expr
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emitL $ "movl %eax, " ++ show offset ++ "(%ebp)"
      Nothing -> throwError
               . CodegenError
               $ printf "Assignment to undeclared variable, `%s`." name

  Reference name -> do
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emitL $ "movl " ++ show offset ++ "(%ebp), %eax"
      Nothing -> throwError
               . CodegenError
               $ printf "Reference to undeclared variable, `%s`." name

  Unary op expr -> do
    expression expr
    case op of Negation -> emitL "neg %eax"
               BitwiseComplement -> emitL "not %eax"
               LogicalNegation -> do
                 emitL "cmpl $0, %eax"
                 emitL "movl $0, %eax"
                 emitL "sete %al"

  -- | General strategy for evaluating binary operators on
  --   sub-expressions e1, e2:
  --   1. Evaluate e1, push result onto the stack
  --   2. Evaluate e2
  --   3. Pop the result of e1 back into a register
  --   4. Perform the binary operation on e1, e2
  Binary op e1 e2 -> do
    expression e2
    emitL "push %eax"
    expression e1
    emitL "pop %ecx"
    case op of
      Addition ->
        emitL "addl %ecx, %eax"
      Subtraction ->
        emitL "subl %ecx, %eax"
      Multiplication ->
        emitL "imul %ecx, %eax"
      Division -> do
        emitL "movl $0, %edx"
        emitL "idivl %ecx"
      Equality -> do
        emitL "cmpl %ecx, %eax"
        emitL "movl $0, %eax"
        emitL "sete %al"
      Inequality -> do
        emitL "cmpl %ecx, %eax"
        emitL "movl $0, %eax"
        emitL "setne %al"
      LessThan -> do
        emitL "cmpl %ecx, %eax"
        emitL "movl $0, %eax"
        emitL "setl %al"
      GreaterThan -> do
        emitL "cmpl %ecx, %eax"
        emitL "movl $0, %eax"
        emitL "setg %al"
      LessEqual -> do
        emitL "cmpl %ecx, %eax"
        emitL "movl $0, %eax"
        emitL "setle %al"
      GreaterEqual -> do
        emitL "cmpl %ecx, %eax"
        emitL "movl $0, %eax"
        emitL "setge %al"
      LogicalOr -> do
        emitL "orl %ecx, %eax"
        emitL "movl $0, %eax"
        emitL "setne %al"
      LogicalAnd -> do
        emitL "cmpl $0, %ecx"
        emitL "setne %cl"
        emitL "cmpl $0, %eax"
        emitL "movl $0, %eax"
        emitL "setne %al"
        emitL "andb %cl, %al"

emit :: String -> M ()
emit s = writer ((), [s])

emitL :: String -> MS ()
emitL = lift . emit
