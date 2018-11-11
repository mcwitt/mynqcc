{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Codegen (generate) where

import           AST
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as Map
import           Error

type MError  = MonadError Error
type MWriter = MonadWriter [String]
type MState  = MonadState Context

data Context =
  Context { funcName   :: String
          , stackIndex :: Int
          , varOffsets :: Map.Map String Int
          , labelCount :: Int}
  deriving Show

generate :: Program -> Either Error [String]
generate = runExcept . execWriterT . program

program :: (MWriter m, MError m) => Program -> m ()
program (Program func) = function func

function :: (MWriter m, MError m) => Function -> m ()
function (Function name body) = do
  emit $ ".globl " ++ name
  emit $ name ++ ":"
  emit "push %ebp"
  emit "movl %esp, %ebp"
  let inner = case Function name body of
        -- Handle special case of empty main function
        Function "main" [] -> statement . Return . Constant $ 0
        Function _ body -> mapM_ blockItem body
      empty = Context { funcName = name
                      , stackIndex = -4
                      , varOffsets = Map.empty
                      , labelCount = 0}
  execStateT inner empty
  emit "movl %ebp, %esp"
  emit "pop %ebp"
  emit "ret"

blockItem :: (MState m, MWriter m, MError m) => BlockItem -> m ()
blockItem item = case item of

  Statement stat -> statement stat

  Declaration name maybeExpr -> do
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just _ -> throwError . CodegenError $
        "Variable `" ++ name ++ "` was declared more than once."
      Nothing -> return ()
    case maybeExpr of
      Just expr -> expression expr
      Nothing -> return ()
    emit "push %eax"
    si <- gets stackIndex
    modify $ \ctx -> ctx { stackIndex = stackIndex ctx - 4
                         , varOffsets = Map.insert name si vars}


statement :: (MState m, MWriter m, MError m) => Statement -> m ()
statement st = case st of

  Return expr -> expression expr
  Expression expr -> expression expr

  If expr s1 maybeStat -> do
    expression expr
    emit "cmpl $0, %eax"
    labelElse <- label "else"
    emit $ "je " ++ labelElse
    statement s1
    labelEndIf <- label "endif"
    emit $ "jmp " ++ labelEndIf
    emit $ labelElse ++ ":"
    case maybeStat of
      Just s2 -> statement s2
      Nothing -> return ()
    emit $ labelEndIf ++ ":"


expression :: (MState m, MWriter m, MError m) => Expression -> m ()
expression expr = case expr of

  Constant i -> emit $ "movl $" ++ show i ++ ", %eax"

  Assignment name expr -> do
    expression expr
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emit $ "movl %eax, " ++ show offset ++ "(%ebp)"
      Nothing -> throwError . CodegenError $
                 "Assignment to undeclared variable, `" ++ name ++ "`."

  Reference name -> do
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emit $ "movl " ++ show offset ++ "(%ebp), %eax"
      Nothing -> throwError . CodegenError $
                 "Reference to undeclared variable, `" ++ name ++ "`."

  Unary op expr -> do
    expression expr
    case op of Negation -> emit "neg %eax"
               BitwiseComplement -> emit "not %eax"
               LogicalNegation -> do
                 emit "cmpl $0, %eax"
                 emit "movl $0, %eax"
                 emit "sete %al"

  {- General strategy for evaluating binary operators on sub-expressions e1, e2:
     1. Evaluate e1, push result onto the stack
     2. Evaluate e2
     3. Pop the result of e1 back into a register
     4. Perform the binary operation on e1, e2 -}
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

  Conditional e1 e2 e3 -> do
    expression e1
    emit "cmpl $0, %eax"
    labelE3 <- label "e3"
    emit $ "je " ++ labelE3
    expression e2
    labelPostCond <- label "post_conditional"
    emit $ "jmp " ++ labelPostCond
    emit $ labelE3 ++ ":"
    expression e3
    emit $ labelPostCond ++ ":"

label :: MState m => String -> m String
label s = do
  prefix <- gets funcName
  lc <- gets labelCount
  modify $ \ctx -> ctx { labelCount = succ lc }
  return $ "_" ++ prefix ++ "__" ++ s ++ "__" ++ show lc

emit :: MWriter m => String -> m ()
emit = tell . pure
