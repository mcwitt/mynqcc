{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Codegen (generate) where

import           AST
import           Control.Monad        (when)
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Error

type MError  = MonadError Error
type MWriter = MonadWriter [String]
type MState  = MonadState Context

data Context =
  Context { funcName   :: String
          , labelCount :: Int
          , stackIndex :: Int
          , varMap     :: Map.Map String Int
          , localVars  :: Set.Set String
          , loopBegin  :: Maybe String
          , loopEnd    :: Maybe String}
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
  let inner = block $ f name body
      f "main" [] = [Statement . Return . Constant $ 0]
      f _ items = items
      empty = Context { funcName   = name
                      , labelCount = 0
                      , stackIndex = -4
                      , varMap     = Map.empty
                      , localVars  = Set.empty
                      , loopBegin  = Nothing
                      , loopEnd    = Nothing}
  execStateT inner empty
  emit $ "_" ++ name ++ "__end:"
  emit "movl %ebp, %esp"
  emit "pop %ebp"
  emit "ret"

block :: (MState m, MWriter m, MError m) => [BlockItem] -> m ()
block items = withNestedContext $ do
  modify $ \c -> c { localVars = Set.empty }
  mapM_ blockItem items
  vars <- gets localVars
  let bytes = 4 * Set.size vars
  emit $ "addl $" ++ show bytes ++ ", %esp"

blockItem :: (MState m, MWriter m, MError m) => BlockItem -> m ()
blockItem item = case item of
  Statement stat -> statement stat
  Declaration decl -> declaration decl

declaration :: (MState m, MWriter m, MError m) => Declaration -> m ()
declaration (Decl name maybeExpr) = do
  vars <- gets localVars
  when (Set.member name vars) $ throwError . CodegenError $
    "Multiple declarations of `" ++ name ++ "` in the same block."
  case maybeExpr of
    Just expr -> expression expr
    Nothing -> return ()
  emit "push %eax"
  sidx <- gets stackIndex
  vmap <- gets varMap
  modify $ \c -> c { stackIndex = stackIndex c - 4
                   , varMap = Map.insert name sidx vmap
                   , localVars = Set.insert name vars}

statement :: (MState m, MWriter m, MError m) => Statement -> m ()
statement st = case st of

  Return expr -> do
    expression expr
    name <- gets funcName
    sidx <- gets stackIndex
    let bytes = -(4 + sidx)
    emit $ "addl $" ++ show bytes ++ ", %esp"
    emit $ "jmp _" ++ name ++ "__end"

  Expression expr -> case expr of
    Just expr -> expression expr
    Nothing -> return ()

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

  Compound items -> block items

  While expr stat -> do
    labelBegin <- label "while_begin"
    emit $ labelBegin ++ ":"
    expression expr
    emit "cmpl $0, %eax"
    labelEnd <- label "while_end"
    emit $ "je " ++ labelEnd
    withNestedContext $ do
      modify $ \c -> c { loopBegin = Just labelBegin
                       , loopEnd   = Just labelEnd}
      statement stat
    emit $ "jmp " ++ labelBegin
    emit $ labelEnd ++ ":"

  Do stat expr -> do
    labelBegin <- label "do_begin"
    emit $ labelBegin ++ ":"
    labelEnd <- label "do_end"
    withNestedContext $ do
      modify $ \c -> c { loopBegin = Just labelBegin
                       , loopEnd   = Just labelEnd}
      statement stat
    expression expr
    emit "cmpl $0, %eax"
    emit $ "je " ++ labelEnd
    emit $ "jmp" ++ labelBegin
    emit $ labelEnd ++ ":"

  For maybeInit cond maybePost stat -> do
    case maybeInit of
      Just expr -> expression expr
      Nothing   -> return ()
    labelBegin <- label "for_begin"
    emit $ labelBegin ++ ":"
    expression cond
    emit "cmpl $0, %eax"
    labelEnd <- label "for_end"
    emit $ "je " ++ labelEnd
    withNestedContext $ do
      modify $ \c -> c { loopBegin = Just labelBegin
                       , loopEnd   = Just labelEnd}
      statement stat
    case maybePost of
      Just expr -> expression expr
      Nothing   -> return ()
    emit $ "jmp " ++ labelBegin
    emit $ labelEnd ++ ":"

  ForDecl decl cond maybePost stat -> withNestedContext $ do
    declaration decl
    labelBegin <- label "for_begin"
    emit $ labelBegin ++ ":"
    expression cond
    emit "cmpl $0, %eax"
    labelEnd <- label "for_end"
    emit $ "je " ++ labelEnd
    modify $ \c -> c { loopBegin = Just labelBegin
                     , loopEnd   = Just labelEnd}
    statement stat
    case maybePost of
      Just expr -> expression expr
      Nothing   -> return ()
    emit $ "jmp " ++ labelBegin
    emit $ labelEnd ++ ":"
    emit $ "addl $4, %esp"

  Break -> do
    maybeLabelEnd <- gets loopEnd
    case maybeLabelEnd of
      Just labelEnd -> emit $ "jmp " ++ labelEnd
      Nothing -> throwError $ CodegenError
        "Found `break` outside of a loop."

  Continue -> do
    maybeLabelBegin <- gets loopBegin
    case maybeLabelBegin of
      Just labelBegin -> emit $ "jmp " ++ labelBegin
      Nothing -> throwError $ CodegenError
        "Found `continue` outside of a loop."

expression :: (MState m, MWriter m, MError m) => Expression -> m ()
expression expr = case expr of

  Constant i -> emit $ "movl $" ++ show i ++ ", %eax"

  Assignment name expr -> do
    expression expr
    vmap <- gets varMap
    case Map.lookup name vmap of
      Just offset -> emit $ "movl %eax, " ++ show offset ++ "(%ebp)"
      Nothing -> throwError . CodegenError $
                 "Assignment to undeclared variable, `" ++ name ++ "`."

  Reference name -> do
    vmap <- gets varMap
    case Map.lookup name vmap of
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
      Modulo -> do
        emit "movl $0, %edx"
        emit "idivl %ecx"
        emit "movl %edx, %eax"
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

withNestedContext :: MState m => m a -> m a
withNestedContext inner = do
  outerContext <- get
  result <- inner
  put outerContext
  return (result)
