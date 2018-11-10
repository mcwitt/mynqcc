module Codegen (generate) where

import           AST
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as Map
import           Error


{- | Inner monad type in which all code generation actions are executed. The
inner Except monad allows for exception handling, and the WriterT keeps track of
the assembly code generated so far.
-}
type Gen = WriterT [String] (Except Error)

{- | Inside functions, we need to keep track of some additional state: for
example, the stack index and variable offsets relative to the base pointer
(EBP). We wrap the base monad in a `StateT FunctionContext` to handle the
additional bookkeeping.
-}
type FuncGen = StateT Context Gen

data Context =
  Context { funcName   :: String
          , stackIndex :: Int
          , varOffsets :: Map.Map String Int
          , labelCount :: Int}
  deriving Show

generate :: Program -> Either Error [String]
generate = runExcept . execWriterT . program

program :: Program -> Gen ()
program (Program func) = function func

function :: Function -> Gen ()
function (Function name body) = do
  emit $ ".globl " ++ name
  emit $ name ++ ":"
  emit "push %ebp"
  emit "movl %esp, %ebp"
  let inner = case (Function name body) of
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

blockItem :: BlockItem -> FuncGen ()
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
    emitL "push %eax"
    si <- gets stackIndex
    modify $ \ctx -> ctx { stackIndex = stackIndex ctx - 4
                         , varOffsets = Map.insert name si vars}


statement :: Statement -> FuncGen ()
statement st = case st of

  Return expr -> expression expr
  Expression expr -> expression expr

  If expr s1 maybeStat -> do
    expression expr
    emitL "cmpl $0, %eax"
    labelElse <- label "else"
    emitL $ "je " ++ labelElse
    statement s1
    labelEndIf <- label "endif"
    emitL $ "jmp " ++ labelEndIf
    emitL $ labelElse ++ ":"
    case maybeStat of
      Just s2 -> statement s2
      Nothing -> return ()
    emitL $ labelEndIf ++ ":"


expression :: Expression -> FuncGen ()
expression expr = case expr of

  Constant i -> emitL $ "movl $" ++ show i ++ ", %eax"

  Assignment name expr -> do
    expression expr
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emitL $ "movl %eax, " ++ show offset ++ "(%ebp)"
      Nothing -> throwError . CodegenError $
                 "Assignment to undeclared variable, `" ++ name ++ "`."

  Reference name -> do
    vars <- gets varOffsets
    case Map.lookup name vars of
      Just offset -> emitL $ "movl " ++ show offset ++ "(%ebp), %eax"
      Nothing -> throwError . CodegenError $
                 "Reference to undeclared variable, `" ++ name ++ "`."

  Unary op expr -> do
    expression expr
    case op of Negation -> emitL "neg %eax"
               BitwiseComplement -> emitL "not %eax"
               LogicalNegation -> do
                 emitL "cmpl $0, %eax"
                 emitL "movl $0, %eax"
                 emitL "sete %al"

  -- | General strategy for evaluating binary operators on
  -- sub-expressions e1, e2:
  -- 1. Evaluate e1, push result onto the stack
  -- 2. Evaluate e2
  -- 3. Pop the result of e1 back into a register
  -- 4. Perform the binary operation on e1, e2
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

  Conditional e1 e2 e3 -> do
    expression e1
    emitL "cmpl $0, %eax"
    labelE3 <- label "e3"
    emitL $ "je " ++ labelE3
    expression e2
    labelPostCond <- label "post_conditional"
    emitL $ "jmp " ++ labelPostCond
    emitL $ labelE3 ++ ":"
    expression e3
    emitL $ labelPostCond ++ ":"

label :: String -> FuncGen String
label s = do
  prefix <- gets funcName
  lc <- gets labelCount
  modify $ \ctx -> ctx { labelCount = succ lc }
  return $ "_" ++ prefix ++ "__" ++ s ++ "__" ++ show lc

emit :: String -> Gen ()
emit s = writer ((), [s])

emitL :: String -> FuncGen ()
emitL = lift . emit
