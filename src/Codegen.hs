{- |
This module implements code generation, i.e., the transformation from
the AST to assembly code.

Code generation works by traversing the AST and emitting assembly code
for each node of the tree. Roughly, the traversal takes place in the
following monad transformer stack:

1. Reader: passes dependencies (configuration, immutable context) down
   the hierarchy
2. Error: for exception handling (provides 'throwError')
3. Writer: collects emitted assembly code
4. State: keeps track of mutable state, e.g. label count, stack index,
   local vars, etc.

We split the state hierarchically into two pieces: the first is
mutated only by top-level AST nodes (e.g., function declarations); the
second is mutated by blockitem-level nodes, and contains, e.g., the
stack index and map of local variables to their location on the stack.
-}

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Codegen
  ( generate
  )
where
import           AST
import           Control.Monad                  ( when )
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import qualified Data.Set                      as Set
import           Numeric                        ( showHex )
import           Error
import           Target

type Asm = [String]

newtype Config = Config { target :: Target }

-- state that is read and written while generating function nodes
newtype GlobalState = GlobalState { functions :: Map.Map String FunctionInfo }

data FunctionInfo = FunctionInfo { isDefined :: Bool, numArgs :: Int }

-- state that is read and written while generating block nodes (and below)
data FunctionState =
  FunctionState { labelCount :: Int
                , stackIndex :: Int
                , varMap     :: Map.Map String Int
                , localVars  :: Set.Set String
                , breakTo    :: Maybe String
                , continueTo :: Maybe String}
  deriving Show

-- state that is read-only while generating block nodes
data FunctionContext =
  FunctionContext { config              :: Config
                  , globalState         :: GlobalState
                  , currentFunctionName :: String
                  , params              :: Set.Set String
                  }

type ConfigReaderM = MonadReader Config
type ErrorM = MonadError Error
type AsmWriterM = MonadWriter Asm
type GlobalStateM = MonadState GlobalState

-- | Monad in which we generate Function nodes
type FunctionGeneratorM m = (ConfigReaderM m, ErrorM m, AsmWriterM m, GlobalStateM m)

type FunctionStateM = MonadState FunctionState
type FunctionContextReaderM = MonadReader FunctionContext

{-| Monad in which we generate Block nodes. Note that the global state
 accessible only through the Reader monad and thus can't be mutated.
-}
type BlockGeneratorM m = (FunctionContextReaderM m, ErrorM m, AsmWriterM m, FunctionStateM m)

-- | Top-level function to generate assembly code given an AST
generate :: Target -> Program -> Either Error Asm
generate target prog = runExcept . execWriterT $ runReaderT
  (evalStateT (program prog) initialState)
  Config {target = target}
  where initialState = GlobalState {functions = Map.empty}

program :: FunctionGeneratorM m => Program -> m ()
program (Program funcs) = mapM_ function funcs

function :: FunctionGeneratorM m => Function -> m ()
function (Function name params body) = do
  globalState <- get
  let funcs = functions globalState
      info  = Map.lookup name funcs
  case info of
    Just info -> do
      when (length params /= numArgs info)
        $  throwError
        .  CodegenError
        $  "Inconsistent declaration of function `"
        ++ name
        ++ "`"
      when (isJust body) $ if isDefined info
        then
          throwError
          .  CodegenError
          $  "Multiple definitions of function `"
          ++ name
          ++ "`"
        else modify $ \c -> c
          { functions = Map.insert name
                                   (FunctionInfo True (length params))
                                   funcs
          }
    Nothing -> modify $ \c -> c
      { functions = Map.insert name
                               (FunctionInfo (isJust body) (length params))
                               funcs
      }
  case body of
    Just body -> do
      config <- ask
      let Target os = target config
          symbol    = symbolName os name
          context   = FunctionContext
            { config              = config
            , globalState         = globalState
            , currentFunctionName = name
            , params              = Set.fromList params
            }
      emit $ ".globl " ++ symbol
      emit $ symbol ++ ":"
      emit "push %ebp"
      emit "movl %esp, %ebp"
      let inner = block $ f name body
          f "main" []    = [Statement . Return . Constant $ 0]
          f _      items = items
          initialState = FunctionState
            { labelCount = 0
            , stackIndex = -4
            , varMap     = Map.fromList $ zip params [8, 12 ..]
            , localVars  = Set.fromList params
            , breakTo    = Nothing
            , continueTo = Nothing
            }
      evalStateT (runReaderT inner context) initialState
      emit $ "_" ++ name ++ "__end:"
      emit "movl %ebp, %esp"
      emit "pop %ebp"
      emit "ret"
    Nothing -> return ()

symbolName :: OS -> String -> String
symbolName os = (prefix ++)
 where
  prefix = case os of
    Darwin -> "_"
    _      -> ""

block :: BlockGeneratorM m => [BlockItem] -> m ()
block items = withNestedContext $ do
  modify $ \c -> c { localVars = Set.empty }
  mapM_ blockItem items
  vars <- gets localVars
  let bytes = 4 * Set.size vars
  emit $ "addl $" ++ show bytes ++ ", %esp"

blockItem :: BlockGeneratorM m => BlockItem -> m ()
blockItem item = case item of
  Statement   stat -> statement stat
  Declaration decl -> declaration decl

declaration :: BlockGeneratorM m => Declaration -> m ()
declaration (Decl name maybeExpr) = do
  vars <- gets localVars
  when (Set.member name vars)
    $  throwError
    .  CodegenError
    $  "Multiple declarations of `"
    ++ name
    ++ "` in the same block."
  params <- asks params
  when (Set.member name params)
    $  throwError
    .  CodegenError
    $  "Cannot redeclare function parameter `"
    ++ name
    ++ "` as a local variable"
  forM_ maybeExpr expression
  emit "push %eax"
  sidx <- gets stackIndex
  vmap <- gets varMap
  modify $ \c -> c { stackIndex = stackIndex c - 4
                   , varMap     = Map.insert name sidx vmap
                   , localVars  = Set.insert name vars
                   }

statement :: BlockGeneratorM m => Statement -> m ()
statement st = case st of

  Return expr -> do
    expression expr
    name <- asks currentFunctionName
    sidx <- gets stackIndex
    let bytes = -(4 + sidx)
    emit $ "addl $" ++ show bytes ++ ", %esp"
    emit $ "jmp _" ++ name ++ "__end"

  Expression expr      -> forM_ expr expression

  If expr s1 maybeStat -> do
    expression expr
    emit "cmpl $0, %eax"
    labelElse <- label "else"
    emit $ "je " ++ labelElse
    statement s1
    labelEndIf <- label "endif"
    emit $ "jmp " ++ labelEndIf
    emit $ labelElse ++ ":"
    forM_ maybeStat statement
    emit $ labelEndIf ++ ":"

  Compound items  -> block items

  While expr stat -> do
    labelBegin <- label "while_begin"
    emit $ labelBegin ++ ":"
    expression expr
    emit "cmpl $0, %eax"
    labelEnd <- label "while_end"
    emit $ "je " ++ labelEnd
    withNestedContext $ do
      modify $ \c -> c { breakTo = Just labelEnd, continueTo = Just labelBegin }
      statement stat
    emit $ "jmp " ++ labelBegin
    emit $ labelEnd ++ ":"

  Do stat expr -> do
    labelBegin <- label "do_begin"
    emit $ labelBegin ++ ":"
    labelEnd <- label "do_end"
    withNestedContext $ do
      modify $ \c -> c { breakTo = Just labelEnd, continueTo = Just labelBegin }
      statement stat
    expression expr
    emit "cmpl $0, %eax"
    emit $ "je " ++ labelEnd
    emit $ "jmp " ++ labelBegin
    emit $ labelEnd ++ ":"

  For maybeInit cond maybePost stat -> do
    forM_ maybeInit expression
    (labelBegin, labelEnd, labelPost) <- forBody cond
    withNestedContext $ do
      modify $ \c -> c { breakTo = Just labelEnd, continueTo = Just labelPost }
      statement stat
    emit $ labelPost ++ ":"
    forM_ maybePost expression
    emit $ "jmp " ++ labelBegin
    emit $ labelEnd ++ ":"

  ForDecl decl cond maybePost stat -> withNestedContext $ do
    modify $ \c -> c { localVars = Set.empty }
    declaration decl
    (labelBegin, labelEnd, labelPost) <- forBody cond
    modify $ \c -> c { breakTo = Just labelEnd, continueTo = Just labelPost }
    statement stat
    emit $ labelPost ++ ":"
    forM_ maybePost expression
    emit $ "jmp " ++ labelBegin
    emit $ labelEnd ++ ":"
    emit "addl $4, %esp"

  Break -> do
    maybeLabel <- gets breakTo
    case maybeLabel of
      Just l  -> emit $ "jmp " ++ l
      Nothing -> throwError $ CodegenError "Found `break` outside of a loop."

  Continue -> do
    maybeLabel <- gets continueTo
    case maybeLabel of
      Just l -> emit $ "jmp " ++ l
      Nothing ->
        throwError $ CodegenError "Found `continue` outside of a loop."

forBody :: BlockGeneratorM m => Expression -> m (String, String, String)
forBody cond = do
  labelBegin <- label "for_begin"
  emit $ labelBegin ++ ":"
  expression cond
  emit "cmpl $0, %eax"
  labelEnd <- label "for_end"
  emit $ "je " ++ labelEnd
  labelPost <- label "for_post"
  return (labelBegin, labelEnd, labelPost)

expression :: BlockGeneratorM m => Expression -> m ()
expression expr = case expr of

  Constant i           -> emit $ "movl $" ++ show i ++ ", %eax"

  Assignment name expr -> do
    expression expr
    vmap <- gets varMap
    case Map.lookup name vmap of
      Just offset -> emit $ "movl %eax, " ++ show offset ++ "(%ebp)"
      Nothing ->
        throwError
          .  CodegenError
          $  "Assignment to undeclared variable, `"
          ++ name
          ++ "`."

  Reference name -> do
    vmap <- gets varMap
    case Map.lookup name vmap of
      Just offset -> emit $ "movl " ++ show offset ++ "(%ebp), %eax"
      Nothing ->
        throwError
          .  CodegenError
          $  "Reference to undeclared variable, `"
          ++ name
          ++ "`."

  Unary op expr -> do
    expression expr
    case op of
      Negation          -> emit "neg %eax"
      BitwiseComplement -> emit "not %eax"
      LogicalNegation   -> do
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
      Addition       -> emit "addl %ecx, %eax"
      Subtraction    -> emit "subl %ecx, %eax"
      Multiplication -> emit "imul %ecx, %eax"
      Division       -> do
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

  FunCall name args -> do
    info <- asks $ Map.lookup name . functions . globalState
    case info of
      Just info ->
        when (length args /= numArgs info)
          $  throwError
          .  CodegenError
          $  "Function `"
          ++ name
          ++ "` called with wrong number of arguments"
      Nothing -> return ()
    Target os <- asks $ target . config
    mapM_ (\expr -> expression expr >> emit "push %eax") $ reverse args
    emit $ "call " ++ symbolName os name
    emit $ "add $0x" ++ showHex (length args * 4) "" ++ ", %esp"

label :: (FunctionContextReaderM m, FunctionStateM m) => String -> m String
label s = do
  prefix <- asks currentFunctionName
  lc     <- gets labelCount
  modify $ \ctx -> ctx { labelCount = succ lc }
  return $ "_" ++ prefix ++ "__" ++ s ++ "__" ++ show lc

emit :: AsmWriterM m => String -> m ()
emit = tell . pure

withNestedContext :: FunctionStateM m => m a -> m a
withNestedContext inner = do
  outerContext <- get
  result       <- inner
  put outerContext
  return result
