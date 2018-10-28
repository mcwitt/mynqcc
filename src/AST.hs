module AST where

import           Data.Tree   (Tree (Node), drawTree)
import           Text.Printf

data Program
  = Program Function
  deriving (Eq, Show)

data Function
  = Function String [BlockItem]
  deriving (Eq, Show)

data BlockItem
  = Statement Statement
  | Declaration String (Maybe Expression)
  deriving (Eq, Show)

data Statement
  = Expression Expression
  | Return Expression
  | If Expression Statement (Maybe Statement)
  deriving (Eq, Show)

data Expression
  = Constant Int
  | Assignment String Expression
  | Reference String
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
  | Conditional Expression Expression Expression
  deriving (Eq, Show)

data UnaryOp
  = Negation
  | BitwiseComplement
  | LogicalNegation
  deriving (Eq, Show)

data BinaryOp
  = Addition
  | Subtraction
  | Multiplication
  | Division
  | LogicalAnd
  | LogicalOr
  | Equality
  | Inequality
  | LessThan
  | GreaterThan
  | LessEqual
  | GreaterEqual
  deriving (Eq, Show)

-- The (non-essential) definitions below allow the AST to be pretty-printed
-- using Data.Tree (drawTree)

class Treelike a where
  toDataTree :: a -> Tree String

  pprint :: a -> String
  pprint = drawTree . toDataTree

instance Treelike Program where
  toDataTree (Program func)
    = Node "Program" [toDataTree func]

instance Treelike Function where
  toDataTree (Function name blockItems)
    = Node ("Function " ++ name) $ map toDataTree blockItems

instance Treelike BlockItem where
  toDataTree item = case item of
    Statement stat -> Node "Statement " [toDataTree stat]
    Declaration name (Just expr) ->
      Node (printf "Declaration with initializer: `%s`" name) [toDataTree expr]
    Declaration name Nothing ->
      Node (printf "Declaration: `%s`" name) []

instance Treelike Statement where
  toDataTree stat = case stat of
    Return expr -> Node "Return" [toDataTree expr]
    Expression expr -> Node "Expression" [toDataTree expr]
    If e1 stat (Just e2) ->
      Node "If with else" [ toDataTree e1
                          , toDataTree stat
                          , toDataTree e2]
    If e1 stat Nothing ->
      Node "If" [ toDataTree e1
                , toDataTree stat]

instance Treelike Expression where
  toDataTree expr = case expr of
    Constant i -> Node ("Constant " ++ show i) []
    Assignment name expr ->
      Node (printf "Assignment: `%s`" name) [toDataTree expr]
    Reference name -> Node (printf "Reference: `%s`" name) []
    Unary op expr -> Node ("Unary " ++ show op) [toDataTree expr]
    Binary op e1 e2 -> Node ("Binary " ++ show op) [ toDataTree e1
                                                   , toDataTree e2]
    Conditional e1 e2 e3 -> Node "Conditional" (map toDataTree [e1, e2, e3])

