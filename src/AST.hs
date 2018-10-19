module AST ( Expression ( Constant
                        , Unary
                        , Binary)
           , UnaryOp    ( Negation
                        , BitwiseComplement
                        , LogicalNegation)
           , BinaryOp   ( Addition
                        , Subtraction
                        , Multiplication
                        , Division)
           , Statement  ( Return)
           , Function   ( Function)
           , Program    ( Program)
           , pprint
           ) where

import Data.Tree ( Tree (Node), drawTree )

data Expression
  = Constant Int
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
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
  deriving (Eq, Show)

data Statement
  = Return Expression
  deriving (Eq, Show)

data Function
  = Function String Statement
  deriving (Eq, Show)

data Program
  = Program Function
  deriving (Eq, Show)


-- The (non-essential) definitions below allow the AST to be pretty-printed
-- using Data.Tree (drawTree)

class Treelike a where
  toDataTree :: a -> Tree String

  pprint :: a -> String
  pprint = drawTree . toDataTree

instance Treelike Expression where
  toDataTree term = case term of
    Constant i -> Node ("Constant " ++ show i) [toDataTree term]
    Unary op expr -> Node ("Unary " ++ show op) [toDataTree expr]
    Binary op e1 e2 -> Node ("Binary " ++ show op) [ toDataTree e1
                                                   , toDataTree e2]

instance Treelike Statement where
  toDataTree st = case st of
    Return expr -> Node "Return" [toDataTree expr]

instance Treelike Function where
  toDataTree (Function name body)
    = Node ("Function " ++ name) [toDataTree body]

instance Treelike Program where
  toDataTree (Program func)
    = Node "Program" [toDataTree func]
