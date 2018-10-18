module AST ( Factor     ( Factor
                        , Constant
                        , Negation
                        , BitwiseComplement
                        , LogicalNegation)
           , Term       ( Term
                        , Multiplication
                        , Division)
           , Expression ( Expression
                        , Addition
                        , Subtraction)
           , Statement  ( Return)
           , Function   ( Function)
           , Program    ( Program)
           , pprint
           ) where

import Data.Tree ( Tree (Node), drawTree )

data Factor
  = Factor Expression
  | Constant Int
  | Negation Factor
  | BitwiseComplement Factor
  | LogicalNegation Factor
  deriving (Eq, Show)

data Term
  = Term Factor
  | Multiplication Term Term
  | Division Term Term
  deriving (Eq, Show)

data Expression
  = Expression Term
  | Addition Expression Expression
  | Subtraction Expression Expression
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

class ASTNode a where
  toDataTree :: a -> Tree String

  pprint :: a -> String
  pprint = drawTree . toDataTree

instance ASTNode Factor where
  toDataTree fact = case fact of
    Factor expr -> Node "Factor" [toDataTree expr]
    Constant ival -> Node ("Constant Int " ++ show ival) []
    Negation fact -> Node "Negation" [toDataTree fact]
    BitwiseComplement fact -> Node "Bitwise complement" [toDataTree fact]
    LogicalNegation fact -> Node "Logical negation" [toDataTree fact]

instance ASTNode Term where
  toDataTree term = case term of
    Term fact -> Node "Term" [toDataTree fact]
    Multiplication l r -> Node "Multiplication" [ toDataTree l
                                                , toDataTree r]

instance ASTNode Expression where
  toDataTree term = case term of
    Expression term -> Node "Expression" [toDataTree term]
    Addition l r -> Node "Addition" [ toDataTree l
                                    , toDataTree r]

instance ASTNode Statement where
  toDataTree st = case st of
    Return expr -> Node "Return" [toDataTree expr]

instance ASTNode Function where
  toDataTree (Function name body)
    = Node ("Function " ++ name) [toDataTree body]

instance ASTNode Program where
  toDataTree (Program func)
    = Node "Program" [toDataTree func]
