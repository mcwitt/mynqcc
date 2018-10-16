module AST ( Expression (Constant)
           , Statement  (Return)
           , Function   (Function)
           , Program    (Program)) where

import Data.Tree ( Tree (Node), drawTree )

data Expression = Constant Int
                deriving (Show, Eq)

data Statement = Return Expression
               deriving (Show, Eq)

data Function = Function String Statement
              deriving (Show, Eq)

data Program = Program Function
             deriving (Show, Eq)


-- The (non-essential) definitions below allow the AST to be pretty-printed
-- using Data.Tree (drawTree)

class ASTNode a where
  toDataTree :: a -> Tree String

  pprint :: a -> String
  pprint = drawTree . toDataTree

instance ASTNode Expression where
  toDataTree (Constant ival) = Node ("Constant Int " ++ (show ival)) []

instance ASTNode Statement where
  toDataTree (Return expr) = Node "Return" [toDataTree expr]

instance ASTNode Function where
  toDataTree (Function name body) = Node ("Function " ++ name) [toDataTree body]

instance ASTNode Program where
  toDataTree (Program func) = Node "Program" [toDataTree func]
