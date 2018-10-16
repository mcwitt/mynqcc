module AST ( Expression (Constant)
           , Statement  (Return)
           , Function   (Function)
           , Program    (Program)) where

data Expression = Constant Int
                deriving (Show, Eq)

data Statement = Return Expression
               deriving (Show, Eq)

data Function = Function String Statement
              deriving (Show, Eq)

data Program = Program Function
             deriving (Show, Eq)

