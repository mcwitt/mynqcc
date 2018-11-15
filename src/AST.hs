module AST where

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
  | Compound [BlockItem]
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
