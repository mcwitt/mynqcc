module AST where

data Program
  = Program Function
  deriving (Eq, Show)

data Function
  = Function String [String] (Maybe [BlockItem])
  deriving (Eq, Show)

data BlockItem
  = Statement Statement
  | Declaration Declaration
  deriving (Eq, Show)

data Declaration
  = Decl String (Maybe Expression)
  deriving (Eq, Show)

data Statement
  = Expression (Maybe Expression)
  | Return Expression
  | If Expression Statement (Maybe Statement)
  | Compound [BlockItem]
  | For (Maybe Expression) Expression (Maybe Expression) Statement
  | ForDecl Declaration Expression (Maybe Expression) Statement
  | While Expression Statement
  | Do Statement Expression
  | Break
  | Continue
  deriving (Eq, Show)

data Expression
  = Constant Int
  | Assignment String Expression
  | Reference String
  | Unary UnaryOp Expression
  | Binary BinaryOp Expression Expression
  | Conditional Expression Expression Expression
  | FunCall String [Expression]
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
  | Modulo
  deriving (Eq, Show)
