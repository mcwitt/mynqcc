{-# LANGUAGE DeriveFunctor #-}

module AST where

import           Control.Arrow
import           RecursionSchemes               ( Fix(Fix) )

{-| Recursive AST types like @Statement@ and @Expression@ are defined
  as parametric types to "factor out" the recursion and allow use with
  recursion-schemes. To represent arbitrarily-deep nesting of
  subexpressions, we use the fixed-point functor, "Fix", e.g.

  >> type Program = Fix ProgF

  The following blog post series was helpful in understanding how this
  works:
  https://blog.sumtypeofway.com/an-introduction-to-recursion-schemes/
-}

newtype Program = Program [Function]
  deriving (Eq, Show)

data Function = Function String [String] (Maybe [BlockItem])
  deriving (Eq, Show)

data BlockItem
  = Statement Statement
  | Declaration Declaration
  deriving (Eq, Show)

data Declaration = Decl String (Maybe Expression)
  deriving (Eq, Show)

data StatF a
  = Expression (Maybe Expression)
  | Return Expression
  | If Expression a (Maybe a)
  | Compound [BlockItem]
  | For (Maybe Expression) Expression (Maybe Expression) a
  | ForDecl Declaration Expression (Maybe Expression) a
  | While Expression a
  | Do a Expression
  | Break
  | Continue
  deriving (Eq, Functor, Show)

data ExprF a
  = Constant Int
  | Assignment String a
  | Reference String
  | Unary UnaryOp a
  | Binary BinaryOp a a
  | Conditional a a a
  | FunCall String [a]
  deriving (Eq, Functor, Show)

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


-- Define aliases for the fixed-point types

type Statement = Fix StatF
type Expression = Fix ExprF

-- The rest is essentially boilerplate. We're just creating functions
-- that wrap each constructor of the above "pattern functors" (StatF
-- and ExprF) in Fix. (Could we auto-generate these definitions?)

expression :: Maybe Expression -> Statement
expression = Fix . Expression

return_ :: Expression -> Statement
return_ = Fix . Return

if_ :: Expression -> Statement -> Maybe Statement -> Statement
if_ expr s1 s2 = Fix $ If expr s1 s2

compound :: [BlockItem] -> Statement
compound = Fix . Compound

for :: Maybe Expression -> Expression -> Maybe Expression -> Statement -> Statement
for init cond inc body = Fix $ For init cond inc body

forDecl :: Declaration -> Expression -> Maybe Expression -> Statement -> Statement
forDecl decl cond inc body = Fix $ ForDecl decl cond inc body

while :: Expression -> Statement -> Statement
while expr st = Fix $ While expr st

do_ :: Statement -> Expression -> Statement
do_ st cond = Fix $ Do st cond

break :: Statement
break = Fix Break

continue :: Statement
continue = Fix Continue

constant :: Int -> Expression
constant = Fix . Constant

assignment :: String -> Expression -> Expression
assignment name expr = Fix (Assignment name expr)

reference :: String -> Expression
reference = Fix . Reference

unary :: UnaryOp -> Expression -> Expression
unary op expr = Fix (Unary op expr)

binary :: BinaryOp -> Expression -> Expression -> Expression
binary op l r = Fix (Binary op l r)

conditional :: Expression -> Expression -> Expression -> Expression
conditional cond l r = Fix (Conditional cond l r)

funCall :: String -> [Expression] -> Expression
funCall name exprs = Fix (FunCall name exprs)
