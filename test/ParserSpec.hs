module ParserSpec
  ( spec
  )
where

import           AST
import           Error
import           Parser
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Token

spec :: Spec
spec = do
  describe "Stage 1" $ do

    it "should parse tokens from multi_digit.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 100
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function "main"
                                []
                                (Just [Statement (Return (Constant 100))])
                     ]
                   )

    it "should fail to parse tokens from missing_paren.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , OpenBrace
                   , KWReturn
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

  describe "Stage 2" $ do

    it "should parse tokens from bitwise.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LogicalNegation
                   , Integer 12
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return (Unary AST.LogicalNegation (Constant 12))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from bitwise_zero.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.BitwiseComplement
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Unary AST.BitwiseComplement (Constant 0))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from neg.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.Negation
                   , Integer 5
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return (Unary AST.Negation (Constant 5)))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from nested_ops.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LogicalNegation
                   , Token.Negation
                   , Integer 3
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Unary AST.LogicalNegation
                                        (Unary AST.Negation (Constant 3))
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from nested_ops_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.Negation
                   , Token.BitwiseComplement
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Unary
                                   AST.Negation
                                   (Unary AST.BitwiseComplement (Constant 0))
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from not_5.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LogicalNegation
                   , Integer 5
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return (Unary AST.LogicalNegation (Constant 5)))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from not_0.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LogicalNegation
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return (Unary AST.LogicalNegation (Constant 0)))
                           ]
                         )
                     ]
                   )

    it "should fail to parse tokens from missing_const.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LogicalNegation
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_semicolon.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LogicalNegation
                   , Integer 5
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from nested_missing_const.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LogicalNegation
                   , Token.BitwiseComplement
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from wrong_order.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 4
                   , Token.Negation
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

  describe "Stage 3" $ do

    it "should parse tokens from add.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.Addition
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Addition (Constant 1) (Constant 2))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from associativity.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.Negation
                   , Integer 2
                   , Token.Negation
                   , Integer 3
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.Subtraction
                                   (Binary AST.Subtraction
                                           (Constant 1)
                                           (Constant 2)
                                   )
                                   (Constant 3)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from associativity_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 6
                   , Token.Division
                   , Integer 3
                   , Token.Division
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.Division
                                   (Binary AST.Division
                                           (Constant 6)
                                           (Constant 3)
                                   )
                                   (Constant 2)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from div.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 4
                   , Token.Division
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Division (Constant 4) (Constant 2))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from mult.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.Multiplication
                   , Integer 3
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Multiplication
                                         (Constant 2)
                                         (Constant 3)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from parens.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.Multiplication
                   , OpenParen
                   , Integer 3
                   , Token.Addition
                   , Integer 4
                   , CloseParen
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.Multiplication
                                   (Constant 2)
                                   (Binary AST.Addition
                                           (Constant 3)
                                           (Constant 4)
                                   )
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from precedence.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.Addition
                   , Integer 3
                   , Token.Multiplication
                   , Integer 4
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.Addition
                                   (Constant 2)
                                   (Binary AST.Multiplication
                                           (Constant 3)
                                           (Constant 4)
                                   )
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from sub_neg.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.Negation
                   , Token.Negation
                   , Integer 1
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Subtraction
                                         (Constant 2)
                                         (Unary AST.Negation (Constant 1))
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from unop_add.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.BitwiseComplement
                   , Integer 2
                   , Token.Addition
                   , Integer 3
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.Addition
                                   (Unary AST.BitwiseComplement (Constant 2))
                                   (Constant 3)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from unop_parens.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.BitwiseComplement
                   , OpenParen
                   , Integer 1
                   , Token.Addition
                   , Integer 1
                   , CloseParen
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Unary
                                   AST.BitwiseComplement
                                   (Binary AST.Addition
                                           (Constant 1)
                                           (Constant 1)
                                   )
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should fail to parse tokens from malformed_parens.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , OpenParen
                   , Token.Negation
                   , Integer 3
                   , CloseParen
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_first.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.Division
                   , Integer 3
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from no_semicolon.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.Multiplication
                   , Integer 2
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

  describe "Stage 4" $ do

    it "should parse tokens from and_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.LogicalAnd
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LogicalAnd
                                         (Constant 1)
                                         (Constant 0)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from and_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.LogicalAnd
                   , Token.Negation
                   , Integer 1
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LogicalAnd
                                         (Constant 1)
                                         (Unary AST.Negation (Constant 1))
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from eq_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.Equality
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Equality (Constant 1) (Constant 2))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from eq_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.Equality
                   , Integer 1
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Equality (Constant 1) (Constant 1))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from ge_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.GreaterEqual
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.GreaterEqual
                                         (Constant 1)
                                         (Constant 2)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from ge_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.GreaterEqual
                   , Integer 1
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.GreaterEqual
                                         (Constant 1)
                                         (Constant 1)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from gt_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.GreaterThan
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.GreaterThan
                                         (Constant 1)
                                         (Constant 2)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from gt_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.GreaterThan
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.GreaterThan
                                         (Constant 1)
                                         (Constant 0)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from le_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.LessEqual
                   , Token.Negation
                   , Integer 1
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LessEqual
                                         (Constant 1)
                                         (Unary AST.Negation (Constant 1))
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from le_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 0
                   , Token.LessEqual
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LessEqual (Constant 0) (Constant 2)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from lt_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.LessThan
                   , Integer 1
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LessThan (Constant 2) (Constant 1))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from lt_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.LessThan
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LessThan (Constant 1) (Constant 2))
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from ne_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 0
                   , Token.Inequality
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Inequality
                                         (Constant 0)
                                         (Constant 0)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from ne_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.Negation
                   , Integer 1
                   , Token.Inequality
                   , Token.Negation
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.Inequality
                                         (Unary AST.Negation (Constant 1))
                                         (Unary AST.Negation (Constant 2))
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from or_false.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 0
                   , Token.LogicalOr
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LogicalOr (Constant 0) (Constant 0)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from or_true.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.LogicalOr
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary AST.LogicalOr (Constant 1) (Constant 0)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from precedence.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.LogicalOr
                   , Integer 0
                   , Token.LogicalAnd
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.LogicalOr
                                   (Constant 1)
                                   (Binary AST.LogicalAnd
                                           (Constant 0)
                                           (Constant 2)
                                   )
                                 )
                               )
                           ]
                         )
                     ]
                   )


    it "should parse tokens from precedence_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , OpenParen
                   , Integer 1
                   , Token.LogicalOr
                   , Integer 0
                   , CloseParen
                   , Token.LogicalAnd
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.LogicalAnd
                                   (Binary AST.LogicalOr
                                           (Constant 1)
                                           (Constant 0)
                                   )
                                   (Constant 0)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from precedence_3.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.Equality
                   , Integer 2
                   , Token.GreaterThan
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.Equality
                                   (Constant 2)
                                   (Binary AST.GreaterThan
                                           (Constant 2)
                                           (Constant 0)
                                   )
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from precedence_4.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.Equality
                   , Integer 2
                   , Token.LogicalOr
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Return
                                 (Binary
                                   AST.LogicalOr
                                   (Binary AST.Equality
                                           (Constant 2)
                                           (Constant 2)
                                   )
                                   (Constant 0)
                                 )
                               )
                           ]
                         )
                     ]
                   )

    it "should fail to parse tokens from missing_first_op.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Token.LessEqual
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_mid_op.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Token.LessThan
                   , Token.GreaterThan
                   , Integer 3
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from missing_second_op.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 2
                   , Token.LogicalAnd
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

  describe "Stage 5" $ do

    it "should parse tokens from assign.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Semicolon
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Statement
                             (Expression
                               (Just (AST.Assignment "a" (Constant 2)))
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from assign_val.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Declaration
                             (Decl "b" (Just (AST.Assignment "a" (Constant 0))))
                           , Statement (Return (Reference "b"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from exp_return_val.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Semicolon
                   , Identifier "a"
                   , Token.Assignment
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 4
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Token.Negation
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Declaration (Decl "b" Nothing)
                           , Statement
                             (Expression
                               (Just
                                 (AST.Assignment
                                   "a"
                                   (AST.Assignment "b" (Constant 4))
                                 )
                               )
                             )
                           , Statement
                             (Return
                               (Binary AST.Subtraction
                                       (Reference "a")
                                       (Reference "b")
                               )
                             )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from initialize.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 2)))
                           , Statement (Return (Constant 0))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from missing_return.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , CloseBrace
                   ]
      `shouldBe` Right (Program [Function "main" [] (Just [])])

    it "should parse tokens from multiple_vars.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Token.Addition
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 1)))
                           , Declaration (Decl "b" (Just (Constant 2)))
                           , Statement
                             (Return
                               (Binary AST.Addition
                                       (Reference "a")
                                       (Reference "b")
                               )
                             )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from no_initialize.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Semicolon
                   , KWReturn
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" Nothing)
                           , Statement (Return (Constant 0))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from refer.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 2)))
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from unused_exp.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , Integer 2
                   , Token.Addition
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                             (Expression
                               (Just
                                 (Binary AST.Addition (Constant 2) (Constant 2))
                               )
                             )
                           , Statement (Return (Constant 0))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from redefine.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 1)))
                           , Declaration (Decl "a" (Just (Constant 2)))
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should fail to parse tokens from syntax_err_bad_decl.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , Identifier "ints"
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_bad_decl_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "foo"
                   , Identifier "bar"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWReturn
                   , Identifier "bar"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_bad_lvalue.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , Identifier "a"
                   , Token.Addition
                   , Integer 3
                   , Token.Assignment
                   , Integer 4
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_bad_lvalue_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , Token.LogicalNegation
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_no_semicolon.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Identifier "a"
                   , Token.Assignment
                   , Identifier "a"
                   , Token.Addition
                   , Integer 4
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should parse tokens from undeclared_var.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function "main"
                                []
                                (Just [Statement (Return (Reference "a"))])
                     ]
                   )

    it "should parse tokens from var_declared_late.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Token.Addition
                   , Integer 2
                   , Semicolon
                   , KWInt
                   , Identifier "a"
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                             (Expression
                               (Just
                                 (AST.Assignment
                                   "a"
                                   (Binary AST.Addition
                                           (Constant 1)
                                           (Constant 2)
                                   )
                                 )
                               )
                             )
                           , Declaration (Decl "a" Nothing)
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

  describe "Stage 6" $ do

    it "should parse tokens from assign_ternary.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , QuestionMark
                   , Integer 2
                   , Colon
                   , Integer 3
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (Expression
                               (Just
                                 (AST.Assignment
                                   "a"
                                   (Conditional (Constant 1)
                                                (Constant 2)
                                                (Constant 3)
                                   )
                                 )
                               )
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from multiple_ternary.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Token.GreaterThan
                   , Integer 2
                   , QuestionMark
                   , Integer 3
                   , Colon
                   , Integer 4
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Token.GreaterThan
                   , Integer 2
                   , QuestionMark
                   , Integer 5
                   , Colon
                   , Integer 6
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Token.Addition
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration
                             (Decl
                               "a"
                               (Just
                                 (Conditional
                                   (Binary AST.GreaterThan
                                           (Constant 1)
                                           (Constant 2)
                                   )
                                   (Constant 3)
                                   (Constant 4)
                                 )
                               )
                             )
                           , Declaration
                             (Decl
                               "b"
                               (Just
                                 (Conditional
                                   (Binary AST.GreaterThan
                                           (Constant 1)
                                           (Constant 2)
                                   )
                                   (Constant 5)
                                   (Constant 6)
                                 )
                               )
                             )
                           , Statement
                             (Return
                               (Binary AST.Addition
                                       (Reference "a")
                                       (Reference "b")
                               )
                             )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from nested_ternary.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWInt
                   , Identifier "flag"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Token.GreaterThan
                   , Identifier "b"
                   , QuestionMark
                   , Integer 5
                   , Colon
                   , Identifier "flag"
                   , QuestionMark
                   , Integer 6
                   , Colon
                   , Integer 7
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 1)))
                           , Declaration (Decl "b" (Just (Constant 2)))
                           , Declaration (Decl "flag" (Just (Constant 0)))
                           , Statement
                             (Return
                               (Conditional
                                 (Binary AST.GreaterThan
                                         (Reference "a")
                                         (Reference "b")
                                 )
                                 (Constant 5)
                                 (Conditional (Reference "flag")
                                              (Constant 6)
                                              (Constant 7)
                                 )
                               )
                             )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from nested_ternary_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , QuestionMark
                   , Integer 2
                   , QuestionMark
                   , Integer 3
                   , Colon
                   , Integer 4
                   , Colon
                   , Integer 5
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 0
                   , QuestionMark
                   , Integer 2
                   , QuestionMark
                   , Integer 3
                   , Colon
                   , Integer 4
                   , Colon
                   , Integer 5
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Token.Multiplication
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration
                             (Decl
                               "a"
                               (Just
                                 (Conditional
                                   (Constant 1)
                                   (Conditional (Constant 2)
                                                (Constant 3)
                                                (Constant 4)
                                   )
                                   (Constant 5)
                                 )
                               )
                             )
                           , Declaration
                             (Decl
                               "b"
                               (Just
                                 (Conditional
                                   (Constant 0)
                                   (Conditional (Constant 2)
                                                (Constant 3)
                                                (Constant 4)
                                   )
                                   (Constant 5)
                                 )
                               )
                             )
                           , Statement
                             (Return
                               (Binary AST.Multiplication
                                       (Reference "a")
                                       (Reference "b")
                               )
                             )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from rh_assignment.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "flag"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , Identifier "flag"
                   , QuestionMark
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Colon
                   , OpenParen
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , CloseParen
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "flag" (Just (Constant 1)))
                           , Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (Expression
                               (Just
                                 (Conditional
                                   (Reference "flag")
                                   (AST.Assignment "a" (Constant 1))
                                   (AST.Assignment "a" (Constant 0))
                                 )
                               )
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from ternary.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Token.GreaterThan
                   , Token.Negation
                   , Integer 1
                   , QuestionMark
                   , Integer 4
                   , Colon
                   , Integer 5
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (Return
                               (Conditional
                                 (Binary AST.GreaterThan
                                         (Reference "a")
                                         (Unary AST.Negation (Constant 1))
                                 )
                                 (Constant 4)
                                 (Constant 5)
                               )
                             )
                           ]
                         )
                     ]
                   )

    it "should fail to parse tokens from incomplete_ternary.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , QuestionMark
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from malformed_ternary.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , QuestionMark
                   , Integer 2
                   , Colon
                   , Integer 3
                   , Colon
                   , Integer 4
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from malformed_ternary_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , QuestionMark
                   , Integer 2
                   , QuestionMark
                   , Integer 3
                   , Colon
                   , Integer 4
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from ternary_assign.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , Identifier "a"
                   , Token.GreaterThan
                   , Identifier "b"
                   , QuestionMark
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Colon
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should parse tokens from else.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , CloseParen
                   , KWReturn
                   , Integer 1
                   , Semicolon
                   , KWElse
                   , KWReturn
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (If (Reference "a")
                                 (Return (Constant 1))
                                 (Just (Return (Constant 2)))
                             )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from if_nested.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , CloseParen
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWElse
                   , KWIf
                   , OpenParen
                   , Identifier "b"
                   , CloseParen
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 1)))
                           , Declaration (Decl "b" (Just (Constant 0)))
                           , Statement
                             (If
                               (Reference "a")
                               (Expression
                                 (Just (AST.Assignment "b" (Constant 1)))
                               )
                               (Just
                                 (If
                                   (Reference "b")
                                   (Expression
                                     (Just (AST.Assignment "b" (Constant 2)))
                                   )
                                   Nothing
                                 )
                               )
                             )
                           , Statement (Return (Reference "b"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from if_nested_2.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , CloseParen
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWElse
                   , KWIf
                   , OpenParen
                   , Identifier "b"
                   , CloseParen
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Declaration (Decl "b" (Just (Constant 1)))
                           , Statement
                             (If
                               (Reference "a")
                               (Expression
                                 (Just (AST.Assignment "b" (Constant 1)))
                               )
                               (Just
                                 (If
                                   (Reference "b")
                                   (Expression
                                     (Just (AST.Assignment "b" (Constant 2)))
                                   )
                                   Nothing
                                 )
                               )
                             )
                           , Statement (Return (Reference "b"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from if_nested_3.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Integer 1
                   , CloseParen
                   , KWIf
                   , OpenParen
                   , Integer 2
                   , CloseParen
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWElse
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 4
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (If
                               (Constant 1)
                               (If
                                 (Constant 2)
                                 (Expression
                                   (Just (AST.Assignment "a" (Constant 3)))
                                 )
                                 (Just
                                   (Expression
                                     (Just (AST.Assignment "a" (Constant 4)))
                                   )
                                 )
                               )
                               Nothing
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from if_nested_4.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Integer 1
                   , CloseParen
                   , KWIf
                   , OpenParen
                   , Integer 0
                   , CloseParen
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWElse
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 4
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (If
                               (Constant 1)
                               (If
                                 (Constant 0)
                                 (Expression
                                   (Just (AST.Assignment "a" (Constant 3)))
                                 )
                                 (Just
                                   (Expression
                                     (Just (AST.Assignment "a" (Constant 4)))
                                   )
                                 )
                               )
                               Nothing
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from if_nested_5.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Integer 0
                   , CloseParen
                   , KWIf
                   , OpenParen
                   , Integer 0
                   , CloseParen
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWElse
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 4
                   , Semicolon
                   , KWElse
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (If
                               (Constant 0)
                               (If
                                 (Constant 0)
                                 (Expression
                                   (Just (AST.Assignment "a" (Constant 3)))
                                 )
                                 (Just
                                   (Expression
                                     (Just (AST.Assignment "a" (Constant 4)))
                                   )
                                 )
                               )
                               (Just
                                 (Expression
                                   (Just (AST.Assignment "a" (Constant 1)))
                                 )
                               )
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from if_not_taken.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , CloseParen
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Declaration (Decl "b" (Just (Constant 0)))
                           , Statement
                             (If
                               (Reference "a")
                               (Expression
                                 (Just (AST.Assignment "b" (Constant 1)))
                               )
                               Nothing
                             )
                           , Statement (Return (Reference "b"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from if_taken.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , CloseParen
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 1)))
                           , Declaration (Decl "b" (Just (Constant 0)))
                           , Statement
                             (If
                               (Reference "a")
                               (Expression
                                 (Just (AST.Assignment "b" (Constant 1)))
                               )
                               Nothing
                             )
                           , Statement (Return (Reference "b"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from multiple_if.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , CloseParen
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWElse
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "b"
                   , CloseParen
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 4
                   , Semicolon
                   , KWElse
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 5
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Token.Addition
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Declaration (Decl "b" (Just (Constant 0)))
                           , Statement
                             (If
                               (Reference "a")
                               (Expression
                                 (Just (AST.Assignment "a" (Constant 2)))
                               )
                               (Just
                                 (Expression
                                   (Just (AST.Assignment "a" (Constant 3)))
                                 )
                               )
                             )
                           , Statement
                             (If
                               (Reference "b")
                               (Expression
                                 (Just (AST.Assignment "b" (Constant 4)))
                               )
                               (Just
                                 (Expression
                                   (Just (AST.Assignment "b" (Constant 5)))
                                 )
                               )
                             )
                           , Statement
                             (Return
                               (Binary AST.Addition
                                       (Reference "a")
                                       (Reference "b")
                               )
                             )
                           ]
                         )
                     ]
                   )

  describe "Stage 7" $ do

    it "should parse tokens from consecutive_blocks.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   , OpenBrace
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 1)))
                           , Statement
                             (Compound
                               [Declaration (Decl "a" (Just (Constant 2)))]
                             )
                           , Statement
                             (Compound [Statement (Return (Reference "a"))])
                           ]
                         )
                     ]
                   )

    it "should parse tokens from consecutive_declarations.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , OpenBrace
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , Identifier "a"
                   , Token.Assignment
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   , OpenBrace
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , Identifier "a"
                   , Token.Assignment
                   , Identifier "a"
                   , Token.Addition
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (Compound
                               [ Declaration (Decl "b" (Just (Constant 1)))
                               , Statement
                                 (Expression
                                   (Just (AST.Assignment "a" (Reference "b")))
                                 )
                               ]
                             )
                           , Statement
                             (Compound
                               [ Declaration (Decl "b" (Just (Constant 2)))
                               , Statement
                                 (Expression
                                   (Just
                                     (AST.Assignment
                                       "a"
                                       (Binary AST.Addition
                                               (Reference "a")
                                               (Reference "b")
                                       )
                                     )
                                   )
                                 )
                               ]
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from declare_after_block.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "i"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "i" (Just (Constant 0)))
                           , Statement
                             (Compound
                               [Declaration (Decl "a" (Just (Constant 2)))]
                             )
                           , Declaration (Decl "b" (Just (Constant 3)))
                           , Statement (Return (Reference "b"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from declare_block.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWIf
                   , OpenParen
                   , Integer 5
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "i"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWReturn
                   , Identifier "i"
                   , Semicolon
                   , CloseBrace
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (If
                                 (Constant 5)
                                 (Compound
                                   [ Declaration (Decl "i" (Just (Constant 0)))
                                   , Statement (Return (Reference "i"))
                                   ]
                                 )
                                 Nothing
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from declare_late.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , OpenBrace
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , CloseBrace
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 2)))
                           , Statement
                             (Compound
                               [ Statement
                                 (Expression
                                   (Just (AST.Assignment "a" (Constant 3)))
                                 )
                               , Declaration (Decl "a" (Just (Constant 0)))
                               ]
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from multi_nesting.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , Token.LessThan
                   , Integer 3
                   , CloseParen
                   , OpenBrace
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 2)))
                           , Statement
                             (If
                               (Binary AST.LessThan (Reference "a") (Constant 3)
                               )
                               (Compound
                                 [ Statement
                                   (Compound
                                     [ Declaration
                                       (Decl "a" (Just (Constant 3)))
                                     , Statement (Return (Reference "a"))
                                     ]
                                   )
                                 , Statement (Return (Reference "a"))
                                 ]
                               )
                               Nothing
                             )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from nested_if.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   , KWElse
                   , OpenBrace
                   , KWInt
                   , Identifier "c"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "a"
                   , Token.LessThan
                   , Identifier "c"
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 4
                   , Semicolon
                   , CloseBrace
                   , KWElse
                   , OpenBrace
                   , KWReturn
                   , Integer 5
                   , Semicolon
                   , CloseBrace
                   , CloseBrace
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 0)))
                           , Statement
                             (If
                               (Reference "a")
                               (Compound
                                 [ Declaration (Decl "b" (Just (Constant 2)))
                                 , Statement (Return (Reference "b"))
                                 ]
                               )
                               (Just
                                 (Compound
                                   [ Declaration (Decl "c" (Just (Constant 3)))
                                   , Statement
                                     (If
                                       (Binary AST.LessThan
                                               (Reference "a")
                                               (Reference "c")
                                       )
                                       (Compound
                                         [Statement (Return (Constant 4))]
                                       )
                                       (Just
                                         (Compound
                                           [Statement (Return (Constant 5))]
                                         )
                                       )
                                     )
                                   ]
                                 )
                               )
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from nested_scope.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , KWInt
                   , Identifier "b"
                   , Token.Assignment
                   , Integer 3
                   , Semicolon
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 1
                   , Semicolon
                   , Identifier "b"
                   , Token.Assignment
                   , Identifier "b"
                   , Token.Addition
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   , KWReturn
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "a" (Just (Constant 2)))
                           , Declaration (Decl "b" (Just (Constant 3)))
                           , Statement
                             (Compound
                               [ Declaration (Decl "a" (Just (Constant 1)))
                               , Statement
                                 (Expression
                                   (Just
                                     (AST.Assignment
                                       "b"
                                       (Binary AST.Addition
                                               (Reference "b")
                                               (Reference "a")
                                       )
                                     )
                                   )
                                 )
                               ]
                             )
                           , Statement (Return (Reference "b"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from double_define.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Semicolon
                   , KWInt
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                               (Compound
                                 [ Declaration (Decl "a" Nothing)
                                 , Declaration (Decl "a" Nothing)
                                 ]
                               )
                           ]
                         )
                     ]
                   )

    it "should parse tokens from out_of_scope.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , OpenBrace
                   , KWInt
                   , Identifier "a"
                   , Token.Assignment
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   , KWReturn
                   , Identifier "a"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Statement
                             (Compound
                               [Declaration (Decl "a" (Just (Constant 2)))]
                             )
                           , Statement (Return (Reference "a"))
                           ]
                         )
                     ]
                   )

    it "should fail to parse tokens from syntax_err_extra_brace.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWIf
                   , OpenParen
                   , Integer 0
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Semicolon
                   , CloseBrace
                   , CloseBrace
                   , KWReturn
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

    it "should fail to parse tokens from syntax_err_missing_brace.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWIf
                   , OpenParen
                   , Integer 0
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 1
                   , Semicolon
                   , KWReturn
                   , Integer 2
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Left (ParserError "Failed to parse the program.")

  describe "Stage 8" $ do

    it "should parse tokens from break.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "sum"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWFor
                   , OpenParen
                   , KWInt
                   , Identifier "i"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , Identifier "i"
                   , Token.LessThan
                   , Integer 10
                   , Semicolon
                   , Identifier "i"
                   , Token.Assignment
                   , Identifier "i"
                   , Token.Addition
                   , Integer 1
                   , CloseParen
                   , OpenBrace
                   , Identifier "sum"
                   , Token.Assignment
                   , Identifier "sum"
                   , Token.Addition
                   , Identifier "i"
                   , Semicolon
                   , KWIf
                   , OpenParen
                   , Identifier "sum"
                   , Token.GreaterThan
                   , Integer 10
                   , CloseParen
                   , KWBreak
                   , Semicolon
                   , CloseBrace
                   , KWReturn
                   , Identifier "sum"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "sum" (Just (Constant 0)))
                           , Statement
                             (ForDecl
                               (Decl "i" (Just (Constant 0)))
                               (Binary AST.LessThan
                                       (Reference "i")
                                       (Constant 10)
                               )
                               (Just
                                 (AST.Assignment
                                   "i"
                                   (Binary AST.Addition
                                           (Reference "i")
                                           (Constant 1)
                                   )
                                 )
                               )
                               (Compound
                                 [ Statement
                                   (Expression
                                     (Just
                                       (AST.Assignment
                                         "sum"
                                         (Binary AST.Addition
                                                 (Reference "sum")
                                                 (Reference "i")
                                         )
                                       )
                                     )
                                   )
                                 , Statement
                                   (If
                                     (Binary AST.GreaterThan
                                             (Reference "sum")
                                             (Constant 10)
                                     )
                                     Break
                                     Nothing
                                   )
                                 ]
                               )
                             )
                           , Statement (Return (Reference "sum"))
                           ]
                         )
                     ]
                   )

    it "should parse tokens from continue.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "sum"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , KWFor
                   , OpenParen
                   , KWInt
                   , Identifier "i"
                   , Token.Assignment
                   , Integer 0
                   , Semicolon
                   , Identifier "i"
                   , Token.LessThan
                   , Integer 10
                   , Semicolon
                   , Identifier "i"
                   , Token.Assignment
                   , Identifier "i"
                   , Token.Addition
                   , Integer 1
                   , CloseParen
                   , OpenBrace
                   , KWIf
                   , OpenParen
                   , Identifier "sum"
                   , PercentSign
                   , Integer 2
                   , CloseParen
                   , KWContinue
                   , Semicolon
                   , Identifier "sum"
                   , Token.Assignment
                   , Identifier "sum"
                   , Token.Addition
                   , Identifier "i"
                   , Semicolon
                   , CloseBrace
                   , KWReturn
                   , Identifier "sum"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                         "main"
                         []
                         (Just
                           [ Declaration (Decl "sum" (Just (Constant 0)))
                           , Statement
                             (ForDecl
                               (Decl "i" (Just (Constant 0)))
                               (Binary AST.LessThan
                                       (Reference "i")
                                       (Constant 10)
                               )
                               (Just
                                 (AST.Assignment
                                   "i"
                                   (Binary AST.Addition
                                           (Reference "i")
                                           (Constant 1)
                                   )
                                 )
                               )
                               (Compound
                                 [ Statement
                                   (If
                                     (Binary Modulo
                                             (Reference "sum")
                                             (Constant 2)
                                     )
                                     Continue
                                     Nothing
                                   )
                                 , Statement
                                   (Expression
                                     (Just
                                       (AST.Assignment
                                         "sum"
                                         (Binary AST.Addition
                                                 (Reference "sum")
                                                 (Reference "i")
                                         )
                                       )
                                     )
                                   )
                                 ]
                               )
                             )
                           , Statement (Return (Reference "sum"))
                           ]
                         )
                     ]
                   )

  describe "Stage 9" $ do

    it "should parse tokens from expression_args.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "add"
                   , OpenParen
                   , KWInt
                   , Identifier "a"
                   , Comma
                   , KWInt
                   , Identifier "b"
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Identifier "a"
                   , Token.Addition
                   , Identifier "b"
                   , Semicolon
                   , CloseBrace
                   , KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "sum"
                   , Token.Assignment
                   , Identifier "add"
                   , OpenParen
                   , Integer 1
                   , Token.Addition
                   , Integer 2
                   , Comma
                   , Integer 4
                   , CloseParen
                   , Semicolon
                   , KWReturn
                   , Identifier "sum"
                   , Token.Addition
                   , Identifier "sum"
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function
                       "add"
                       ["a", "b"]
                       (Just
                         [ Statement
                             (Return
                               (Binary AST.Addition
                                       (Reference "a")
                                       (Reference "b")
                               )
                             )
                         ]
                       )
                     , Function
                       "main"
                       []
                       (Just
                         [ Declaration
                           (Decl
                             "sum"
                             (Just
                               (FunCall
                                 "add"
                                 [ Binary AST.Addition (Constant 1) (Constant 2)
                                 , Constant 4
                                 ]
                               )
                             )
                           )
                         , Statement
                           (Return
                             (Binary AST.Addition
                                     (Reference "sum")
                                     (Reference "sum")
                             )
                           )
                         ]
                       )
                     ]
                   )

    it "should parse tokens from fib.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "fib"
                   , OpenParen
                   , KWInt
                   , Identifier "n"
                   , CloseParen
                   , OpenBrace
                   , KWIf
                   , OpenParen
                   , Identifier "n"
                   , Token.Equality
                   , Integer 0
                   , Token.LogicalOr
                   , Identifier "n"
                   , Token.Equality
                   , Integer 1
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Identifier "n"
                   , Semicolon
                   , CloseBrace
                   , KWElse
                   , OpenBrace
                   , KWReturn
                   , Identifier "fib"
                   , OpenParen
                   , Identifier "n"
                   , Token.Negation
                   , Integer 1
                   , CloseParen
                   , Token.Addition
                   , Identifier "fib"
                   , OpenParen
                   , Identifier "n"
                   , Token.Negation
                   , Integer 2
                   , CloseParen
                   , Semicolon
                   , CloseBrace
                   , CloseBrace
                   , KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWInt
                   , Identifier "n"
                   , Token.Assignment
                   , Integer 5
                   , Semicolon
                   , KWReturn
                   , Identifier "fib"
                   , OpenParen
                   , Identifier "n"
                   , CloseParen
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` (Right
                   (Program
                     [ Function
                       "fib"
                       ["n"]
                       (Just
                         [ Statement
                             (If
                               (Binary
                                 AST.LogicalOr
                                 (Binary AST.Equality
                                         (Reference "n")
                                         (Constant 0)
                                 )
                                 (Binary AST.Equality
                                         (Reference "n")
                                         (Constant 1)
                                 )
                               )
                               (Compound [Statement (Return (Reference "n"))])
                               (Just
                                 (Compound
                                   [ Statement
                                       (Return
                                         (Binary
                                           AST.Addition
                                           (FunCall
                                             "fib"
                                             [ (Binary Subtraction
                                                       (Reference "n")
                                                       (Constant 1)
                                               )
                                             ]
                                           )
                                           (FunCall
                                             "fib"
                                             [ (Binary Subtraction
                                                       (Reference "n")
                                                       (Constant 2)
                                               )
                                             ]
                                           )
                                         )
                                       )
                                   ]
                                 )
                               )
                             )
                         ]
                       )
                     , Function
                       "main"
                       []
                       (Just
                         [ Declaration (Decl "n" (Just (Constant 5)))
                         , (Statement (Return (FunCall "fib" [(Reference "n")]))
                           )
                         ]
                       )
                     ]
                   )
                 )
