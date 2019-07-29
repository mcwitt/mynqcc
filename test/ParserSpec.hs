module ParserSpec
  ( spec
  )
where

import           AST
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
                                (Just [Statement (return_ (constant 100))])
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
                               (return_ (unary AST.LogicalNegation (constant 12))
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
                               (return_
                                 (unary AST.BitwiseComplement (constant 0))
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
                               (return_ (unary AST.Negation (constant 5)))
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
                               (return_
                                 (unary AST.LogicalNegation
                                        (unary AST.Negation (constant 3))
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
                               (return_
                                 (unary
                                   AST.Negation
                                   (unary AST.BitwiseComplement (constant 0))
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
                               (return_ (unary AST.LogicalNegation (constant 5)))
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
                               (return_ (unary AST.LogicalNegation (constant 0)))
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
                               (return_
                                 (binary AST.Addition (constant 1) (constant 2))
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
                               (return_
                                 (binary
                                   AST.Subtraction
                                   (binary AST.Subtraction
                                           (constant 1)
                                           (constant 2)
                                   )
                                   (constant 3)
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
                               (return_
                                 (binary
                                   AST.Division
                                   (binary AST.Division
                                           (constant 6)
                                           (constant 3)
                                   )
                                   (constant 2)
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
                               (return_
                                 (binary AST.Division (constant 4) (constant 2))
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
                               (return_
                                 (binary AST.Multiplication
                                         (constant 2)
                                         (constant 3)
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
                               (return_
                                 (binary
                                   AST.Multiplication
                                   (constant 2)
                                   (binary AST.Addition
                                           (constant 3)
                                           (constant 4)
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
                               (return_
                                 (binary
                                   AST.Addition
                                   (constant 2)
                                   (binary AST.Multiplication
                                           (constant 3)
                                           (constant 4)
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
                               (return_
                                 (binary AST.Subtraction
                                         (constant 2)
                                         (unary AST.Negation (constant 1))
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
                               (return_
                                 (binary
                                   AST.Addition
                                   (unary AST.BitwiseComplement (constant 2))
                                   (constant 3)
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
                               (return_
                                 (unary
                                   AST.BitwiseComplement
                                   (binary AST.Addition
                                           (constant 1)
                                           (constant 1)
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
                               (return_
                                 (binary AST.LogicalAnd
                                         (constant 1)
                                         (constant 0)
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
                               (return_
                                 (binary AST.LogicalAnd
                                         (constant 1)
                                         (unary AST.Negation (constant 1))
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
                               (return_
                                 (binary AST.Equality (constant 1) (constant 2))
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
                               (return_
                                 (binary AST.Equality (constant 1) (constant 1))
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
                               (return_
                                 (binary AST.GreaterEqual
                                         (constant 1)
                                         (constant 2)
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
                               (return_
                                 (binary AST.GreaterEqual
                                         (constant 1)
                                         (constant 1)
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
                               (return_
                                 (binary AST.GreaterThan
                                         (constant 1)
                                         (constant 2)
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
                               (return_
                                 (binary AST.GreaterThan
                                         (constant 1)
                                         (constant 0)
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
                               (return_
                                 (binary AST.LessEqual
                                         (constant 1)
                                         (unary AST.Negation (constant 1))
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
                               (return_
                                 (binary AST.LessEqual (constant 0) (constant 2)
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
                               (return_
                                 (binary AST.LessThan (constant 2) (constant 1))
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
                               (return_
                                 (binary AST.LessThan (constant 1) (constant 2))
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
                               (return_
                                 (binary AST.Inequality
                                         (constant 0)
                                         (constant 0)
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
                               (return_
                                 (binary AST.Inequality
                                         (unary AST.Negation (constant 1))
                                         (unary AST.Negation (constant 2))
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
                               (return_
                                 (binary AST.LogicalOr (constant 0) (constant 0)
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
                               (return_
                                 (binary AST.LogicalOr (constant 1) (constant 0)
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
                               (return_
                                 (binary
                                   AST.LogicalOr
                                   (constant 1)
                                   (binary AST.LogicalAnd
                                           (constant 0)
                                           (constant 2)
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
                               (return_
                                 (binary
                                   AST.LogicalAnd
                                   (binary AST.LogicalOr
                                           (constant 1)
                                           (constant 0)
                                   )
                                   (constant 0)
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
                               (return_
                                 (binary
                                   AST.Equality
                                   (constant 2)
                                   (binary AST.GreaterThan
                                           (constant 2)
                                           (constant 0)
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
                               (return_
                                 (binary
                                   AST.LogicalOr
                                   (binary AST.Equality
                                           (constant 2)
                                           (constant 2)
                                   )
                                   (constant 0)
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
                             (expression
                               (Just (AST.assignment "a" (constant 2)))
                             )
                           , Statement (return_ (reference "a"))
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
                             (Decl "b" (Just (AST.assignment "a" (constant 0))))
                           , Statement (return_ (reference "b"))
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
                             (expression
                               (Just
                                 (AST.assignment
                                   "a"
                                   (AST.assignment "b" (constant 4))
                                 )
                               )
                             )
                           , Statement
                             (return_
                               (binary AST.Subtraction
                                       (reference "a")
                                       (reference "b")
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
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement (return_ (constant 0))
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
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 2)))
                           , Statement
                             (return_
                               (binary AST.Addition
                                       (reference "a")
                                       (reference "b")
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
                           , Statement (return_ (constant 0))
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
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement (return_ (reference "a"))
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
                             (expression
                               (Just
                                 (binary AST.Addition (constant 2) (constant 2))
                               )
                             )
                           , Statement (return_ (constant 0))
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
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "a" (Just (constant 2)))
                           , Statement (return_ (reference "a"))
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
                                (Just [Statement (return_ (reference "a"))])
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
                             (expression
                               (Just
                                 (assignment
                                   "a"
                                   (binary AST.Addition
                                           (constant 1)
                                           (constant 2)
                                   )
                                 )
                               )
                             )
                           , Declaration (Decl "a" Nothing)
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (expression
                               (Just
                                 (assignment
                                   "a"
                                   (conditional (constant 1)
                                                (constant 2)
                                                (constant 3)
                                   )
                                 )
                               )
                             )
                           , Statement (return_ (reference "a"))
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
                                 (conditional
                                   (binary AST.GreaterThan
                                           (constant 1)
                                           (constant 2)
                                   )
                                   (constant 3)
                                   (constant 4)
                                 )
                               )
                             )
                           , Declaration
                             (Decl
                               "b"
                               (Just
                                 (conditional
                                   (binary AST.GreaterThan
                                           (constant 1)
                                           (constant 2)
                                   )
                                   (constant 5)
                                   (constant 6)
                                 )
                               )
                             )
                           , Statement
                             (return_
                               (binary AST.Addition
                                       (reference "a")
                                       (reference "b")
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
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 2)))
                           , Declaration (Decl "flag" (Just (constant 0)))
                           , Statement
                             (return_
                               (conditional
                                 (binary AST.GreaterThan
                                         (reference "a")
                                         (reference "b")
                                 )
                                 (constant 5)
                                 (conditional (reference "flag")
                                              (constant 6)
                                              (constant 7)
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
                                 (conditional
                                   (constant 1)
                                   (conditional (constant 2)
                                                (constant 3)
                                                (constant 4)
                                   )
                                   (constant 5)
                                 )
                               )
                             )
                           , Declaration
                             (Decl
                               "b"
                               (Just
                                 (conditional
                                   (constant 0)
                                   (conditional (constant 2)
                                                (constant 3)
                                                (constant 4)
                                   )
                                   (constant 5)
                                 )
                               )
                             )
                           , Statement
                             (return_
                               (binary AST.Multiplication
                                       (reference "a")
                                       (reference "b")
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
                           [ Declaration (Decl "flag" (Just (constant 1)))
                           , Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (expression
                               (Just
                                 (conditional
                                   (reference "flag")
                                   (assignment "a" (constant 1))
                                   (assignment "a" (constant 0))
                                 )
                               )
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (return_
                               (conditional
                                 (binary AST.GreaterThan
                                         (reference "a")
                                         (unary AST.Negation (constant 1))
                                 )
                                 (constant 4)
                                 (constant 5)
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_ (reference "a")
                                 (return_ (constant 1))
                                 (Just (return_ (constant 2)))
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
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 0)))
                           , Statement
                             (if_
                               (reference "a")
                               (expression
                                 (Just (assignment "b" (constant 1)))
                               )
                               (Just
                                 (if_
                                   (reference "b")
                                   (expression
                                     (Just (assignment "b" (constant 2)))
                                   )
                                   Nothing
                                 )
                               )
                             )
                           , Statement (return_ (reference "b"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Declaration (Decl "b" (Just (constant 1)))
                           , Statement
                             (if_
                               (reference "a")
                               (expression
                                 (Just (assignment "b" (constant 1)))
                               )
                               (Just
                                 (if_
                                   (reference "b")
                                   (expression
                                     (Just (assignment "b" (constant 2)))
                                   )
                                   Nothing
                                 )
                               )
                             )
                           , Statement (return_ (reference "b"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_
                               (constant 1)
                               (if_
                                 (constant 2)
                                 (expression
                                   (Just (assignment "a" (constant 3)))
                                 )
                                 (Just
                                   (expression
                                     (Just (assignment "a" (constant 4)))
                                   )
                                 )
                               )
                               Nothing
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_
                               (constant 1)
                               (if_
                                 (constant 0)
                                 (expression
                                   (Just (assignment "a" (constant 3)))
                                 )
                                 (Just
                                   (expression
                                     (Just (assignment "a" (constant 4)))
                                   )
                                 )
                               )
                               Nothing
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_
                               (constant 0)
                               (if_
                                 (constant 0)
                                 (expression
                                   (Just (assignment "a" (constant 3)))
                                 )
                                 (Just
                                   (expression
                                     (Just (assignment "a" (constant 4)))
                                   )
                                 )
                               )
                               (Just
                                 (expression
                                   (Just (assignment "a" (constant 1)))
                                 )
                               )
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Declaration (Decl "b" (Just (constant 0)))
                           , Statement
                             (if_
                               (reference "a")
                               (expression
                                 (Just (assignment "b" (constant 1)))
                               )
                               Nothing
                             )
                           , Statement (return_ (reference "b"))
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
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Declaration (Decl "b" (Just (constant 0)))
                           , Statement
                             (if_
                               (reference "a")
                               (expression
                                 (Just (assignment "b" (constant 1)))
                               )
                               Nothing
                             )
                           , Statement (return_ (reference "b"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Declaration (Decl "b" (Just (constant 0)))
                           , Statement
                             (if_
                               (reference "a")
                               (expression
                                 (Just (assignment "a" (constant 2)))
                               )
                               (Just
                                 (expression
                                   (Just (assignment "a" (constant 3)))
                                 )
                               )
                             )
                           , Statement
                             (if_
                               (reference "b")
                               (expression
                                 (Just (assignment "b" (constant 4)))
                               )
                               (Just
                                 (expression
                                   (Just (assignment "b" (constant 5)))
                                 )
                               )
                             )
                           , Statement
                             (return_
                               (binary AST.Addition
                                       (reference "a")
                                       (reference "b")
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
                           [ Declaration (Decl "a" (Just (constant 1)))
                           , Statement
                             (compound
                               [Declaration (Decl "a" (Just (constant 2)))]
                             )
                           , Statement
                             (compound [Statement (return_ (reference "a"))])
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (compound
                               [ Declaration (Decl "b" (Just (constant 1)))
                               , Statement
                                 (expression
                                   (Just (assignment "a" (reference "b")))
                                 )
                               ]
                             )
                           , Statement
                             (compound
                               [ Declaration (Decl "b" (Just (constant 2)))
                               , Statement
                                 (expression
                                   (Just
                                     (assignment
                                       "a"
                                       (binary AST.Addition
                                               (reference "a")
                                               (reference "b")
                                       )
                                     )
                                   )
                                 )
                               ]
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "i" (Just (constant 0)))
                           , Statement
                             (compound
                               [Declaration (Decl "a" (Just (constant 2)))]
                             )
                           , Declaration (Decl "b" (Just (constant 3)))
                           , Statement (return_ (reference "b"))
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
                               (if_
                                 (constant 5)
                                 (compound
                                   [ Declaration (Decl "i" (Just (constant 0)))
                                   , Statement (return_ (reference "i"))
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
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement
                             (compound
                               [ Statement
                                 (expression
                                   (Just (assignment "a" (constant 3)))
                                 )
                               , Declaration (Decl "a" (Just (constant 0)))
                               ]
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Statement
                             (if_
                               (binary AST.LessThan (reference "a") (constant 3)
                               )
                               (compound
                                 [ Statement
                                   (compound
                                     [ Declaration
                                       (Decl "a" (Just (constant 3)))
                                     , Statement (return_ (reference "a"))
                                     ]
                                   )
                                 , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 0)))
                           , Statement
                             (if_
                               (reference "a")
                               (compound
                                 [ Declaration (Decl "b" (Just (constant 2)))
                                 , Statement (return_ (reference "b"))
                                 ]
                               )
                               (Just
                                 (compound
                                   [ Declaration (Decl "c" (Just (constant 3)))
                                   , Statement
                                     (if_
                                       (binary AST.LessThan
                                               (reference "a")
                                               (reference "c")
                                       )
                                       (compound
                                         [Statement (return_ (constant 4))]
                                       )
                                       (Just
                                         (compound
                                           [Statement (return_ (constant 5))]
                                         )
                                       )
                                     )
                                   ]
                                 )
                               )
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "a" (Just (constant 2)))
                           , Declaration (Decl "b" (Just (constant 3)))
                           , Statement
                             (compound
                               [ Declaration (Decl "a" (Just (constant 1)))
                               , Statement
                                 (expression
                                   (Just
                                     (assignment
                                       "b"
                                       (binary AST.Addition
                                               (reference "b")
                                               (reference "a")
                                       )
                                     )
                                   )
                                 )
                               ]
                             )
                           , Statement (return_ (reference "b"))
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
                               (compound
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
                             (compound
                               [Declaration (Decl "a" (Just (constant 2)))]
                             )
                           , Statement (return_ (reference "a"))
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
                           [ Declaration (Decl "sum" (Just (constant 0)))
                           , Statement
                             (forDecl
                               (Decl "i" (Just (constant 0)))
                               (binary AST.LessThan
                                       (reference "i")
                                       (constant 10)
                               )
                               (Just
                                 (assignment
                                   "i"
                                   (binary AST.Addition
                                           (reference "i")
                                           (constant 1)
                                   )
                                 )
                               )
                               (compound
                                 [ Statement
                                   (expression
                                     (Just
                                       (assignment
                                         "sum"
                                         (binary AST.Addition
                                                 (reference "sum")
                                                 (reference "i")
                                         )
                                       )
                                     )
                                   )
                                 , Statement
                                   (if_
                                     (binary AST.GreaterThan
                                             (reference "sum")
                                             (constant 10)
                                     )
                                     AST.break
                                     Nothing
                                   )
                                 ]
                               )
                             )
                           , Statement (return_ (reference "sum"))
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
                           [ Declaration (Decl "sum" (Just (constant 0)))
                           , Statement
                             (forDecl
                               (Decl "i" (Just (constant 0)))
                               (binary AST.LessThan
                                       (reference "i")
                                       (constant 10)
                               )
                               (Just
                                 (assignment
                                   "i"
                                   (binary AST.Addition
                                           (reference "i")
                                           (constant 1)
                                   )
                                 )
                               )
                               (compound
                                 [ Statement
                                   (if_
                                     (binary Modulo
                                             (reference "sum")
                                             (constant 2)
                                     )
                                     continue
                                     Nothing
                                   )
                                 , Statement
                                   (expression
                                     (Just
                                       (assignment
                                         "sum"
                                         (binary AST.Addition
                                                 (reference "sum")
                                                 (reference "i")
                                         )
                                       )
                                     )
                                   )
                                 ]
                               )
                             )
                           , Statement (return_ (reference "sum"))
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
                             (return_
                               (binary AST.Addition
                                       (reference "a")
                                       (reference "b")
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
                               (funCall
                                 "add"
                                 [ binary AST.Addition (constant 1) (constant 2)
                                 , constant 4
                                 ]
                               )
                             )
                           )
                         , Statement
                           (return_
                             (binary AST.Addition
                                     (reference "sum")
                                     (reference "sum")
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
                             (if_
                               (binary
                                 AST.LogicalOr
                                 (binary AST.Equality
                                         (reference "n")
                                         (constant 0)
                                 )
                                 (binary AST.Equality
                                         (reference "n")
                                         (constant 1)
                                 )
                               )
                               (compound [Statement (return_ (reference "n"))])
                               (Just
                                 (compound
                                   [ Statement
                                       (return_
                                         (binary
                                           AST.Addition
                                           (funCall
                                             "fib"
                                             [ (binary Subtraction
                                                       (reference "n")
                                                       (constant 1)
                                               )
                                             ]
                                           )
                                           (funCall
                                             "fib"
                                             [ (binary Subtraction
                                                       (reference "n")
                                                       (constant 2)
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
                         [ Declaration (Decl "n" (Just (constant 5)))
                         , (Statement (return_ (funCall "fib" [(reference "n")]))
                           )
                         ]
                       )
                     ]
                   )
                 )

    it "should parse tokens from forward_decl.c"
      $          parseTokens
                   [ KWInt
                   , Identifier "foo"
                   , OpenParen
                   , CloseParen
                   , Semicolon
                   , KWInt
                   , Identifier "main"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Identifier "foo"
                   , OpenParen
                   , CloseParen
                   , Semicolon
                   , CloseBrace
                   , KWInt
                   , Identifier "foo"
                   , OpenParen
                   , CloseParen
                   , OpenBrace
                   , KWReturn
                   , Integer 3
                   , Semicolon
                   , CloseBrace
                   ]
      `shouldBe` Right
                   (Program
                     [ Function "foo" [] Nothing
                     , Function
                       "main"
                       []
                       (Just [(Statement (return_ (funCall "foo" [])))])
                     , Function "foo"
                                []
                                (Just [(Statement (return_ (constant 3)))])
                     ]
                   )


