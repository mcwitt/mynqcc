module LexerSpec (spec) where

import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe)

import Lexer ( lexString)
import Token ( Token (..))

spec :: Spec
spec = do

  describe "Stage 1" $ do

    it "should lex multi_digit.c" $ do
      lexString "int main() {\n\
                \    return 100;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 100
              , Semicolon
              , CloseBrace
              ]

    it "should lex newlines.c" $ do
      lexString "int\n\
                \main\n\
                \(\n\
                \)\n\
                \{\n\
                \return\n\
                \0\n\
                \;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 0
              , Semicolon
              , CloseBrace
              ]

    it "should lex no_newlines.c" $ do
      lexString "int main(){return 0;}" `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 0
              , Semicolon
              , CloseBrace
              ]

    it "should lex return_0.c" $ do
      lexString "int main() {\n\
                \    return 0;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 0
              , Semicolon
              , CloseBrace
              ]

    it "should lex return_2.c" $ do
      lexString "int main() {\n\
                \    return 2;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 2
              , Semicolon
              , CloseBrace
              ]

    it "should lex spaces.c" $ do
      lexString "int   main    (  )  {   return  0 ; }"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 0
              , Semicolon
              , CloseBrace
              ]

    it "should lex missing_paren.c" $ do
      lexString "int main( {\n\
                \    return 0;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , OpenBrace
              , KWReturn
              , Integer 0
              , Semicolon
              , CloseBrace
              ]

    it "should lex missing_retval.c" $ do
      lexString "int main() {\n\
                \    return;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Semicolon
              , CloseBrace
              ]

    it "should lex no_brace.c" $ do
      lexString "int main() {\n\
                \    return 0;"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 0
              , Semicolon
              ]

    it "should lex no_semicolon.c" $ do
      lexString "int main {\n\
                \    return 0\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenBrace
              , KWReturn
              , Integer 0
              , CloseBrace
              ]

    it "should lex no_space.c" $ do
      lexString "int main() {\n\
                \    return0;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , Identifier "return0"
              , Semicolon
              , CloseBrace
              ]

  describe "Stage 2" $ do

    it "should lex bitwise.c" $ do
        lexString "int main() {\n\
                  \    return !12;\n\
                  \}"
          `shouldBe`
          Right [ KWInt
                , Identifier "main"
                , OpenParen
                , CloseParen
                , OpenBrace
                , KWReturn
                , LogicalNegation
                , Integer 12
                , Semicolon
                , CloseBrace]

    it "should lex bitwise_zero.c" $ do
      lexString "int main() {\n\
                \    return ~0;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , BitwiseComplement
              , Integer 0
              , Semicolon
              , CloseBrace]

    it "should lex neg.c" $ do
      lexString "int main() {\n\
                \    return -5;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Negation
              , Integer 5
              , Semicolon
              , CloseBrace]

    it "should lex nested_ops.c" $ do
      lexString "int main() {\n\
                \    return !-3;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , LogicalNegation
              , Negation
              , Integer 3
              , Semicolon
              , CloseBrace]

    it "should lex nested_ops_2.c" $ do
      lexString "int main() {\n\
                \    return -~0;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Negation
              , BitwiseComplement
              , Integer 0
              , Semicolon
              , CloseBrace]

    it "should lex not_5.c" $ do
      lexString "int main() {\n\
                \    return !5;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , LogicalNegation
              , Integer 5
              , Semicolon
              , CloseBrace]

    it "should lex not_0.c" $ do
      lexString "int main() {\n\
                \    return !0;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , LogicalNegation
              , Integer 0
              , Semicolon
              , CloseBrace]

    it "should lex missing_const.c" $ do
      lexString "int main() {\n\
                \    return !;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , LogicalNegation
              , Semicolon
              , CloseBrace]

    it "should lex missing_semicolon.c" $ do
      lexString "int main() {\n\
                \    return !5\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , LogicalNegation
              , Integer 5
              , CloseBrace]

    it "should lex nested_missing_const.c" $ do
      lexString "int main() {\n\
                \    return !~;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , LogicalNegation
              , BitwiseComplement
              , Semicolon
              , CloseBrace]

    it "should lex wrong_order.c" $ do
      lexString "int main() {\n\
                \    return 4-;\n\
                \}"
        `shouldBe`
        Right [ KWInt
              , Identifier "main"
              , OpenParen
              , CloseParen
              , OpenBrace
              , KWReturn
              , Integer 4
              , Negation
              , Semicolon
              , CloseBrace]

  describe "Stage 3" $ do

    it "should lex add.c" $ do
      lexString "int main() {\n\
                \    return 1 + 2;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 1
            , Addition
            , Integer 2
            , Semicolon
            , CloseBrace]

    it "should lex associativity.c" $ do
      lexString "int main() {\n\
                \    return 1 - 2 - 3;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 1
            , Negation
            , Integer 2
            , Negation
            , Integer 3
            , Semicolon
            , CloseBrace]

    it "should lex associativity_2.c" $ do
      lexString "int main() {\n\
                \    return 6 / 3 / 2;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 6
            , Division
            , Integer 3
            , Division
            , Integer 2
            , Semicolon
            , CloseBrace]

    it "should lex div.c" $ do
      lexString "int main() {\n\
                \    return 4 / 2;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 4
            , Division
            , Integer 2
            , Semicolon
            , CloseBrace]

    it "should lex mult.c" $ do
      lexString "int main() {\n\
                \    return 2 * 3;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 2
            , Multiplication
            , Integer 3
            , Semicolon
            , CloseBrace]

    it "should lex parens.c" $ do
      lexString "int main() {\n\
                \    return 2 * (3 + 4);\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 2
            , Multiplication
            , OpenParen
            , Integer 3
            , Addition
            , Integer 4
            , CloseParen
            , Semicolon
            , CloseBrace]

    it "should lex precedence.c" $ do
      lexString "int main() {\n\
                \    return 2 + 3 * 4;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 2
            , Addition
            , Integer 3
            , Multiplication
            , Integer 4
            , Semicolon
            , CloseBrace]

    it "should lex sub.c" $ do
      lexString "int main() {\n\
                \    return 1 - 2;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 1
            , Negation
            , Integer 2
            , Semicolon
            , CloseBrace]

    it "should lex sub_neg.c" $ do
      lexString "int main() {\n\
                \    return 2- -1;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 2
            , Negation
            , Negation
            , Integer 1
            , Semicolon
            , CloseBrace]

    it "should lex unop_add.c" $ do
      lexString "int main() {\n\
                \    return ~2 + 3;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , BitwiseComplement
            , Integer 2
            , Addition
            , Integer 3
            , Semicolon
            , CloseBrace]

    it "should lex unop_parens.c" $ do
      lexString "int main() {\n\
                \    return ~(1 + 1);\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , BitwiseComplement
            , OpenParen
            , Integer 1
            , Addition
            , Integer 1
            , CloseParen
            , Semicolon
            , CloseBrace]

    it "should lex malformed_paren.c" $ do
      lexString "int main() {\n\
                \    return 2 (- 3);\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 2
            , OpenParen
            , Negation
            , Integer 3
            , CloseParen
            , Semicolon
            , CloseBrace]

    it "should lex missing_first.c" $ do
      lexString "int main() {\n\
                \    return /3;\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Division
            , Integer 3
            , Semicolon
            , CloseBrace]

    it "should lex no_semicolon.c" $ do
      lexString "int main() {\n\
                \    return 2*2\n\
                \}"
      `shouldBe`
      Right [ KWInt
            , Identifier "main"
            , OpenParen
            , CloseParen
            , OpenBrace
            , KWReturn
            , Integer 2
            , Multiplication
            , Integer 2
            , CloseBrace]
