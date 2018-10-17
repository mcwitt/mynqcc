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
                \return 0\n\
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
                  \return !12;\n\
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
                \return ~0;\n\
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
                \return -5;\n\
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
                \return !-3;\n\
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
                \return -~0;\n\
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
                \return !5;\n\
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
                \return !0;\n\
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
                \return !;\n\
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
                \return !5\n\
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
                \return !~;\n\
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
                \return 4-;\n\
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
