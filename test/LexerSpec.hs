module LexerSpec (spec) where

import Test.Hspec ( Spec
                  , it
                  , shouldBe)

import Lexer (lexString)
import Token (Token (..))

spec :: Spec
spec = do
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
