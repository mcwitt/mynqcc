module LexerSpec (spec) where

import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe)

import Lexer (Token (..), lex)

spec :: Spec
spec = do
  it "should lex multi_digit.c" $ do
    Lexer.lex "int main() {\n\
              \    return 100;\n\
              \}"
      `shouldBe`
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

  it "should lex newlines.c" $ do
    Lexer.lex "int\n\
              \main\n\
              \(\n\
              \)\n\
              \{\n\
              \return\n\
              \0\n\
              \;\n\
              \}"
      `shouldBe`
      [ KWInt
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
    Lexer.lex "int main(){return 0;}" `shouldBe`
      [ KWInt
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
    Lexer.lex "int main() {\n\
              \    return 0;\n\
              \}"
      `shouldBe`
      [ KWInt
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
    Lexer.lex "int main() {\n\
              \    return 2;\n\
              \}"
      `shouldBe`
      [ KWInt
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
    Lexer.lex "int   main    (  )  {   return  0 ; }"
      `shouldBe`
      [ KWInt
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
    Lexer.lex "int main( {\n\
              \    return 0;\n\
              \}"
      `shouldBe`
      [ KWInt
      , Identifier "main"
      , OpenParen
      , OpenBrace
      , KWReturn
      , Integer 0
      , Semicolon
      , CloseBrace
      ]

  it "should lex missing_retval.c" $ do
    Lexer.lex "int main() {\n\
              \    return;\n\
              \}"
      `shouldBe`
      [ KWInt
      , Identifier "main"
      , OpenParen
      , CloseParen
      , OpenBrace
      , KWReturn
      , Semicolon
      , CloseBrace
      ]

  it "should lex no_brace.c" $ do
    Lexer.lex "int main() {\n\
              \    return 0;"
      `shouldBe`
      [ KWInt
      , Identifier "main"
      , OpenParen
      , CloseParen
      , OpenBrace
      , KWReturn
      , Integer 0
      , Semicolon
      ]

  it "should lex no_semicolon.c" $ do
    Lexer.lex "int main {\n\
              \return 0\n\
              \}"
      `shouldBe`
      [ KWInt
      , Identifier "main"
      , OpenBrace
      , KWReturn
      , Integer 0
      , CloseBrace
      ]

  it "should lex no_space.c" $ do
    Lexer.lex "int main() {\n\
              \    return0;\n\
              \}"
      `shouldBe`
      [ KWInt
      , Identifier "main"
      , OpenParen
      , CloseParen
      , OpenBrace
      , Identifier "return0"
      , Semicolon
      , CloseBrace
      ]
