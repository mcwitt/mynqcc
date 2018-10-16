module CodegenSpec (spec) where

import Test.Hspec        ( Spec
                         , describe
                         , it
                         , shouldBe
                         )

import Codegen (generate)
import Parser (parse_)

import AST ( Program (Program)
           , Function (Function)
           , Statement (Return)
           , Expression (Constant)
           )

spec :: Spec
spec = do
  it "should generate code for multi_digit.c" $ do
    generate (Program (Function "main" (Return (Constant 100)))) `shouldBe`
      ".globl main\n\
      \main:\n\
      \movl $100, %eax\n\
      \ret"
