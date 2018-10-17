module CodegenSpec (spec) where

import Test.Hspec        ( Spec
                         , describe
                         , it
                         , shouldBe
                         )

import Codegen
import Parser
import AST

spec :: Spec
spec = do
  describe "Stage 1" $ do

    it "should generate code for multi_digit.c" $ do
      generate (Program (
                   Function "main" (
                       Return (Constant 100))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $100, %eax\n\
        \ret"

  describe "Stage 2" $ do

    it "should generate code for bitwise.c" $ do
      generate (Program (
                   Function "main" (
                       Return (
                           AST.LogicalNegation (Constant 12)))))
        `shouldBe`
        ".globl main\n\
        \main:\n\
        \movl $12, %eax\n\
        \cmpl $0, %eax\n\
        \movl $0, %eax\n\
        \sete %al\n\
        \ret"
