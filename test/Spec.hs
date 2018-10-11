{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "lex" $ for_ cases test
  where
    test Case {..} = it description $ Lib.lex input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: [Token]
                 }

cases :: [Case]
cases = [ Case { description = "basic"
               , input       = "int main() {\
                               \    return 100;\
                               \}"
               , expected    = [ KWInt
                               , Identifier "main"
                               , OpenParen
                               , CloseParen
                               , OpenBrace
                               , KWReturn
                               , Integer 100
                               , Semicolon
                               , CloseBrace
                               ]
               }
        ]
