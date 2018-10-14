{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Lexer (Token (..), lex)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = (describe "stage 1: valid"   $ for_ stage1ValidCases test) >>
        (describe "stage 1: invalid" $ for_ stage1InvalidCases test)
  where
    test Case {..} = it description $ Lexer.lex input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: [Token]
                 }

stage1ValidCases :: [Case]
stage1ValidCases = [ Case { description = "multi_digit.c"
               , input       = "int main() {\n\
                               \    return 100;\n\
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

        , Case { description = "newlines.c"
               , input       = "int\n\
                               \main\n\
                               \(\n\
                               \)\n\
                               \{\n\
                               \return\n\
                               \0\n\
                               \;\n\
                               \}"
               , expected    = [ KWInt
                               , Identifier "main"
                               , OpenParen
                               , CloseParen
                               , OpenBrace
                               , KWReturn
                               , Integer 0
                               , Semicolon
                               , CloseBrace
                               ]
               }

        , Case { description = "no_newlines.c"
               , input       = "int main(){return 0;}"
               , expected    = [ KWInt
                               , Identifier "main"
                               , OpenParen
                               , CloseParen
                               , OpenBrace
                               , KWReturn
                               , Integer 0
                               , Semicolon
                               , CloseBrace
                               ]
               }

        , Case { description = "return_0.c"
               , input       = "int main() {\n\
                               \    return 0;\n\
                               \}"
               , expected    = [ KWInt
                               , Identifier "main"
                               , OpenParen
                               , CloseParen
                               , OpenBrace
                               , KWReturn
                               , Integer 0
                               , Semicolon
                               , CloseBrace
                               ]
               }

        , Case { description = "return_2.c"
               , input       = "int main() {\n\
                               \    return 2;\n\
                               \}"
               , expected    = [ KWInt
                               , Identifier "main"
                               , OpenParen
                               , CloseParen
                               , OpenBrace
                               , KWReturn
                               , Integer 2
                               , Semicolon
                               , CloseBrace
                               ]
               }

        , Case { description = "spaces.c"
               , input       = "int   main    (  )  {   return  0 ; }"
               , expected    = [ KWInt
                               , Identifier "main"
                               , OpenParen
                               , CloseParen
                               , OpenBrace
                               , KWReturn
                               , Integer 0
                               , Semicolon
                               , CloseBrace
                               ]
               }

        ]

stage1InvalidCases :: [Case]
stage1InvalidCases = [ Case { description = "missing_paren.c"
                            , input       = "int main( {\n\
                                            \    return 0;\n\
                                            \}"
                            , expected    = [ KWInt
                                            , Identifier "main"
                                            , OpenParen
                                            , OpenBrace
                                            , KWReturn
                                            , Integer 0
                                            , Semicolon
                                            , CloseBrace
                                            ]
                            }

                     , Case { description = "missing_retval.c"
                            , input       = "int main() {\n\
                                            \    return;\n\
                                            \}"
                            , expected    = [ KWInt
                                            , Identifier "main"
                                            , OpenParen
                                            , CloseParen
                                            , OpenBrace
                                            , KWReturn
                                            , Semicolon
                                            , CloseBrace
                                            ]
                            }

                     , Case { description = "no_brace.c"
                            , input       = "int main() {\n\
                                            \    return 0;"
                            , expected    = [ KWInt
                                            , Identifier "main"
                                            , OpenParen
                                            , CloseParen
                                            , OpenBrace
                                            , KWReturn
                                            , Integer 0
                                            , Semicolon
                                            ]
                            }

                     , Case { description = "no_semicolon.c"
                            , input       = "int main {\n\
                                            \return 0\n\
                                            \}"
                            , expected    = [ KWInt
                                            , Identifier "main"
                                            , OpenBrace
                                            , KWReturn
                                            , Integer 0
                                            , CloseBrace
                                            ]
                            }

                     , Case { description = "no_space.c"
                            , input       = "int main() {\n\
                                            \    return0;\n\
                                            \}"
                            , expected    = [ KWInt
                                            , Identifier "main"
                                            , OpenParen
                                            , CloseParen
                                            , OpenBrace
                                            , Identifier "return0"
                                            , Semicolon
                                            , CloseBrace
                                            ]
                            }
                     ]
