module Test where

import AST

returnStatement = Statement . Return
expressionStatement = Statement . Expression
