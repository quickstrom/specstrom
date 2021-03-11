{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Specstrom.Gen where

import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Specstrom.Lexer (Position, dummyPosition)
import Specstrom.Syntax (Expr (..), Lit (..), Name, Pattern)

name :: Gen Name
name = ("n" <>) . Text.pack . show @Int <$> Gen.integral (Range.linear 1 100)

selector :: Gen Text
selector = ("sel-" <>) <$> Gen.text (Range.linear 1 10) Gen.alphaNum

literal :: Gen Lit
literal =
  Gen.choice
    [ IntLit <$> Gen.integral (Range.linear 0 10),
      -- FloatLit <$> Gen.double (Range.linearFrac (0) 10),
      StringLit <$> Gen.text (Range.linear 0 10) Gen.unicode,
      CharLit <$> Gen.unicode,
      SelectorLit <$> selector
    ]

-- | * Expr
literalExpr :: Gen (Expr Pattern)
literalExpr = Literal <$> pure dummyPosition <*> literal

boolExpr :: Gen (Expr Pattern)
boolExpr =
  Gen.recursive
    Gen.choice
    [ -- Var <$> position <*> name,
      -- Literal <$> position <*> literal
      App . App (Var dummyPosition "_==_") <$> literalExpr <*> literalExpr,
      App . App (Var dummyPosition "_!=_") <$> literalExpr <*> literalExpr
    ]
    [ Gen.subterm boolExpr (App (Var dummyPosition "not_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var dummyPosition "_&&_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var dummyPosition "_||_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var dummyPosition "_==>_"))
    ]

expr :: Gen (Expr Pattern)
expr =
  Gen.recursive
    Gen.choice
    [ -- Var <$> position <*> name,
      -- Literal <$> position <*> literal
      Gen.subterm boolExpr (App (Var dummyPosition "always_")),
      Gen.subterm boolExpr (App (Var dummyPosition "next_")),
      Gen.subterm boolExpr (App (Var dummyPosition "eventually_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var dummyPosition "_until_"))
    ]
    [ Gen.subterm expr (App (Var dummyPosition "not_")),
      Gen.subterm2 expr expr (App . App (Var dummyPosition "_&&_")),
      Gen.subterm2 expr expr (App . App (Var dummyPosition "_||_")),
      Gen.subterm2 expr expr (App . App (Var dummyPosition "_==>_"))
    ]
