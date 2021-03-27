{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Specstrom.Gen where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Specstrom.Checker.Protocol as Protocol
import qualified Specstrom.Dependency as Dependency
import Specstrom.Lexer (dummyPosition)
import Specstrom.Syntax (Expr (..), Lit (..), Name, Pattern, Selector (..))
import Data.Traversable (for)

name :: Gen Name
name = ("n" <>) . Text.pack . show @Int <$> Gen.integral (Range.linear 1 100)

selector :: Gen Selector
selector = Selector . ("sel-" <>) <$> Gen.text (Range.linear 1 10) Gen.alphaNum

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

-- * Dep

elementState :: Dependency.DepSchema -> Gen JSON.Value
elementState (Dependency.DepSchema fields)
  | HashMap.null fields = JSON.Bool <$> Gen.bool
  | otherwise = JSON.Object <$> traverse elementState fields

ref :: Gen Text
ref = Gen.element ["e1", "e2", "e3"]

state :: Dependency.Dep -> Gen Protocol.State
state (Dependency.Dep bySelector) =
  for bySelector $ \schema -> do
    Gen.list (Range.linear 1 10) $ do
      r <- ref
      JSON.Object es <- elementState schema
      pure (JSON.Object (es <> HashMap.singleton "ref" (JSON.String r)))
