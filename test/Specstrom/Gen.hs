{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Specstrom.Gen where

import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Specstrom.Checker.Protocol as Protocol
import qualified Specstrom.Dependency as Dependency
import Specstrom.Lexer (dummyPosition)
import Specstrom.Syntax (Expr (..), Lit (..), Name, Pattern, Selector (..))

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
literalExpr = Literal dummyPosition <$> literal

intExpr :: Gen (Expr Pattern)
intExpr =
  Gen.recursive
    Gen.choice
    [ Literal dummyPosition . IntLit <$> Gen.integral (Range.linear 0 10)
    ]
    [ Gen.subterm2 intExpr intExpr (App . App (Var dummyPosition "_+_")),
      Gen.subterm2 intExpr intExpr (App . App (Var dummyPosition "_-_"))
    ]

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
      Gen.subterm boolExpr (App (Var dummyPosition "next_")),
      -- Gen.subterm boolExpr (App (Var dummyPosition "eventually_")),
      Gen.subterm2 intExpr boolExpr (App . App (Var dummyPosition "always{_}_")),
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

state :: Dependency.Dep -> Gen Protocol.State
state (Dependency.Dep bySelector) =
  flip HashMap.traverseWithKey bySelector $ \(Selector s) schema -> do
    elements <- Gen.list (Range.linear 1 10) (elementState schema)
    pure
      ( JSON.Array
          ( Vector.fromList
              [ JSON.Object (element <> HashMap.singleton "ref" (JSON.String (s <> "-" <> Text.pack (show i))))
                | (i, JSON.Object element) <- zip [0 :: Int ..] elements
              ]
          )
      )
