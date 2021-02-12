{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Specstrom.Gen where

import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Specstrom.Evaluator (Accessor, Formula (..), FormulaExpr (..), IValue (..), Op (..), Value (..))
import Specstrom.Lexer (Position)
import Specstrom.Parser (Expr (..), Lit (..), Name)

name :: Gen Name
name = ("n" <>) . Text.pack . show @Int <$> Gen.integral (Range.linear 1 100)

selector :: Gen Text
selector = ("sel-" <>) <$> Gen.text (Range.linear 1 10) Gen.alphaNum

position :: Gen Position
position =
  ("test.spec",,)
    <$> Gen.integral (Range.linear 1 10)
    <*> Gen.integral (Range.linear 1 10)

pos1 :: Position
pos1 = ("test.spec", 1, 1)

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
literalExpr :: Gen Expr
literalExpr = Literal <$> position <*> literal

boolExpr :: Gen Expr
boolExpr =
  Gen.recursive
    Gen.choice
    [ -- Var <$> position <*> name,
      -- Literal <$> position <*> literal
      App . App (Var pos1 "_==_") <$> literalExpr <*> literalExpr,
      App . App (Var pos1 "_!=_") <$> literalExpr <*> literalExpr
    ]
    [ Gen.subterm boolExpr (App (Var pos1 "not_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var pos1 "_&&_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var pos1 "_||_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var pos1 "_==>_"))
    ]

expr :: Gen Expr
expr =
  Gen.recursive
    Gen.choice
    [ -- Var <$> position <*> name,
      -- Literal <$> position <*> literal
      Gen.subterm boolExpr (App (Var pos1 "always_")),
      Gen.subterm boolExpr (App (Var pos1 "next_")),
      Gen.subterm boolExpr (App (Var pos1 "eventually_")),
      Gen.subterm2 boolExpr boolExpr (App . App (Var pos1 "_until_"))
    ]
    [ Gen.subterm expr (App (Var pos1 "not_")),
      Gen.subterm2 expr expr (App . App (Var pos1 "_&&_")),
      Gen.subterm2 expr expr (App . App (Var pos1 "_||_")),
      Gen.subterm2 expr expr (App . App (Var pos1 "_==>_"))
    ]

-- | * Formula and FormulaExpr
unaryBoolFormulaExprOp :: Gen Op
unaryBoolFormulaExprOp =
  Gen.element [NotOp]

binaryBoolFormulaExprOp :: Gen Op
binaryBoolFormulaExprOp =
  Gen.element
    [ Equals,
      NotEquals,
      Less,
      LessEq,
      Greater,
      GreaterEq,
      AndOp,
      OrOp,
      ImpliesOp
    ]

unaryFormulaOp :: Gen Op
unaryFormulaOp =
  Gen.element
    [ NotOp,
      AlwaysOp,
      EventuallyOp,
      NextOp
    ]

binaryFormulaExprOp :: Gen Op
binaryFormulaExprOp =
  Gen.element
    [ Equals,
      NotEquals,
      Plus,
      Times,
      Divide,
      Modulo,
      Less,
      LessEq,
      Greater,
      GreaterEq,
      AndOp,
      OrOp
      -- ImpliesOp,
      -- UntilOp
    ]

boolFormulaExpr :: Gen (FormulaExpr Accessor)
boolFormulaExpr =
  Gen.recursive
    Gen.choice
    [ pure (Constant (BoolVal True)),
      pure (Constant (BoolVal False))
      -- , FreezeVar <$> name <*> position
    ]
    [ Gen.subtermM boolFormulaExpr (\e -> Op <$> unaryBoolFormulaExprOp <*> pure [e]),
      Gen.subtermM2 boolFormulaExpr boolFormulaExpr (\e1 e2 -> Op <$> binaryBoolFormulaExprOp <*> pure [e1, e2]),
      Gen.subtermM boolFormulaExpr (\e -> LocExpr <$> name <*> position <*> pure e)
    ]

formula :: Gen (Formula Accessor)
formula =
  Gen.recursive
    Gen.choice
    [Atomic <$> boolFormulaExpr]
    [ Gen.subterm formula Not,
      Gen.subterm formula Always,
      Gen.subterm formula Eventually,
      Gen.subterm formula Next,
      Gen.subterm2 formula formula Until,
      Gen.subterm2 formula formula And,
      Gen.subterm2 formula formula Or,
      Gen.subterm2 formula formula Implies,
      Gen.subtermM formula (\f -> LocFormula <$> name <*> position <*> pure f)
      -- FreezeIn Position Name (FormulaExpr a) (Formula a)
    ]

ivalue :: Gen IValue
ivalue =
  Gen.choice
    [ LitVal <$> literal,
      pure (BoolVal True),
      pure (BoolVal False),
      pure Null
    ]

value :: Gen Value
value =
  Gen.choice
    [ Independent <$> ivalue,
      Formula mempty <$> formula
      -- StateDependent (S.Set Accessor) (FormulaExpr Accessor)
      -- Closure (Maybe (Name, Position)) Env [(Name, Position)] Body
      -- PartialOp Op [Value]
    ]