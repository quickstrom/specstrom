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
import Specstrom.Parser (Lit (..), Name)

name :: Gen Name
name = ("n" <>) . Text.pack . show @Int <$> Gen.integral (Range.linear 1 100)

selector :: Gen Text
selector = ("sel-" <>) <$> Gen.text (Range.linear 1 10) Gen.alphaNum

position :: Gen Position
position =
  ("testfile",,)
    <$> Gen.integral (Range.linear 1 10)
    <*> Gen.integral (Range.linear 1 10)

unaryOp :: Gen Op
unaryOp =
  Gen.element
    [ NotOp,
      AlwaysOp,
      EventuallyOp,
      NextOp
    ]

binaryOp :: Gen Op
binaryOp =
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
      OrOp,
      ImpliesOp,
      UntilOp
    ]

formulaExpr :: Gen (FormulaExpr Accessor)
formulaExpr =
  Gen.recursive
    Gen.choice
    [ Accessor <$> selector,
      Constant <$> ivalue,
      Op MkAccessor . pure . Constant . LitVal . SelectorLit <$> selector
      -- , FreezeVar <$> name <*> position
    ]
    [ Gen.subtermM formulaExpr (\e -> Op <$> unaryOp <*> pure [e]),
      Gen.subtermM2 formulaExpr formulaExpr (\e1 e2 -> Op <$> binaryOp <*> pure [e1, e2]),
      Gen.subtermM formulaExpr (\e -> LocExpr <$> name <*> position <*> pure e)
    ]

formula :: Gen (Formula Accessor)
formula =
  Gen.recursive
    Gen.choice
    [Atomic <$> formulaExpr]
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

literal :: Gen Lit
literal =
  Gen.choice
    [ IntLit <$> Gen.integral (Range.linear (0) 10),
      FloatLit <$> Gen.double (Range.linearFrac (0) 10),
      StringLit <$> Gen.text (Range.linear 0 10) Gen.unicode,
      CharLit <$> Gen.unicode,
      SelectorLit <$> selector
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