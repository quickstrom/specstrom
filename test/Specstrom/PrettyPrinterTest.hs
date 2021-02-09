{-# LANGUAGE ScopedTypeVariables #-}
module Specstrom.PrettyPrinterTest where

import Hedgehog (failure, property, Property, forAll, (===))
import qualified Specstrom.Gen as Gen
import Specstrom.PrettyPrinter (prettyEvalError, prettyParseError, prettyLexerError, prettyValue)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc (unAnnotate, Doc, defaultLayoutOptions, layoutPretty)
import Data.Bifunctor (Bifunctor(first))
import Specstrom.Lexer (lexer)
import Specstrom.Parser (builtIns, parseBindingBody)
import Specstrom.Evaluator (Value, initialEnv, evaluate)
import Data.Text (Text)
import Hedgehog (annotate)
import qualified Data.Text as Text

hprop_prettyprint_eval_roundtrip :: Property
hprop_prettyprint_eval_roundtrip = property $ do
  v <- forAll Gen.value
  let t = ppValue v
  annotate (Text.unpack t)
  case parseEval t of
      Left errDoc -> annotate (Text.unpack (renderStrict (layoutPretty defaultLayoutOptions errDoc))) >> failure
      Right v' -> t === ppValue v'

parseEval :: Text -> Either (Doc ()) Value
parseEval t = do
    ts <- first (unAnnotate . prettyLexerError) (lexer ("test.spec", 1, 1) t)
    (_, b) <- first (unAnnotate . prettyParseError) (parseBindingBody builtIns ts)
    first (unAnnotate . prettyEvalError) (evaluate initialEnv b)

ppValue :: Value -> Text
ppValue v = renderStrict (layoutPretty defaultLayoutOptions (prettyValue v))