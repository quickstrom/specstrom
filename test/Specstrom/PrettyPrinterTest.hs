{-# LANGUAGE ScopedTypeVariables #-}

module Specstrom.PrettyPrinterTest where

import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Doc, defaultLayoutOptions, layoutPretty, unAnnotate)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Hedgehog (annotateShow, Property, annotate, failure, forAll, property, (===))
import Specstrom.Evaluator (Value, evaluate, initialEnv)
import qualified Specstrom.Gen as Gen
import Specstrom.Lexer (lexer)
import Specstrom.Parser (builtIns, parseBindingBody)
import Specstrom.PrettyPrinter (prettyEvalError, prettyLexerError, prettyParseError, prettyValue)

hprop_prettyprint_eval_roundtrip :: Property
hprop_prettyprint_eval_roundtrip = property $ do
  v <- forAll Gen.value
  let t = ppValue v
  annotate (Text.unpack t)
  case parseEval t of
    Left errDoc -> annotate (Text.unpack (renderStrict (layoutPretty defaultLayoutOptions errDoc))) >> failure
    Right v' -> do
      annotateShow v'
      t === ppValue v'

parseEval :: Text -> Either (Doc ()) Value
parseEval t = do
  ts <- first (unAnnotate . prettyLexerError) (lexer ("test.spec", 1, 1) t)
  (_, b) <- first (unAnnotate . prettyParseError) (parseBindingBody builtIns ts)
  first (unAnnotate . prettyEvalError) (evaluate initialEnv b)

ppValue :: Value -> Text
ppValue v = renderStrict (layoutPretty defaultLayoutOptions (prettyValue v))