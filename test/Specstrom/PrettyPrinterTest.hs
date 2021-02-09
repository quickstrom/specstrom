{-# LANGUAGE ScopedTypeVariables #-}


module Specstrom.PrettyPrinterTest where

import Data.Bifunctor (Bifunctor (first))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Doc, defaultLayoutOptions, layoutPretty, unAnnotate)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Hedgehog (MonadTest, annotateShow, Property, annotate, failure, forAll, property, (===))
import Specstrom.Evaluator (Value, evaluate, initialEnv)
import qualified Specstrom.Gen as Gen
import Specstrom.Lexer (lexer)
import Specstrom.Parser (Body(Done),  builtIns, parseBindingBody)
import Specstrom.PrettyPrinter (prettyEvalError, prettyLexerError, prettyParseError, prettyValue)
import GHC.Stack (withFrozenCallStack, HasCallStack)

hprop_prettyprint_eval_roundtrip :: Property
hprop_prettyprint_eval_roundtrip = property $ do
  e <- forAll Gen.expr
  v <- requireRight (eval (Done e))
  let t = ppValue v
  annotate (Text.unpack t)
  e' <- requireRight (parse t)
  annotateShow e'
  v' <- requireRight (eval e')
  annotateShow v'
  t === ppValue v'

parse :: Text -> Either (Doc ()) Body
parse t = do
  ts <- first (unAnnotate . prettyLexerError) (lexer ("test.spec", 1, 1) t)
  (_, b) <- first (unAnnotate . prettyParseError) (parseBindingBody builtIns ts)
  pure b

eval :: Body -> Either (Doc ()) Value
eval b = first (unAnnotate . prettyEvalError) (evaluate initialEnv b)

ppValue :: Value -> Text
ppValue v = renderStrict (layoutPretty defaultLayoutOptions (prettyValue v))

requireRight :: (HasCallStack, MonadTest m) => Either (Doc ()) a -> m a
requireRight ma = withFrozenCallStack $ case ma of
  Left doc -> annotate (Text.unpack (renderStrict (layoutPretty defaultLayoutOptions doc))) >> failure
  Right a -> pure a
