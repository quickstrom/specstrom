{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Specstrom.PrettyPrinterTest where

import Control.Monad.Trans.Except (except, withExceptT, runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Doc, defaultLayoutOptions, layoutPretty, unAnnotate)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (evalIO, MonadTest, Property, annotate, checkParallel, discover, failure, forAll, property, (===))
import qualified Specstrom.Gen as Gen
import Specstrom.Lexer (dummyPosition, lexer)
import Specstrom.Parser (builtIns, parseTopLevel)
import Specstrom.PrettyPrinter (prettyAll, prettyLexerError, prettyParseError)
import Specstrom.Syntax

prop_prettyprint_parse_roundtrip :: Property
prop_prettyprint_parse_roundtrip = property $ do
  e <- forAll Gen.expr
  let t = [Binding (Bind (Direct (VarP "test" dummyPosition)) (Done e))]
      pp = ppTopLevel t
      clearPos = map (mapPosition (const dummyPosition))
  annotate (Text.unpack pp)
  t' <- requireRight =<< evalIO (parse pp)
  t === clearPos t'

parse :: Text -> IO (Either (Doc ()) [TopLevel])
parse t = runExceptT $ do
  ts <- withExceptT (unAnnotate . prettyLexerError) (except (lexer ("test.spec", 1, 1) t))
  (_, b) <- withExceptT (unAnnotate . prettyParseError) (parseTopLevel [] builtIns ts)
  pure b

ppTopLevel :: [TopLevel] -> Text
ppTopLevel t = renderStrict (layoutPretty defaultLayoutOptions (prettyAll t))

requireRight :: (HasCallStack, MonadTest m) => Either (Doc ()) a -> m a
requireRight ma = withFrozenCallStack $ case ma of
  Left doc -> annotate (Text.unpack (renderStrict (layoutPretty defaultLayoutOptions doc))) >> failure
  Right a -> pure a

tests :: IO Bool
tests = checkParallel $$(discover)
