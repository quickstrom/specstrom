{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Specstrom.TargetedSearchTest where

import Control.Monad.IO.Class (MonadIO)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Hedgehog (Property, checkParallel, discover, property, (===))

class StateSpace s where
  type State s :: *
  type Action s :: *

  initial :: (MonadIO m, MonadFail m) => s -> m (State s)
  actions :: (MonadIO m, MonadFail m) => s -> State s -> m [Action s]
  perform :: (MonadIO m, MonadFail m) => s -> State s -> Action s -> m (State s)

data TestStateMachine = TestStateMachine
  { initialState :: Char,
    states :: HashMap Char Int,
    transitions :: HashMap Char [Char]
  }

instance StateSpace TestStateMachine where
  type Action TestStateMachine = Int
  type State TestStateMachine = Char
  initial TestStateMachine {initialState} = pure initialState
  actions TestStateMachine {transitions} state = do
    as <- maybe (fail "Invalid state") pure (HashMap.lookup state transitions)
    pure [0 .. length as - 1]
  perform TestStateMachine {transitions} state i = do
    ts <- maybe (fail "Invalid state") pure (HashMap.lookup state transitions)
    pure (ts !! i)

prop_prettyprint_parse_roundtrip :: Property
prop_prettyprint_parse_roundtrip = property $ do
  pure ()

tests :: IO Bool
tests = checkParallel $$(discover)
