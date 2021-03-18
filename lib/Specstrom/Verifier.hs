{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Specstrom.Verifier where

import Control.Monad (foldM)
import Data.Foldable (Foldable (fold))
import qualified Data.Map as M
import Specstrom.Evaluator (force, evaluateBind, Env, basicEnv, State, Value (..), step, stop)
import Specstrom.Syntax (TopLevel(..))
import qualified Data.Text as Text
import Data.Traversable (for)

type Trace = [State]

data Result = Definitely Bool | Probably Bool
  deriving (Eq, Show)

check :: [TopLevel] -> IO [Result]
check ts = do
  env <- foldM toEnv basicEnv ts
  fold <$> traverse (checkProps env) ts
  where
    toEnv :: Env -> TopLevel -> IO Env
    toEnv env = \case
      Binding b -> evaluateBind env b
      Imported _ ts' -> (env <>) <$> foldM toEnv env ts'
      Properties {} -> pure env

checkProps :: Env -> TopLevel -> IO [Result]
checkProps initialEnv = \case
  Properties _ _propGlob _ _ -> do
    let props = M.toList (M.filterWithKey (\k _ -> "prop" `Text.isPrefixOf` k) initialEnv)
    for props $ \(name, val) -> do
      putStrLn ("Checking property: " <> Text.unpack name)
      loop initialEnv mempty val
  _ -> pure []
  where
    loop env currentState val = force currentState val >>= \case
      Trivial -> pure (Definitely True)
      Absurd -> pure (Definitely False)
      Residual r ->
        case stop r of
          Just v -> pure (Probably v)
          Nothing -> do
            putStrLn "Getting another state to continue with residual formula"
            let nextState = mempty -- TODO: get state
            loop env nextState =<< step r currentState
      v -> do
        putStrLn ("Unexpected value: " <> show v)
        pure (Probably False)
