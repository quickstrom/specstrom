{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Specstrom.Verifier where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Numeric.Natural
import Pipes (Producer, await, yield, (>->))
import qualified Data.Map as M
import Pipes.Parse (Parser, draw, evalStateT, runStateT)
import Pipes.Prelude (stdinLn)
import Specstrom.Evaluator (force, evaluate, evaluateBind, Env, basicEnv, State, Value (..))
import Specstrom.Syntax (TopLevel(..),  Expr, Pattern)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin)
import Data.Foldable (Foldable(fold, foldl'))
import Control.Monad (foldM)
import qualified Data.Text as Text
import Debug.Trace (traceShow)

type Trace = [State]

data Result = Definitely Bool | Probably Bool
  deriving (Eq, Show)


check :: [TopLevel] -> IO [Result]
check ts =  do
  env <- foldM toEnv basicEnv ts
  fold <$> traverse (checkProps env) ts

  where
    toEnv :: Env -> TopLevel -> IO Env
    toEnv env = \case
      Binding b -> evaluateBind env b
      Imported _ ts' -> (env <>) <$> foldM toEnv env ts'
      Properties{} -> pure env
    checkProps :: Env -> TopLevel -> IO [Result]
    checkProps env = \case
      Properties _ _propGlob _ _ -> do
        let props = M.elems (M.filterWithKey (\k _ -> "prop" `Text.isPrefixOf` k) env)
            check s env val =
              force s val >>= \case
                Trivial -> pure (Definitely True)
                Absurd -> pure (Definitely False)
                v -> traceShow v $ pure (Probably False)
        traverse (check mempty env) props
      _ -> pure []

{-
-- | Steps the formula through the trace as long as it requires
-- more states (i.e. contains 'DNext' terms).
stepRequiredList :: Expr Pattern -> Trace -> IO (Value, Trace)
stepRequiredList f [] = fail "Cannot step"
stepRequiredList f (x : xs) =
  case step f x of
    Step f' Nothing -> stepRequiredList f' xs
    Step f' (Just r) -> pure (f', r, xs)

-- | Steps the formula through the residual states (states that are
-- available but not required) and computes the final result.
stepResidualList :: Formula -> Result -> Trace -> Either CheckError Result
stepResidualList f _ (x : xs) =
  case step f x of
    Step f' Nothing -> throwError (CannotStep f')
    Step f' (Just r) -> stepResidualList f' r xs
stepResidualList _ r [] = pure r

-- | Verify a pre-collected trace with the given formula.
verifyList :: Formula -> Trace -> Either CheckError Result
verifyList f t = do
  (f', r, t') <- stepRequiredList f t
  stepResidualList f' r t'
-}

-- * Streaming with Pipes

{-

-- | Steps the formula through the trace as long as it requires
-- more states (i.e. contains 'DNext' terms).
stepRequired :: MonadError CheckError m => Formula -> Parser State m (Formula, Result)
stepRequired f =
  draw >>= \case
    Nothing -> throwError (CannotStep f)
    Just x ->
      case step f x of
        Step f' Nothing -> stepRequired f'
        Step f' (Just r) -> pure (f', r)

-- | Steps the formula through the residual states (states that are
-- available but not required) and computes the final result.
stepResidual :: MonadError CheckError m => Formula -> Result -> Parser State m Result
stepResidual _ r@Definitely {} = pure r
stepResidual f r =
  draw >>= \case
    Nothing -> pure r
    Just s ->
      case step f s of
        Step f' Nothing -> throwError (CannotStep f')
        Step f' (Just r') -> stepResidual f' r'

verify :: MonadError CheckError m => Formula -> Producer State m () -> m Result
verify f trace = do
  ((f', r), trace') <- runStateT (stepRequired f) trace
  evalStateT (stepResidual f' r) trace'

-- * Run interactively

stdinStates :: MonadIO m => Producer State m ()
stdinStates = stdinLn >-> loop
  where
    loop = do
      line <- await
      if null line then pure () else mapM_ yield line >> loop

verifyInteractive :: Formula -> IO ()
verifyInteractive f = do
  hSetBuffering stdin LineBuffering
  runExceptT (verify f stdinStates) >>= \case
    Left CannotStep {} -> putStrLn "Verification failed due to lack of observed states."
    Left err -> putStrLn ("Verification failed with error: " <> show err)
    Right result -> putStrLn ("Verification result: " <> show result)

-}