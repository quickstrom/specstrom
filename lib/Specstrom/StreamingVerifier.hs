{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | This module is a draft for the streaming verifier of
-- Specstrom/quickLTL formulae. Currently it uses a separate and
-- simpler language (at least for atomic propositions) just for
-- experimentation.
module Specstrom.StreamingVerifier where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Numeric.Natural
import Pipes (Producer, await, yield, (>->))
import Pipes.Parse (Parser, draw, evalStateT, runStateT)
import Pipes.Prelude (stdinLn)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdin)

-- * Language

type State = Char

type Trace = [State]

data Formula
  = Trivial
  | Absurd
  | Atomic (State -> Bool)
  | And Formula Formula
  | Or Formula Formula
  | Not Formula
  | -- | strong next
    Next Formula
  | -- | weak next
    WNext Formula
  | -- | demanding next
    DNext Formula

instance Show Formula where
  show = \case
    Trivial -> "Trivial"
    Absurd -> "Absurd"
    Atomic _ -> "Atomic"
    And p q -> "(And " <> show p <> " " <> show q <> ")"
    Or p q -> "(Or " <> show p <> " " <> show q <> ")"
    Not p -> "(Not " <> show p <> ")"
    Next _ -> "(Next ...)"
    WNext _ -> "(WNext ...)"
    DNext p -> "(DNext " <> show p <> ")"

-- * Syntax

is :: Char -> Formula
is c = Atomic (== c)

next :: Formula -> Formula
next = Next

wnext :: Formula -> Formula
wnext = WNext

always :: Natural -> Formula -> Formula
always 0 f = f `formulaAnd` WNext (always 0 f)
always n f = f `formulaAnd` DNext (always (pred n) f)

eventually :: Natural -> Formula -> Formula
eventually 0 f = f `formulaOr` Next (eventually 0 f)
eventually n f = f `formulaOr` DNext (eventually (pred n) f)

formulaAnd :: Formula -> Formula -> Formula
formulaAnd Absurd _ = Absurd
formulaAnd _ Absurd = Absurd
formulaAnd Trivial x = x
formulaAnd x Trivial = x
formulaAnd x y = And x y

formulaOr :: Formula -> Formula -> Formula
formulaOr Absurd x = x
formulaOr x Absurd = x
formulaOr Trivial _ = Trivial
formulaOr _ Trivial = Trivial
formulaOr x y = Or x y

formulaNegate :: Formula -> Formula
formulaNegate = \case
  Trivial -> Absurd
  Absurd -> Trivial
  Not p -> p
  And p q -> formulaNegate p `formulaOr` formulaNegate q
  Or p q -> formulaNegate p `formulaAnd` formulaNegate q
  p -> Not p

-- * Checking

data Result = Definitely Bool | Probably Bool
  deriving (Eq, Show)

resultAnd :: Result -> Result -> Result
resultAnd _ (Definitely False) = Definitely False
resultAnd (Definitely False) _ = Definitely False
resultAnd (Definitely a) (Definitely b) = Definitely (a && b)
resultAnd (Probably a) (Definitely b) = Probably (a && b)
resultAnd (Definitely a) (Probably b) = Probably (a && b)
resultAnd (Probably a) (Probably b) = Probably (a && b)

resultOr :: Result -> Result -> Result
resultOr p (Definitely False) = p
resultOr (Definitely False) p = p
resultOr (Definitely a) (Definitely b) = Definitely (a || b)
resultOr (Probably a) (Definitely b) = Definitely (a || b)
resultOr (Definitely a) (Probably b) = Definitely (a || b)
resultOr (Probably a) (Probably b) = Probably (a || b)

resultNegate :: Result -> Result
resultNegate (Probably b) = Probably (not b)
resultNegate (Definitely b) = Definitely (not b)

data CheckError
  = CannotStep Formula
  | UnexpectedEndFormula Formula
  deriving (Show)

data Step = Step {stepFormula :: Formula, stepResult :: Maybe Result}

stepTop :: Step
stepTop = Step Trivial (Just (Definitely True))

stepBottom :: Step
stepBottom = Step Absurd (Just (Definitely False))

stepAnd :: Step -> Step -> Step
stepAnd s1 s2 =
  case (s1, s2) of
    (Step p' (Just pr), Step q' (Just qr)) -> Step (p' `formulaAnd` q') (Just (pr `resultAnd` qr))
    (_, r@(Step _ (Just (Definitely False)))) -> r
    (r@(Step _ (Just (Definitely False))), _) -> r
    (Step p' Nothing, Step q' Just {}) -> Step (p' `formulaAnd` q') Nothing
    (Step p' Just {}, Step q' Nothing) -> Step (p' `formulaAnd` q') Nothing
    (Step p' Nothing, Step q' Nothing) -> Step (p' `formulaAnd` q') Nothing

stepOr :: Step -> Step -> Step
stepOr s1 s2 =
  case (s1, s2) of
    (Step p' (Just pr), Step q' (Just qr)) -> Step (p' `formulaOr` q') (Just (pr `resultOr` qr))
    (_, r@(Step _ (Just (Definitely True)))) -> r
    (r@(Step _ (Just (Definitely True))), _) -> r
    (Step p' Nothing, Step q' Nothing) -> Step (p' `formulaOr` q') Nothing
    (r@(Step _ Nothing), _) -> r
    (_, r@(Step _ Nothing)) -> r

stepNegate :: Step -> Step
stepNegate (Step p' pr) = Step (formulaNegate p') (resultNegate <$> pr)

-- | Simplify and advance the formula one step forward using the given
-- state. Returns a 'Left' if more states are required to determine
-- a result, and 'Right' if a possible result is available.
step :: Formula -> State -> Step
step Trivial _ = stepTop
step Absurd _ = stepBottom
step (Atomic a) s = if a s then stepTop else stepBottom
step (And p q) s = stepAnd (step p s) (step q s)
step (Or p q) s = stepOr (step p s) (step q s)
step (Not p) s = stepNegate (step p s)
step (Next f) _ = Step f (Just (Probably False))
step (WNext f) _ = Step f (Just (Probably True))
step (DNext f) _ = Step f Nothing

-- | Steps the formula through the trace as long as it requires
-- more states (i.e. contains 'DNext' terms).
stepRequiredList :: Formula -> Trace -> Either CheckError (Formula, Result, Trace)
stepRequiredList f [] = Left (CannotStep f)
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

-- * Streaming with Pipes

isDefinitive :: Formula -> Bool
isDefinitive Trivial = True
isDefinitive Absurd = True
isDefinitive _ = False

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
