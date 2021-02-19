{-# LANGUAGE DeriveFunctor #-}
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
import Pipes.Parse (Parser, draw, evalStateT, peek, runStateT)
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

-- | Simplify and advance the formula one step forward using the given
-- state.
step :: Formula -> State -> Formula
step Trivial _ = Trivial
step Absurd _ = Absurd
step (Atomic a) s = if a s then Trivial else Absurd
step (And p q) s = step p s `formulaAnd` step q s
step (Or p q) s = step p s `formulaOr` step q s
step (Not p) s = formulaNegate (step p s)
step (Next f) _ = f
step (WNext f) _ = f
step (DNext f) _ = f

requiresMoreStates :: Formula -> Bool
requiresMoreStates = \case
  Trivial -> False
  Absurd -> False
  Atomic {} -> False
  And p q -> requiresMoreStates p || requiresMoreStates q
  Or p q -> requiresMoreStates p || requiresMoreStates q
  Not p -> requiresMoreStates p
  Next {} -> False
  WNext {} -> False
  DNext {} -> True

-- | Steps the formula through the trace as long as it requires
-- more states (i.e. contains 'DNext' terms).
stepRequiredList :: Formula -> Trace -> Either CheckError (Formula, Trace)
stepRequiredList f t | requiresMoreStates f =
  case t of
    [] -> Left (CannotStep f)
    x : xs -> stepRequiredList (step f x) xs
stepRequiredList f t = Right (f, t)

-- | Steps the formula through the residual states (states that are
-- available but not required) and computes the final result.
stepResidualList :: Formula -> Trace -> Either CheckError Result
stepResidualList f [] = Left (CannotStep f)
stepResidualList f (x1 : x2 : xs) = stepResidualList (step f x1) (x2 : xs)
stepResidualList f [s] = compute f
  where
    compute = \case
      Trivial -> Right (Definitely True)
      Absurd -> Right (Definitely False)
      Atomic a -> if a s then Right (Definitely True) else Right (Definitely False)
      Next {} -> Right (Probably False)
      WNext {} -> Right (Probably True)
      n@DNext {} -> Left (UnexpectedEndFormula n)
      And p q -> resultAnd <$> compute p <*> compute q
      Or p q -> resultOr <$> compute p <*> compute q
      Not p -> resultNegate <$> compute p

-- | Verify a pre-collected trace with the given formula.
verifyList :: Formula -> Trace -> Either CheckError Result
verifyList f t = do
  (f', t') <- stepRequiredList f t
  stepResidualList f' t'

-- * Streaming with Pipes

isDecided :: Formula -> Bool
isDecided Trivial = True
isDecided Absurd = True
isDecided _ = False

-- | Steps the formula through the trace as long as it requires
-- more states (i.e. contains 'DNext' terms).
stepRequired :: MonadError CheckError m => Formula -> Parser State m Formula
stepRequired f
  | requiresMoreStates f =
    draw >>= \case
      Nothing -> throwError (CannotStep f)
      Just x ->
        let f' = step f x
         in stepRequired f'
stepRequired f = pure f

-- | Steps the formula through the residual states (states that are
-- available but not required) and computes the final result.
stepResidual :: MonadError CheckError m => Formula -> Parser State m Result
stepResidual Trivial = pure (Definitely True)
stepResidual Absurd = pure (Definitely False)
stepResidual f =
  draw >>= \case
    Nothing -> throwError (CannotStep f)
    Just s ->
      case step f s of
        f'
          | isDecided f' -> compute s f'
          | otherwise ->
            peek >>= \case
              Just {} -> stepResidual f'
              Nothing -> compute s f'
  where
    compute s = \case
      Trivial -> pure (Definitely True)
      Absurd -> pure (Definitely False)
      Atomic a -> pure (if a s then Definitely True else Definitely False)
      Next {} -> pure (Probably False)
      WNext {} -> pure (Probably True)
      n@DNext {} -> throwError (UnexpectedEndFormula n)
      And p q -> resultAnd <$> compute s p <*> compute s q
      Or p q -> resultOr <$> compute s p <*> compute s q
      Not p -> resultNegate <$> compute s p

verify :: MonadError CheckError m => Formula -> Producer State m () -> m Result
verify f trace = do
  (f', trace') <- runStateT (stepRequired f) trace
  evalStateT (stepResidual f') trace'

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
    Left err -> putStrLn ("Verification failed with error:" <> show err)
    Right result -> putStrLn ("Verification result: " <> show result)
