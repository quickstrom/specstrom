{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module is a draft for the streaming verifier of
-- Specstrom/quickLTL formulae. Currently it uses a separate and
-- simpler language (at least for atomic propositions) just for
-- experimentation.
module Specstrom.StreamingVerifier where

import Algebra.Heyting
import Algebra.Lattice
import Control.Applicative (Applicative (liftA2))
import Numeric.Natural

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
always 0 f = f /\ WNext (always 0 f)
always n f = f /\ DNext (always (pred n) f)

eventually :: Natural -> Formula -> Formula
eventually 0 f = f \/ Next (eventually 0 f)
eventually n f = f \/ DNext (eventually (pred n) f)

instance Lattice Formula where
  Absurd /\ _ = Absurd
  _ /\ Absurd = Absurd
  Trivial /\ x = x
  x /\ Trivial = x
  x /\ y = And x y

  Absurd \/ x = x
  x \/ Absurd = x
  Trivial \/ _ = Trivial
  _ \/ Trivial = Trivial
  x \/ y = Or x y

instance BoundedMeetSemiLattice Formula where
  top = Trivial

instance BoundedJoinSemiLattice Formula where
  bottom = Absurd

instance Heyting Formula where
  p ==> q = neg p \/ q
  neg = \case
    Trivial -> Absurd
    Absurd -> Trivial
    Not p -> p
    And p q -> neg p \/ neg q
    Or p q -> neg p /\ neg q
    p -> Not p

-- * Checking

data Certainty a = Definitely a | Probably a
  deriving (Eq, Show, Functor)

instance Lattice (Certainty Bool) where
  _ /\ Definitely False = Definitely False
  Definitely False /\ _ = Definitely False
  Definitely a /\ Definitely b = Definitely (a /\ b)
  Probably a /\ Definitely b = Probably (a /\ b)
  Definitely a /\ Probably b = Probably (a /\ b)
  Probably a /\ Probably b = Probably (a /\ b)

  p \/ Definitely False = p
  Definitely False \/ p = p
  Definitely a \/ Definitely b = Definitely (a \/ b)
  Probably a \/ Definitely b = Definitely (a \/ b)
  Definitely a \/ Probably b = Definitely (a \/ b)
  Probably a \/ Probably b = Probably (a \/ b)

instance BoundedMeetSemiLattice (Certainty Bool) where
  top = Definitely top

instance BoundedJoinSemiLattice (Certainty Bool) where
  bottom = Definitely bottom

instance Heyting (Certainty Bool) where
  p ==> q = neg p \/ q
  neg = fmap neg

type Result = Certainty Bool

data CheckError
  = CannotStep Formula
  | UnexpectedEndFormula Formula
  deriving (Show)

-- | Advance the formula one step forward using the given state.
step :: Formula -> State -> Formula
step Trivial _ = Trivial
step Absurd _ = Absurd
step (Atomic a) s = if a s then Trivial else Absurd
step (And f1 f2) s = step f1 s /\ step f2 s
step (Or f1 f2) s = step f1 s \/ step f2 s
step (Not p) s = neg (step p s)
step (Next f) _ = f
step (WNext f) _ = f
step (DNext f) _ = f

requiresMoreStates :: Formula -> Bool
requiresMoreStates = \case
  Trivial -> False
  Absurd -> False
  Atomic {} -> False
  And f1 f2 -> requiresMoreStates f1 \/ requiresMoreStates f2
  Or f1 f2 -> requiresMoreStates f1 \/ requiresMoreStates f2
  Not p -> requiresMoreStates p
  Next {} -> False
  WNext {} -> False
  DNext {} -> True

-- | Steps the formula through the trace as long as it requires
-- more states (i.e. contains 'DNext' terms).
stepRequired :: Formula -> Trace -> Either CheckError (Formula, Trace)
stepRequired f t | requiresMoreStates f =
  case t of
    [] -> Left (CannotStep f)
    x : xs -> stepRequired (step f x) xs
stepRequired f t = Right (f, t)

-- | Steps the formula through the residual states (states that are
-- available but not required) and computes the final result.
stepResidual :: Formula -> Trace -> Either CheckError Result
stepResidual f [] = Left (CannotStep f)
stepResidual f (x1 : x2 : xs) = stepResidual (step f x1) (x2 : xs)
stepResidual f [s] = compute f
  where
    compute = \case
      Trivial -> Right (Definitely True)
      Absurd -> Right (Definitely False)
      Atomic a -> if a s then Right (Definitely True) else Right (Definitely False)
      Next {} -> Right (Probably False)
      WNext {} -> Right (Probably True)
      n@DNext {} -> Left (UnexpectedEndFormula n)
      And p q -> (/\) <$> compute p <*> compute q
      Or p q -> (\/) <$> compute p <*> compute q
      Not p -> neg <$> compute p

-- | Verify a pre-collected trace with the given formula.
verify :: Formula -> Trace -> Either CheckError Result
verify f t = do
  (f', t') <- stepRequired f t
  stepResidual f' t'
