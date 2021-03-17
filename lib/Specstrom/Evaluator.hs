{-# LANGUAGE OverloadedStrings #-}

module Specstrom.Evaluator where

import Control.Monad (zipWithM)
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Specstrom.Lexer (Position, dummyPosition)
import Specstrom.Syntax

type Env = M.Map Name Value

type State = M.Map Name [Value]

data Thunk = T Env (Expr Pattern) (IORef (Maybe Value))

data Strength = AssumeTrue | AssumeFalse | Demand deriving (Show)

data PrimOp
  = -- unary
    NextF
  | NextT
  | NextD
  | Always
  | Not
  | -- binary
    And
  | Or
  | Equals
  | -- ternary
    IfThenElse
  deriving (Show, Eq, Ord, Enum, Bounded)

primOpVar :: PrimOp -> Name
primOpVar op = case op of
  NextF -> "nextF_"
  NextT -> "nextT_"
  NextD -> "next_"
  Always -> "always_"
  Not -> "not_"
  And -> "_&&_"
  Or -> "_||_"
  Equals -> "_==_"
  IfThenElse -> "if_then_else_"

basicEnv :: Env
basicEnv =
  M.fromList (values <> primOps)
  where
    values =
      [ ("true", Trivial),
        ("false", Absurd),
        ("null", Null)
      ]
    primOps =
      [ (primOpVar op, Op op []) | op <- enumFromTo minBound maxBound
      ]

isUnary :: PrimOp -> Bool
isUnary = (<= Not)

isBinary :: PrimOp -> Bool
isBinary x = not (isUnary x) && x <= Equals

data Residual
  = Next Strength Thunk
  | Conjunction Residual Residual
  | Disjunction Residual Residual

data Value
  = -- function values
    Closure (Name, Position, Int) Env [Pattern] Body
  | Op PrimOp [Value]
  | -- thunks (not returned by false)
    Thunk Thunk
  | Frozen State Thunk
  | -- Residual formulae
    Residual Residual
  | -- Values
    Trivial
  | Absurd
  | Null
  | Object (M.Map Name Value)
  | List [Value]
  | LitVal Lit

data EvalError = Error String

type Eval = IO

evaluateBody :: Env -> Body -> Eval Value
evaluateBody g (Local b r) = evaluateBind g b >>= \g' -> evaluateBody g' r
evaluateBody g (Done e) = Thunk <$> newThunk g e

newThunk :: Env -> Expr Pattern -> Eval Thunk
newThunk g e = T g e <$> newIORef Nothing

evaluateBind :: Env -> Bind -> Eval Env
evaluateBind g (Bind (Direct (VarP n p)) e) = M.insert n <$> evaluateBody g e <*> pure g
evaluateBind g (Bind (FunP n p pats) e) = pure (M.insert n (Closure (n, p, 0) g pats e) g)

withPatterns :: Pattern -> Value -> Env -> Eval Env
withPatterns (VarP n p) v g = pure (M.insert n v g)

delayedEvaluate :: State -> Env -> Expr Pattern -> Eval Value
-- note that any call to "evaluate" here must not in turn call "force" or we will get dangerous strictness.
delayedEvaluate s g v@(Var {}) = evaluate s g v
delayedEvaluate s g v@(Literal {}) = evaluate s g v
delayedEvaluate s g v@(Lam {}) = evaluate s g v
delayedEvaluate s g e = Thunk <$> newThunk g e

evaluate :: State -> Env -> Expr Pattern -> Eval Value
evaluate s g (Projection e t) = do
  v' <- force s =<< evaluate s g e
  case v' of
    List (Object m : _) | Just v <- M.lookup t m -> pure v
    Object m | Just v <- M.lookup t m -> pure v
    _ -> error "TODO proper error"
evaluate s g (Var p t) = case M.lookup t g of
  Just v -> pure v
  Nothing -> error "Impossible: variable not found in environment"
evaluate s g (App e1 e2) = do
  v <- force s =<< evaluate s g e1
  v2 <- delayedEvaluate s g e2 -- TODO avoid thunking everything when safe
  case v of
    Op o [] ->
      if isUnary o
        then unaryOp o s v2
        else pure (Op o [v2])
    Op o [v1] ->
      if isBinary o
        then binaryOp o s v1 v2
        else pure (Op o [v1, v2])
    Op o [v1, v1'] -> ternaryOp o s v1 v1' v2
    Closure (n, p, ai) g' [pat] body -> do
      g'' <- withPatterns pat v2 g'
      evaluateBody g'' body -- If we want backtraces, add (n,p,ai) to a stack while running this
    Closure (n, p, ai) g' (pat : pats) body -> do
      g'' <- withPatterns pat v2 g'
      pure (Closure (n, p, ai + 1) g'' pats body)
evaluate s g (Literal p (SelectorLit l)) = case M.lookup l s of
  Nothing -> error "Can't find it in the state (analysis failed?)"
  Just ls -> pure (List ls)
evaluate s g (Literal p l) = pure (LitVal l)
evaluate s g (Lam p pat e) = pure (Closure ("fun", p, 0) g [pat] (Done e))
evaluate s g (Freeze p pat e1 e2) = do
  v1 <- Frozen s <$> newThunk g e1
  g' <- withPatterns pat v1 g
  evaluate s g' e2

forceThunk :: Thunk -> State -> Eval Value
forceThunk (T g e r) s = do
  x <- readIORef r
  case x of
    Nothing -> do
      v <- force s =<< evaluate s g e
      writeIORef r (Just v)
      pure v
    Just v -> pure v

force :: State -> Value -> Eval Value
force _ (Frozen s' t) = forceThunk t s'
force s (Thunk t) = forceThunk t s
force s v = pure v

ternaryOp :: PrimOp -> State -> Value -> Value -> Value -> Eval Value
ternaryOp IfThenElse s v1 v2 v3 = do
  v1' <- force s v1
  case v1' of
    Trivial -> pure v2
    Absurd -> pure v3
    _ -> error "Expected boolean in if condition"

areEqual :: State -> Value -> Value -> Eval Bool
areEqual s v1 v2 = do
  v1' <- force s v1
  v2' <- force s v2
  areEqual' v1' v2'
  where
    areEqual' :: Value -> Value -> Eval Bool
    areEqual' (Closure {}) _ = error "Cannot compare functions for equality"
    areEqual' _ (Closure {}) = error "Cannot compare functions for equality"
    areEqual' (Op {}) _ = error "Cannot compare functions for equality"
    areEqual' _ (Op {}) = error "Cannot compare functions for equality"
    areEqual' (Residual {}) _ = error "Cannot compare temporal formulae for equality"
    areEqual' _ (Residual {}) = error "Cannot compare temporal formulae for equality"
    areEqual' Trivial Trivial = pure True
    areEqual' Absurd Absurd = pure True
    areEqual' Null Null = pure True
    areEqual' (List as) (List bs) | length as == length bs = and <$> zipWithM (areEqual s) as bs
    areEqual' (Object as) (Object bs) = undefined -- for now
    areEqual' _ _ = pure False

binaryOp :: PrimOp -> State -> Value -> Value -> Eval Value
binaryOp And s v1 v2 = do
  v1' <- force s v1
  case v1' of
    Absurd -> pure Absurd
    Trivial -> force s v2
    Residual r -> do
      v2' <- force s v2
      case v2' of
        Absurd -> pure Absurd
        Trivial -> pure (Residual r)
        Residual r' -> pure (Residual (Conjunction r r'))
        _ -> error "And expects formulae"
    _ -> error "And expects formulae"
binaryOp Or s v1 v2 = do
  v1' <- force s v1
  case v1' of
    Trivial -> pure Trivial
    Absurd -> force s v2
    Residual r -> do
      v2' <- force s v2
      case v2' of
        Trivial -> pure Trivial
        Absurd -> pure (Residual r)
        Residual r' -> pure (Residual (Disjunction r r'))
        _ -> error "Or expects formulae"
    _ -> error "Or expects formulae"
binaryOp Equals s v1 v2 = areEqual s v1 v2 >>= \b -> if b then pure Trivial else pure Absurd

unaryOp :: PrimOp -> State -> Value -> Eval Value
unaryOp Always s v@(Thunk (T g e _)) = do
  v' <- force s v
  case v' of
    Absurd -> pure Absurd
    Trivial -> do
      residual <- Next AssumeTrue <$> newThunk g e
      pure (Residual residual)
    Residual r -> do
      residual <- Next AssumeTrue <$> newThunk g e
      pure (Residual (Conjunction r residual))
    _ -> error "Always expects formula"
unaryOp Not s v = do
  v' <- force s v
  case v' of
    Absurd -> pure Trivial
    Trivial -> pure Absurd
    Residual f -> Residual <$> negateResidual f
    _ -> error "Not expects boolean"
  where
    negateResidual (Conjunction a b) = Disjunction <$> negateResidual a <*> negateResidual b
    negateResidual (Disjunction a b) = Conjunction <$> negateResidual a <*> negateResidual b
    negateResidual (Next st (T g e v)) = do
      let st' = case st of AssumeTrue -> AssumeFalse; AssumeFalse -> AssumeTrue; Demand -> Demand
      Next st' <$> newThunk g (App (Var dummyPosition "not_") e)
unaryOp NextF s (Thunk t) = pure (Residual (Next AssumeFalse t))
unaryOp NextT s (Thunk t) = pure (Residual (Next AssumeTrue t))
unaryOp NextD s (Thunk t) = pure (Residual (Next Demand t))
unaryOp _ _ _ = error "impossible"

-- The output value will be Trivial if the formula is now true, Absurd if false,
-- Or another Residual if still requiring more states.
-- Any other value is presumably a type error.
step :: Residual -> State -> Eval Value
step (Next _ t) s = forceThunk t s
step (Conjunction r1 r2) s = binaryOp And s <$> step r1 s <*> step r2 s
step (Disjunction r1 r2) s = binaryOp Or s <$> step r1 s <*> step r2 s

stop :: Residual -> Eval Bool
stop (Next s t) = case s of
  AssumeTrue -> pure True
  AssumeFalse -> pure False
  Demand -> error "Cannot stop, require more states"
stop (Conjunction r1 r2) = (&&) <$> stop r1 <*> stop r2
stop (Disjunction r1 r2) = (||) <$> stop r1 <*> stop r2
