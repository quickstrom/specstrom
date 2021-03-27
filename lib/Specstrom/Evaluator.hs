{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Specstrom.Evaluator where

import Control.Monad (MonadPlus (mzero), zipWithM)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as M
import Data.IORef
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Specstrom.Lexer (Position, dummyPosition)
import Specstrom.Syntax

type Env = M.HashMap Name Value

type State = M.HashMap Selector [Value]

data Thunk = T Env (Expr Pattern) (IORef (Maybe Value))

instance Show Thunk where
  show _ = "<thunk>"

data Strength = AssumeTrue | AssumeFalse | Demand deriving (Show)

data BaseAction = Click Name | Noop | Loaded | Changed Name
  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

isEvent :: BaseAction -> Bool
isEvent Loaded = True
isEvent (Changed n) = True
isEvent _ = False

data Action = A BaseAction (Maybe Int)
  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

data PrimOp
  = -- unary
    NextF
  | NextT
  | NextD
  | Always
  | Not
  | ClickAct
  | ChangedAct
  | -- binary
    And
  | Or
  | Equals
  | WhenAct
  | TimeoutAct
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | -- ternary
    IfThenElse
  deriving (Show, Eq, Ord, Enum, Bounded)

primOpVar :: PrimOp -> Name
primOpVar op = case op of
  WhenAct -> "_when_"
  TimeoutAct -> "_timeout_"
  NextF -> "nextF_"
  NextT -> "nextT_"
  NextD -> "next_"
  Always -> "always_"
  Not -> "not_"
  And -> "_&&_"
  Or -> "_||_"
  Equals -> "_==_"
  Addition -> "_+_"
  Subtraction -> "_-_"
  Multiplication -> "_*_"
  Division -> "_/_"
  IfThenElse -> "if_then_else_"
  ClickAct -> "click!"
  ChangedAct -> "changed?"

basicEnv :: Env
basicEnv =
  M.fromList (values <> primOps)
  where
    values =
      [ ("true", Trivial),
        ("false", Absurd),
        ("null", Null),
        ("noop!", Action (A Noop Nothing)),
        ("loaded?", Action (A Loaded Nothing))
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
  deriving (Show)

data Value
  = -- function values
    Closure (Name, Position, Int) Env [Pattern] Body
  | Op PrimOp [Value]
  | -- thunks (not returned by force)
    Thunk Thunk
  | Frozen State Thunk
  | -- Residual formulae
    Residual Residual
  | -- Actions
    Action Action
  | -- Values
    Trivial
  | Absurd
  | Null
  | Object (M.HashMap Name Value)
  | List [Value]
  | LitVal Lit
  deriving (Show)

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
  Nothing -> error ("Impossible: variable '" <> Text.unpack t <> "' not found in environment")
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
evaluate s g (Literal p (SelectorLit l@(Selector sel))) = case M.lookup l s of
  Nothing -> error ("Can't find '" <> Text.unpack sel <> "' in the state (analysis failed?)")
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

unaryOp :: PrimOp -> State -> Value -> Eval Value
unaryOp ClickAct s v =
  do
    v' <- force s v
    let vs = case v' of
          Object m -> [Object m]
          List ms -> ms
          _ -> error "click expects element(s)"
    let vs' = flip filter vs $ \x -> case x of
          Object m -> case M.lookup "disabled" m of
            Just Absurd -> True
            _ -> False
    as <- flip traverse vs' $ \v -> case v of
      Object m -> case M.lookup "ref" m of
        Just (LitVal (StringLit s)) -> pure (Action (A (Click s) Nothing))
        _ -> error "Cannot find ref of element for click"
      _ -> error "click expects element(s)"
    case as of
      [a] -> pure a
      as -> pure (List as)
unaryOp ChangedAct s v = do
  v' <- force s v
  let vs = case v' of
        Object m -> [Object m]
        List ms -> ms
        _ -> error "changed expects element(s)"
  as <- flip traverse vs $ \v -> case v of
    Object m -> case M.lookup "ref" m of
      Just (LitVal (StringLit s)) -> pure (Action (A (Changed s) Nothing))
      _ -> error "Cannot find ref of element for changed"
    _ -> error "changed expects element(s)"
  case as of
    [a] -> pure a
    as -> pure (List as)
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
    _ -> error ("Not expects boolean, got: " <> show v')
  where
    negateResidual (Conjunction a b) = Disjunction <$> negateResidual a <*> negateResidual b
    negateResidual (Disjunction a b) = Conjunction <$> negateResidual a <*> negateResidual b
    negateResidual (Next st (T g e v)) = do
      let st' = case st of AssumeTrue -> AssumeFalse; AssumeFalse -> AssumeTrue; Demand -> Demand
      Next st' <$> newThunk g (App (Var dummyPosition "not_") e)
unaryOp NextF s (Thunk t) = pure (Residual (Next AssumeFalse t))
unaryOp NextT s (Thunk t) = pure (Residual (Next AssumeTrue t))
unaryOp NextD s (Thunk t) = pure (Residual (Next Demand t))
unaryOp op _ arg = error ("Impossible unary operation " <> show op <> " on: " <> show arg)

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
binaryOp Addition s v1 v2 = binaryNumOp s v1 v2 Addition
binaryOp Subtraction s v1 v2 = binaryNumOp s v1 v2 Subtraction
binaryOp WhenAct s v1 v2 = do
  v2' <- force s v2
  case v2' of
    Trivial -> pure v1
    Absurd -> pure Null
    _ -> error "Expected boolean in when condition"
binaryOp TimeoutAct s v1 v2 = do
  v1' <- force s v1
  v2' <- force s v2
  t <- case v2 of
    LitVal (IntLit i) -> pure i
    _ -> error "Timeout expects an integer timeout"
  case v1 of
    Action (A act Nothing) -> pure (Action (A act (Just t)))
    Action (A _ (Just _)) -> error "Action already has a timeout"
    List vs -> List <$> traverse (\x -> binaryOp TimeoutAct s x v2') vs
    _ -> error "Timeout expects an action"

binaryNumOp :: State -> Value -> Value -> PrimOp -> IO Value
binaryNumOp s v1 v2 op = do
  v1' <- force s v1
  v2' <- force s v2
  case (v1', v2') of
    (LitVal (IntLit i1), LitVal (IntLit i2)) 
      | op == Division -> pure (LitVal (IntLit (i1 `div` i2))) 
      | otherwise -> pure (LitVal (IntLit (numOp op i1 i2)))
    (LitVal (FloatLit i1), LitVal (FloatLit i2)) 
      | op == Division -> pure (LitVal (FloatLit (i1 / i2)))
      | otherwise -> pure (LitVal (FloatLit (numOp op i1 i2)))
    _ -> error (show op <> " expects matching numeric types, but got: " <> show v1 <> " and " <> show v2)
  where
    numOp Addition = (+)
    numOp Subtraction = (-)
    numOp Multiplication = (*)

resetThunk :: Thunk -> Eval Thunk
resetThunk (T e exp ior) = do
  writeIORef ior Nothing
  (\e' -> T e' exp ior) <$> traverse resetThunks e

resetThunks :: Value -> Eval Value
resetThunks (Closure a e b c) = (\e' -> Closure a e' b c) <$> traverse resetThunks e
resetThunks (Op o vs) = Op o <$> traverse resetThunks vs
resetThunks (Thunk t) = Thunk <$> resetThunk t
resetThunks (Frozen s t) = pure $ Frozen s t -- should be safe? Never need to re-evaluate frozen stuff.
resetThunks (Residual r) = pure $ Residual r -- that all gets cleared out later
resetThunks (Object o) = Object <$> traverse resetThunks o
resetThunks (List o) = List <$> traverse resetThunks o
resetThunks (Absurd) = pure Absurd
resetThunks (Trivial) = pure Trivial
resetThunks (Null) = pure Null
resetThunks (LitVal l) = pure $ LitVal l

-- The output value will be Trivial if the formula is now true, Absurd if false,
-- Or another Residual if still requiring more states.
-- Any other value is presumably a type error.
step :: Residual -> State -> Eval Value
step (Next _ t) s = resetThunk t >>= \t' -> forceThunk t' s
step (Conjunction r1 r2) s = do
  r1' <- step r1 s
  r2' <- step r2 s
  binaryOp And s r1' r2'
step (Disjunction r1 r2) s = do
  r1' <- step r1 s
  r2' <- step r2 s
  binaryOp Or s r1' r2'

stop :: Residual -> Maybe Bool
stop (Next s t) = case s of
  AssumeTrue -> pure True
  AssumeFalse -> pure False
  Demand -> mzero
stop (Conjunction r1 r2) = (&&) <$> stop r1 <*> stop r2
stop (Disjunction r1 r2) = (||) <$> stop r1 <*> stop r2
