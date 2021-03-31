{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Specstrom.Evaluator where

import Control.Exception (Exception, throw)
import Control.Monad (MonadPlus (mzero), filterM, zipWithM)
import qualified Data.Aeson as JSON
import Data.Fixed (mod')
import Data.Foldable (foldlM, foldrM)
import qualified Data.HashMap.Strict as M
import Data.IORef
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Specstrom.Lexer (Position, dummyPosition)
import Specstrom.Syntax

type Env = M.HashMap Name Value

type State = M.HashMap Selector Value

data Thunk = T Env (Expr Pattern) (IORef (Maybe Value))

instance Show Thunk where
  show _ = "<thunk>"

data Strength = AssumeTrue | AssumeFalse | Demand deriving (Show)

-- data BaseAction = Click Name | Noop | Loaded | Changed Name
--   deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)
--
-- isEvent :: BaseAction -> Bool
-- isEvent Loaded = True
-- isEvent (Changed n) = True
-- isEvent _ = False

--data Action = A BaseAction (Maybe Int)
--  deriving (Show, Eq, Generic, JSON.FromJSON, JSON.ToJSON)

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
  | Implies
  | WhenAct
  | TimeoutAct
  | Addition
  | Subtraction
  | Multiplication
  | Division
  | Modulo
  | LessEq
  | Less
  | GreaterEq
  | Greater
  | NotEquals
  | Equals
  | -- HOFs (binary)
    Map
  | -- ternary
    IfThenElse
  | Foldr
  | Foldl
  | -- quad
    MkPrimAction
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
  Implies -> "_==>_"
  Equals -> "_==_"
  NotEquals -> "_!=_"
  LessEq -> "_<=_"
  Less -> "_<_"
  GreaterEq -> "_>=_"
  Greater -> "_>_"
  Modulo -> "_%_"
  Addition -> "_+_"
  Subtraction -> "_-_"
  Multiplication -> "_*_"
  Division -> "_/_"
  IfThenElse -> "if_then_else_"
  MkPrimAction -> "#act"
  Map -> "map"
  Foldr -> "foldr"
  Foldl -> "foldl"

emptyEnv :: Env
emptyEnv = M.fromList []

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
isBinary x = not (isUnary x) && x <= Map

data Residual
  = Next Strength Thunk
  | Conjunction Residual Residual
  | Disjunction Residual Residual
  | Implication Residual Residual
  deriving (Show)

data Value
  = -- function values
    Closure (Name, Position, Int) Env [Pattern] Body
  | Op PrimOp [Value]
  | -- thunks (not returned by force)
    Thunk Thunk
  | Frozen State Thunk
  | Matched Thunk Pattern Name
  | -- Residual formulae
    Residual Residual
  | -- Actions
    Action Name [Value] (Maybe Int)
  | -- Values
    Trivial
  | Absurd
  | Null
  | Object (M.HashMap Name Value)
  | List [Value]
  | LitVal Lit
  deriving (Show)

data EvalError = Error String deriving (Show)

type Eval = IO

instance Exception EvalError

evalError :: String -> a
evalError = throw . Error

evaluateBody :: Env -> Body -> Eval Value
evaluateBody g (Local b r) = evaluateBind g b >>= \g' -> evaluateBody g' r
evaluateBody g (Done e) = Thunk <$> newThunk g e

newThunk :: Env -> Expr Pattern -> Eval Thunk
newThunk g e = T g e <$> newIORef Nothing

evaluateBind :: Env -> Bind -> Eval Env
evaluateBind g b = evaluateBind' g g b

evaluateActionBind :: Env -> Bind -> Eval Env
evaluateActionBind g (Bind (Direct (VarP n _)) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind (FunP n _ _) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind _ _) = evalError "impossible"

evaluateBind' :: Env -> Env -> Bind -> Eval Env
evaluateBind' g g' (Bind (Direct (VarP n _)) e) = M.insert n <$> evaluateBody g' e <*> pure g
evaluateBind' g g' (Bind (Direct pat) e) = do
  Thunk t <- evaluateBody g' e
  pure (M.union (M.fromList (map (\v -> (v, Matched t pat v)) (patternVars pat))) g)
evaluateBind' g g' (Bind (FunP n p pats) e) = pure (M.insert n (Closure (n, p, 0) g' pats e) g)

withPatterns :: State -> Pattern -> Value -> Env -> Eval (Maybe Env)
withPatterns s (VarP n p) v g = pure (Just $ M.insert n v g)
withPatterns s (ListP p ps) v g = do
  v' <- force s v
  case v' of
    List ls | length ls == length ps -> withPatternses s ps ls g
    _ -> pure Nothing
withPatterns s (ActionP n p ps) v g = do
  v' <- force s v
  case v' of
    Action n' ls _ | n == n' && length ls == length ps -> withPatternses s ps ls g
    _ -> pure Nothing

withPatternses :: State -> [Pattern] -> [Value] -> Env -> Eval (Maybe Env)
withPatternses _ [] _ g = pure $ Just g
withPatternses _ _ [] g = pure $ Just g
withPatternses s (p : ps) (v : vs) g = do
  mg' <- withPatterns s p v g
  case mg' of
    Nothing -> pure Nothing
    Just g' -> withPatternses s ps vs g'

delayedEvaluate :: State -> Env -> Expr Pattern -> Eval Value
-- note that any call to "evaluate" here must not in turn call "force" or we will get dangerous strictness.
delayedEvaluate s g v@(Var {}) = evaluate s g v
delayedEvaluate s g v@(Literal {}) = evaluate s g v
delayedEvaluate s g v@(Lam {}) = evaluate s g v
delayedEvaluate s g v@(ListLiteral {}) = evaluate s g v
delayedEvaluate s g e = Thunk <$> newThunk g e

appAll :: State -> Value -> [Value] -> Eval Value
appAll s v [] = pure v
appAll s v (x : xs) = app s v x >>= \v' -> appAll s v' xs

app :: State -> Value -> Value -> Eval Value
app s v v2 =
  case v of
    Op o [] ->
      if isUnary o
        then unaryOp o s v2
        else pure (Op o [v2])
    Op o [v1] ->
      if isBinary o
        then binaryOp o s v1 v2
        else pure (Op o [v1, v2])
    Op o [v1, v1']
      | o /= MkPrimAction -> ternaryOp o s v1 v1' v2
      | otherwise -> pure (Op o [v1, v1', v2])
    Op MkPrimAction [v11, v12, v13] -> makePrimAction s v11 v12 v13 v2
    Action a args t -> do
      v2' <- force s v2
      pure (Action a (args ++ [v2]) t)
    Closure (n, p, ai) g' [pat] body -> do
      mg'' <- withPatterns s pat v2 g'
      case mg'' of
        Nothing -> pure Null
        Just g'' -> evaluateBody g'' body -- If we want backtraces, add (n,p,ai) to a stack while running this
    Closure (n, p, ai) g' (pat : pats) body -> do
      mg'' <- withPatterns s pat v2 g'
      case mg'' of
        Nothing -> pure Null
        Just g'' -> pure (Closure (n, p, ai + 1) g'' pats body)
    Null -> pure Null

evaluate :: State -> Env -> Expr Pattern -> Eval Value
evaluate s g (Index e1 e2) = do
  v' <- force s =<< evaluate s g e1
  i' <- force s =<< evaluate s g e2
  case v' of
    List ls -> do
      case i' of
        LitVal (IntLit i) ->
          let l = length ls; i' = if i < 0 then l + i else i
           in if i' < l && i' >= 0
                then pure (ls !! i')
                else pure Null
        _ -> evalError ("Lists are only indexable by integers")
    Object m -> do
      case i' of
        LitVal (StringLit s) -> case M.lookup s m of
          Just v -> pure v
          Nothing -> pure Null
        _ -> evalError ("Objects are only indexable by strings")
    _ -> evalError ("Indexing doesn't work on non-list/object values")
evaluate s g (Projection e t) = do
  v' <- force s =<< evaluate s g e
  case v' of
    List (Object m : _) | Just v <- M.lookup t m -> pure v
    Object m | Just v <- M.lookup t m -> pure v
    _ -> evalError "Cannot take projection of a non-object (or list of objects)"
evaluate s g (Var p t) = case M.lookup t g of
  Just v -> pure v
  Nothing -> evalError ("Impossible: variable '" <> Text.unpack t <> "' not found in environment (type checker found it though)")
evaluate s g (App e1 e2) = do
  v <- force s =<< evaluate s g e1
  v2 <- delayedEvaluate s g e2 -- TODO avoid thunking everything when safe
  app s v v2
evaluate s g (Literal p (SelectorLit l@(Selector sel))) = case M.lookup l s of
  Nothing -> evalError ("Can't find '" <> Text.unpack sel <> "' in the state (analysis failed?)")
  Just ls -> pure ls
evaluate s g (Literal p l) = pure (LitVal l)
evaluate s g (Lam p pat e) = pure (Closure ("fun", p, 0) g [pat] (Done e))
evaluate s g (ListLiteral p ls) = do
  vs <- mapM (evaluate s g) ls
  pure (List vs)
evaluate s g (Freeze p pat e1 e2) = do
  v1 <- Frozen s <$> newThunk g e1
  mg' <- withPatterns s pat v1 g
  case mg' of
    Just g' -> evaluate s g' e2
    _ -> evalError "Pattern match failure in freeze"

forceThunk :: Thunk -> State -> Eval Value
forceThunk (T g e r) s = do
  x <- readIORef r
  case x of
    Nothing -> do
      v <- force s =<< evaluate s g e
      writeIORef r (Just v)
      pure v
    Just v -> pure v

deepForce :: State -> Value -> Eval Value
deepForce s v = do
  v' <- force s v
  case v' of
    List ls -> List <$> mapM (deepForce s) ls
    Object m -> Object <$> traverse (deepForce s) m
    _ -> pure v'

force :: State -> Value -> Eval Value
force _ (Frozen s' t) = forceThunk t s'
force s (Thunk t) = forceThunk t s
force s (Matched t pat v) = do
  val <- forceThunk t s
  g <- withPatterns s pat val mempty
  case g >>= M.lookup v of
    Nothing -> evalError "Pattern match failure in let binding"
    Just v' -> force s v'
force s v = pure v

makePrimAction :: State -> Value -> Value -> Value -> Value -> Eval Value
makePrimAction s v1 v2 v3 v4 = do
  v1' <- force s v1
  v2' <- force s v2
  v3' <- force s v3
  v4' <- force s v4
  -- no typechecking..
  pure (Object (M.fromList [("id", v1'), ("event", v2'), ("args", v3'), ("timeout", v4')]))

ternaryOp :: PrimOp -> State -> Value -> Value -> Value -> Eval Value
ternaryOp IfThenElse s v1 v2 v3 = do
  v1' <- force s v1
  case v1' of
    Trivial -> pure v2
    Absurd -> pure v3
    _ -> evalError "Expected boolean in if condition"
ternaryOp Foldr s v1 v2 v3 = do
  func <- force s v1
  zero <- force s v2
  list <- force s v3
  case list of
    List ls -> foldrM (\a b -> appAll s func [a, b]) zero ls
    x -> appAll s func [x, zero]
ternaryOp Foldl s v1 v2 v3 = do
  func <- force s v1
  zero <- force s v2
  list <- force s v3
  case list of
    List ls -> foldlM (\a b -> appAll s func [a, b]) zero ls
    x -> appAll s func [zero, x]

areEqual :: State -> Value -> Value -> Eval Bool
areEqual s v1 v2 = do
  v1' <- force s v1
  v2' <- force s v2
  areEqual' v1' v2'
  where
    areEqual' :: Value -> Value -> Eval Bool
    areEqual' (Closure {}) _ = evalError "Cannot compare functions for equality"
    areEqual' _ (Closure {}) = evalError "Cannot compare functions for equality"
    areEqual' (Op {}) _ = evalError "Cannot compare functions for equality"
    areEqual' _ (Op {}) = evalError "Cannot compare functions for equality"
    areEqual' (Residual {}) _ = evalError "Cannot compare temporal formulae for equality"
    areEqual' _ (Residual {}) = evalError "Cannot compare temporal formulae for equality"
    areEqual' Trivial Trivial = pure True
    areEqual' Absurd Absurd = pure True
    areEqual' Null Null = pure True
    areEqual' (List as) (List bs) | length as == length bs = and <$> zipWithM (areEqual s) as bs
    areEqual' (Action n as x) (Action m bs y) | n == m && length as == length bs && x == y = and <$> zipWithM (areEqual s) as bs
    areEqual' (Object as) (Object bs) = undefined -- for now
    areEqual' (LitVal l) (LitVal l') = pure $ l == l'
    areEqual' _ _ = pure False

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
    _ -> evalError "Always expects formula"
unaryOp Not s v = do
  v' <- force s v
  case v' of
    Absurd -> pure Trivial
    Trivial -> pure Absurd
    Residual f -> Residual <$> negateResidual f
    _ -> evalError ("Not expects boolean, got: " <> show v')
unaryOp NextF s (Thunk t) = pure (Residual (Next AssumeFalse t))
unaryOp NextT s (Thunk t) = pure (Residual (Next AssumeTrue t))
unaryOp NextD s (Thunk t) = pure (Residual (Next Demand t))
unaryOp op _ arg = evalError ("Impossible unary operation " <> show op <> " on: " <> show arg)

negateResidual (Conjunction a b) = Disjunction <$> negateResidual a <*> negateResidual b
negateResidual (Disjunction a b) = Conjunction <$> negateResidual a <*> negateResidual b
negateResidual (Implication a b) = Conjunction <$> pure a <*> negateResidual b
negateResidual (Next st (T g e v)) = do
  let st' = case st of AssumeTrue -> AssumeFalse; AssumeFalse -> AssumeTrue; Demand -> Demand
  Next st' <$> newThunk g (App (Var dummyPosition "not_") e)

binaryOp :: PrimOp -> State -> Value -> Value -> Eval Value
binaryOp Implies s v1 v2 = do
  v1' <- force s v1
  case v1' of
    Absurd -> pure Trivial
    Trivial -> force s v2
    Residual r -> do
      v2' <- force s v2
      case v2' of
        Absurd -> Residual <$> negateResidual r
        Trivial -> pure Trivial
        Residual r' -> pure (Residual (Implication r r'))
        _ -> evalError "Implies expects formulae"
    _ -> evalError "Implies expects formulae"
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
        _ -> evalError "And expects formulae"
    _ -> evalError "And expects formulae"
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
        _ -> evalError "Or expects formulae"
    _ -> evalError "Or expects formulae"
binaryOp Equals s v1 v2 = areEqual s v1 v2 >>= \b -> if b then pure Trivial else pure Absurd
binaryOp NotEquals s v1 v2 = areEqual s v1 v2 >>= \b -> if b then pure Absurd else pure Trivial
binaryOp Less s v1 v2 = binaryCmpOp s v1 v2 Less
binaryOp Greater s v1 v2 = binaryCmpOp s v1 v2 Greater
binaryOp LessEq s v1 v2 = binaryCmpOp s v1 v2 LessEq
binaryOp GreaterEq s v1 v2 = binaryCmpOp s v1 v2 GreaterEq
binaryOp Addition s v1 v2 = binaryNumOp s v1 v2 Addition
binaryOp Subtraction s v1 v2 = binaryNumOp s v1 v2 Subtraction
binaryOp Division s v1 v2 = binaryNumOp s v1 v2 Division
binaryOp Multiplication s v1 v2 = binaryNumOp s v1 v2 Multiplication
binaryOp Modulo s v1 v2 = binaryNumOp s v1 v2 Modulo
binaryOp WhenAct s v1 v2 = do
  v2' <- force s v2
  case v2' of
    Trivial -> pure v1
    Absurd -> pure Null
    _ -> evalError "Expected boolean in when condition"
binaryOp TimeoutAct s v1 v2 = do
  v1' <- force s v1
  v2' <- force s v2
  t <- case v2' of
    LitVal (IntLit i) -> pure i
    _ -> evalError "Timeout expects an integer timeout"
  case v1' of
    Action act args _ -> pure (Action act args (Just t))
    _ -> evalError "Timeout expects an action"
binaryOp Map s v1 v2 = do
  v1' <- force s v1
  v2' <- force s v2
  let notNull x = do
        x' <- force s x
        pure $ case x' of Null -> False; _ -> True
  case v2' of
    List ls -> List <$> (filterM notNull =<< mapM (app s v1') ls)
    r -> app s v1' v2'

binaryCmpOp :: State -> Value -> Value -> PrimOp -> IO Value
binaryCmpOp s v1 v2 op = do
  v1' <- force s v1
  v2' <- force s v2
  let toVal x = if x then Trivial else Absurd
  case (v1', v2') of
    (LitVal (IntLit i1), LitVal (IntLit i2))
      | otherwise -> pure (toVal (cmpOp op i1 i2))
    (LitVal (FloatLit i1), LitVal (FloatLit i2))
      | otherwise -> pure (toVal (cmpOp op i1 i2))
    (LitVal (IntLit i1), LitVal (FloatLit i2)) -> binaryCmpOp s (LitVal (FloatLit $ fromIntegral i1)) (LitVal (FloatLit i2)) op
    (LitVal (FloatLit i1), LitVal (IntLit i2)) -> binaryCmpOp s (LitVal (FloatLit i1)) (LitVal (FloatLit $ fromIntegral i2)) op
    _ -> evalError (show op <> " expects numeric types, but got: " <> show v1 <> " and " <> show v2)
  where
    cmpOp Less = (<)
    cmpOp Greater = (>)
    cmpOp LessEq = (<=)
    cmpOp GreaterEq = (>=)

binaryNumOp :: State -> Value -> Value -> PrimOp -> IO Value
binaryNumOp s v1 v2 op = do
  v1' <- force s v1
  v2' <- force s v2
  case (v1', v2') of
    (LitVal (IntLit i1), LitVal (IntLit i2))
      | op == Division -> pure (LitVal (IntLit (i1 `div` i2)))
      | op == Modulo -> pure (LitVal (IntLit (i1 `mod` i2)))
      | otherwise -> pure (LitVal (IntLit (numOp op i1 i2)))
    (LitVal (FloatLit i1), LitVal (FloatLit i2))
      | op == Division -> pure (LitVal (FloatLit (i1 / i2)))
      | op == Modulo -> pure (LitVal (FloatLit (i1 `mod'` i2)))
      | otherwise -> pure (LitVal (FloatLit (numOp op i1 i2)))
    (LitVal (IntLit i1), LitVal (FloatLit i2)) -> binaryNumOp s (LitVal (FloatLit $ fromIntegral i1)) (LitVal (FloatLit i2)) op
    (LitVal (FloatLit i1), LitVal (IntLit i2)) -> binaryNumOp s (LitVal (FloatLit i1)) (LitVal (FloatLit $ fromIntegral i2)) op
    _ -> evalError (show op <> " expects numeric types, but got: " <> show v1 <> " and " <> show v2)
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
resetThunks (Matched t p v) = Matched <$> resetThunk t <*> pure p <*> pure v
resetThunks (Object o) = Object <$> traverse resetThunks o
resetThunks (List o) = List <$> traverse resetThunks o
resetThunks (Action n o t) = Action n <$> traverse resetThunks o <*> pure t
resetThunks (Absurd) = pure Absurd
resetThunks (Trivial) = pure Trivial
resetThunks (Null) = pure Null
resetThunks (LitVal l) = pure $ LitVal l

-- The output value will be Trivial if the formula is now true, Absurd if false,
-- Or another Residual if still requiring more states.
-- Any other value is presumably a type error.
step :: Residual -> State -> Eval Value
step (Next _ t) s = resetThunk t >>= \t' -> forceThunk t' s
step (Implication r1 r2) s = do
  r1' <- step r1 s
  r2' <- step r2 s
  binaryOp Implies s r1' r2'
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
stop (Implication r1 r2) = (\a b -> not a || b) <$> stop r1 <*> stop r2
stop (Disjunction r1 r2) = (||) <$> stop r1 <*> stop r2
