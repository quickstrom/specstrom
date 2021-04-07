{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Specstrom.Evaluator where

import Control.Exception (Exception, throw, throwIO, catch)
import Control.Monad (MonadPlus (mzero), filterM, zipWithM, (<=<))
import Data.Fixed (mod')
import Data.Foldable (foldlM, foldrM)
import qualified Data.HashMap.Strict as M
import Data.IORef
import qualified Data.Text as Text
import qualified Debug.Trace as Debug
import Specstrom.Lexer (Position, dummyPosition)
import Specstrom.Syntax

type Env = M.HashMap Name Value

type State = (Int, (Maybe [Value], M.HashMap Selector Value))

dummyState :: State
dummyState = (-32767, throw . Error $ "Cannot access state in top-level binding")

data Thunk = T Env (Expr TopPattern) (IORef (Maybe (Int, Value)))

instance Show Thunk where
  show _ = "<thunk>"

data Strength = AssumeTrue | AssumeFalse | Demand deriving (Show)

data PrimOp
  = -- unary
    NextF
  | NextT
  | NextD
  | Not
  | -- binary
    And
  | Or
  | Always
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
  | Index
  | -- HOFs (binary)
    Map
  | -- ternary
    IfThenElse
  | Foldr
  | Foldl
  deriving (Show, Eq, Ord, Enum, Bounded)

primOpVar :: PrimOp -> Name
primOpVar op = case op of
  WhenAct -> "_when_"
  TimeoutAct -> "_timeout_"
  NextF -> "nextF_"
  NextT -> "nextT_"
  NextD -> "next_"
  Always -> "always{_}_"
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
  IfThenElse -> "if_{_}else{_}"
  Index -> "nth"
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
      [ (primOpVar op, Op op dummyPosition []) | op <- enumFromTo minBound maxBound
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
    Closure (Name, Position, Int) Env [TopPattern] (Expr TopPattern)
  | Op PrimOp Position [Value]
  | -- thunks (not returned by force)
    Thunk Thunk
  | -- Residual formulae
    Residual Residual
  | -- Actions
    Action Name [Value] (Maybe Int)
  | Constructor Name [Value]
  | -- Values
    Trivial
  | Absurd
  | Null
  | Object Bool (M.HashMap Name Value)
  | List [Value]
  | LitVal Lit
  deriving (Show)

data EvalError = Error String 
               | Backtrace Position Text.Text EvalError
               deriving (Show)


instance Exception EvalError

type Eval = IO

evalError :: String -> Eval a
evalError = throwIO . Error

newThunk :: Env -> Expr TopPattern -> Eval Thunk
newThunk g e = T g e <$> newIORef Nothing

evaluateBind :: State -> Env -> Bind -> Eval Env
evaluateBind s g b = evaluateBind' s g g b

evaluateActionBind :: Env -> Bind -> Eval Env
evaluateActionBind g (Bind (Direct (MacroExpansionTP pat _)) bod) = evaluateActionBind g (Bind (Direct pat) bod)
evaluateActionBind g (Bind (Direct (MatchP (MacroExpansionP pat _))) bod) = evaluateActionBind g (Bind (Direct (MatchP pat)) bod)
evaluateActionBind g (Bind (Direct (MatchP (VarP n _))) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind (Direct (LazyP n _)) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind (FunP n _ _) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind _ _) = evalError "impossible"

evaluateBind' :: State -> Env -> Env -> Bind -> Eval Env
evaluateBind' s g g' (Bind (Direct (MacroExpansionTP pat _)) bod) = evaluateBind' s g g' (Bind (Direct pat) bod)
evaluateBind' s g g' (Bind (Direct (MatchP (MacroExpansionP pat _))) bod) = evaluateBind' s g g' (Bind (Direct (MatchP pat)) bod)
evaluateBind' s g g' (Bind (Direct (LazyP n _)) e) = M.insert n <$> delayedEvaluate s g' e <*> pure g
evaluateBind' s g g' (Bind (Direct (MatchP pat)) e) = do
  t <- evaluate s g' e
  Just g'' <- withPatterns s pat t g
  pure g''
evaluateBind' s g g' (Bind (FunP n p pats) e) = pure (M.insert n (Closure (n, p, 0) g' pats e) g)

withPatterns :: State -> Pattern -> Value -> Env -> Eval (Maybe Env)
withPatterns s (MacroExpansionP p _) v g = withPatterns s p v g
withPatterns s (IgnoreP p) v g = pure (Just g)
withPatterns s (NullP p) v g = do
  v' <- force s v
  case v' of
    Null -> pure (Just g)
    _ -> pure Nothing
withPatterns s (BoolP p l) v g = do
  v' <- force s v
  case v' of
    Absurd | not l -> pure (Just g)
    Trivial | l -> pure (Just g)
    _ -> pure Nothing
withPatterns s (LitP p l) v g = do
  v' <- force s v
  case v' of
    LitVal l' | l == l' -> pure (Just g)
    _ -> pure Nothing
withPatterns s (VarP n p) (Closure (_,_,_) g' pats e) g = pure (Just $ M.insert n (Closure (n,p,0) g' pats e) g)
withPatterns s (VarP n p) v g = pure (Just $ M.insert n v g)
withPatterns s (ObjectP p ps) v g = do
  v' <- force s v
  case v' of
    Object _ fs | all (`elem` M.keys fs) (map fst ps) -> withPatternses s (map snd ps) (map ((\(Just x) -> x) . (fs M.!?) . fst) ps) g
    _ -> pure Nothing
withPatterns s (ListP p ps) v g = do
  v' <- force s v
  case v' of
    List ls | length ls == length ps -> withPatternses s ps ls g
    _ -> pure Nothing
withPatterns s (SymbolP n p ps) v g = do
  v' <- force s v
  case v' of
    Constructor n' ls | n == n' && length ls == length ps -> withPatternses s ps ls g
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

isSelectorLit :: Lit -> Bool
isSelectorLit (SelectorLit {}) = True
isSelectorLit _ = False

delayedEvaluate :: State -> Env -> Expr TopPattern -> Eval Value
-- note that any call to "evaluate" here must not in turn call "force" or consult the state or we will get dangerous strictness.
-- Adding more cases here can make things faster but can also cause incorrectness if it is too strict
delayedEvaluate s g v@(Lam {}) = evaluate s g v
delayedEvaluate s g v@(Var _ n) | Just (Thunk {}) <- M.lookup n g = evaluate s g v
delayedEvaluate s g e = Thunk <$> newThunk g e

appAll :: State -> Value -> [Value] -> Eval Value
appAll s v [] = pure v
appAll s v (x : xs) = force s v >>= \v' -> app s v' x >>= \v' -> appAll s v' xs

-- if the next argument to this thing should be delayed or not
isLazy :: Value -> Bool
isLazy (Op o _ []) | o `elem` [NextT, NextF, NextD, WhenAct] = True
isLazy (Op o _ [_]) | o `elem` [And, Or, Implies, IfThenElse, WhenAct, Always] = True
isLazy (Op IfThenElse _ [_, _]) = True
isLazy (Closure _ _ (LazyP {} : pats) _) = True
isLazy _ = False

app :: State -> Value -> Value -> Eval Value
app s v v2 =
  case v of
    Op o pos [] ->
      if isUnary o
        then backtrace pos (Text.pack $ show o) $ unaryOp o s v2
        else pure (Op o pos [v2])
    Op o pos [v1] ->
      if isBinary o
        then backtrace pos (Text.pack $ show o) $ binaryOp o s v1 v2
        else pure (Op o pos [v1, v2])
    Op o pos [v1, v1'] -> backtrace pos (Text.pack $ show o) $ ternaryOp o s v1 v1' v2
    Action a args t -> do
      pure (Action a (args ++ [v2]) t)
    Constructor a args -> do
      pure (Constructor a (args ++ [v2]))
    Closure (n, p, ai) g' (MacroExpansionTP pat _ : rest) body -> app s (Closure (n, p, ai) g' (pat : rest) body) v2
    Closure (n, p, ai) g' [MatchP pat] body -> do
      mg'' <- withPatterns s pat v2 g'
      case mg'' of
        Nothing -> pure Null
        Just g'' -> backtrace p n $ evaluate s g'' body
    Closure (n, p, ai) g' [LazyP nam pos] body -> do
      mg'' <- withPatterns s (VarP nam pos) v2 g'
      case mg'' of
        Nothing -> pure Null
        Just g'' -> backtrace p n $ evaluate s g'' body
    Closure (n, p, ai) g' (MatchP pat : pats) body -> do
      mg'' <- withPatterns s pat v2 g'
      case mg'' of
        Nothing -> pure Null
        Just g'' -> pure (Closure (n, p, ai + 1) g'' pats body)
    Closure (n, p, ai) g' (LazyP nam pos : pats) body -> do
      mg'' <- withPatterns s (VarP nam pos) v2 g'
      case mg'' of
        Nothing -> pure Null
        Just g'' -> pure (Closure (n, p, ai + 1) g'' pats body)
    Null -> pure Null
    Thunk (T _ e _) -> evalError $ (show e)

evaluate :: State -> Env -> Expr TopPattern -> Eval Value
evaluate s g (Projection e t) = do
  v' <- force s =<< evaluate s g e
  case v' of
    List (Object _ m : _) | Just v <- M.lookup t m -> pure v
    Object _ m | Just v <- M.lookup t m -> pure v
    _ -> evalError "Cannot take projection of a non-object (or list of objects)"
evaluate s g (Symbol _ t) = pure $ Constructor t []
evaluate s g (Var p "happened") = case fst (snd s) of
  Just v -> pure (List v)
  Nothing -> evalError "Set 'happened' of actions is not available when computing actions"
evaluate s g (Var p t) = case M.lookup t g of
  Just (Op o _ []) -> pure (Op o p [])
  Just v -> pure v
  Nothing -> evalError ("Impossible: variable '" <> Text.unpack t <> "' not found in environment (type checker found it though)")
evaluate s g (App e1 e2) = do
  v <- force s =<< evaluate s g e1
  v2 <- (if isLazy v then delayedEvaluate s g else force s <=< evaluate s g) e2 -- TODO avoid thunking everything when safe
  app s v v2
evaluate s g (Literal p (SelectorLit l@(Selector sel))) = case M.lookup l (snd (snd s)) of
  Nothing -> evalError ("Can't find '" <> Text.unpack sel <> "' in the state (analysis failed?)")
  Just ls -> pure ls
evaluate s g (Literal p l) = pure (LitVal l)
evaluate s g (MacroExpansion e _) = evaluate s g e
evaluate s g (Lam p pat e) = pure (Closure ("fun", p, 0) g pat e)
evaluate s g (ListLiteral p ls) = do
  vs <- mapM (force s <=< evaluate s g) ls
  pure (List vs)
evaluate s g (ObjectLiteral p ls) = do
  vs <- mapM (traverse (force s <=< evaluate s g)) ls
  pure (Object True $ M.fromList vs)

forceThunk :: Thunk -> State -> Eval Value
forceThunk (T g e r) s = do
  x <- readIORef r
  case x of
    Just (version, v) | version == fst s -> pure v
    _ -> do
      v <- force s =<< evaluate s g e
      writeIORef r (Just (fst s, v))
      pure v

deepForce :: State -> Value -> Eval Value
deepForce s v = do
  v' <- force s v
  case v' of
    List ls -> List <$> mapM (deepForce s) ls
    Action n ls t -> Action n <$> mapM (deepForce s) ls <*> pure t
    Constructor n ls -> Constructor n <$> mapM (deepForce s) ls
    Object b m -> Object b <$> traverse (deepForce s) m
    _ -> pure v'

force :: State -> Value -> Eval Value
force s (Thunk t) = forceThunk t s
force s v = pure v


ternaryOp :: PrimOp -> State -> Value -> Value -> Value -> Eval Value
ternaryOp IfThenElse s v1' v2 v3 = do
  case v1' of
    Trivial -> pure v2
    Absurd -> pure v3
    _ -> evalError "Expected boolean in if condition"
ternaryOp Foldr s func zero list = do
  case list of
    List ls -> foldrM (\a b -> appAll s func [a, b]) zero ls
    x -> appAll s func [x, zero]
ternaryOp Foldl s func zero list = do
  case list of
    List ls -> foldlM (\a b -> appAll s func [a, b]) zero ls
    x -> appAll s func [zero, x]

backtrace :: Position -> Text.Text -> Eval a -> Eval a
backtrace pos text act = catch act (\(e :: EvalError) -> throwIO (Backtrace pos text e))

areEqual :: State -> Value -> Value -> Eval Bool
areEqual s v1' v2' = areEqual' v1' v2'
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
    areEqual' (Constructor n as) (Constructor m bs) | n == m && length as == length bs = and <$> zipWithM (areEqual s) as bs
    areEqual' (Object b as) (Object b' bs) | b == b' && b, M.keys as == M.keys bs = all snd . M.toList <$> M.traverseWithKey (\k a -> let Just c = M.lookup k bs in areEqual s a c) as
                                           | not b || not b' = evalError "Cannot compare partial objects for equality"
    areEqual' (LitVal l) (LitVal l') = pure $ l == l'
    areEqual' _ _ = pure False

unaryOp :: PrimOp -> State -> Value -> Eval Value
unaryOp Not s v' = do
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
binaryOp Always s v1' v2 =
  case v2 of
    Thunk (T g e _) -> do
      v2' <- force s v2
      case v1' of
        LitVal (IntLit n) -> do
          let mkResidual =
                let nextOp =
                      if n > 0
                        then Next Demand
                        else Next AssumeTrue
                 in nextOp <$> newThunk g (App (App (Var dummyPosition (primOpVar Always)) (Literal dummyPosition (IntLit (n - 1)))) e)
          case v2' of
            Absurd -> pure Absurd
            Trivial -> do
              Residual <$> mkResidual
            Residual r -> do
              residual <- mkResidual
              pure (Residual (Conjunction r residual))
            _ -> evalError ("Always expects formula, got: " <> show v2')
    v -> pure v
binaryOp Implies s v1' v2 = do
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
binaryOp And s v1' v2 = do
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
binaryOp Or s v1' v2 = do
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
binaryOp TimeoutAct s v1' v2' = do
  t <- case v2' of
    LitVal (IntLit i) -> pure i
    _ -> evalError "Timeout expects an integer timeout"
  case v1' of
    Action act args _ -> pure (Action act args (Just t))
    _ -> evalError "Timeout expects an action"
binaryOp Map s v1' v2' = do
  let notNull x = pure $ case x of Null -> False; _ -> True
  case v2' of
    List ls -> List <$> (filterM notNull =<< mapM (app s v1') ls)
    r -> app s v1' v2'
binaryOp Index s e1 e2 = do
  v' <- force s e1
  i' <- force s e2
  case v' of
    List ls -> do
      case i' of
        LitVal (IntLit i) ->
          let l = length ls; i' = if i < 0 then l + i else i
           in if i' < l && i' >= 0
                then pure (ls !! i')
                else pure Null
        _ -> evalError ("Lists are only indexable by integers")
    Object _ m -> do
      case i' of
        LitVal (StringLit s) -> case M.lookup s m of
          Just v -> pure v
          Nothing -> pure Null
        Constructor s [] -> case M.lookup s m of
          Just v -> pure v
          Nothing -> pure Null  
        _ -> evalError ("Objects are only indexable by strings or raw constructors")
    _ -> evalError ("Indexing doesn't work on non-list/object values")

binaryCmpOp :: State -> Value -> Value -> PrimOp -> IO Value
binaryCmpOp s v1' v2' op = do
  let toVal x = if x then Trivial else Absurd
  case (v1', v2') of
    (LitVal (IntLit i1), LitVal (IntLit i2))
      | otherwise -> pure (toVal (cmpOp op i1 i2))
    (LitVal (FloatLit i1), LitVal (FloatLit i2))
      | otherwise -> pure (toVal (cmpOp op i1 i2))
    (LitVal (IntLit i1), LitVal (FloatLit i2)) -> binaryCmpOp s (LitVal (FloatLit $ fromIntegral i1)) (LitVal (FloatLit i2)) op
    (LitVal (FloatLit i1), LitVal (IntLit i2)) -> binaryCmpOp s (LitVal (FloatLit i1)) (LitVal (FloatLit $ fromIntegral i2)) op
    _ -> evalError (show op <> " expects numeric types, but got: " <> show v1' <> " and " <> show v2')
  where
    cmpOp Less = (<)
    cmpOp Greater = (>)
    cmpOp LessEq = (<=)
    cmpOp GreaterEq = (>=)

binaryNumOp :: State -> Value -> Value -> PrimOp -> IO Value
binaryNumOp s v1' v2' op = do
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
    _ -> evalError (show op <> " expects numeric types, but got: " <> show v1' <> " and " <> show v2')
  where
    numOp Addition = (+)
    numOp Subtraction = (-)
    numOp Multiplication = (*)

-- The output value will be Trivial if the formula is now true, Absurd if false,
-- Or another Residual if still requiring more states.
-- Any other value is presumably a type error.
step :: Residual -> State -> Eval Value
step (Next _ t) s = forceThunk t s
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
