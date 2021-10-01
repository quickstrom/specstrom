{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Specstrom.Evaluator where

import Control.Exception (Exception, catch, throw, throwIO)
import Control.Monad (MonadPlus (mzero), filterM, zipWithM, (<=<))
import Data.Fixed (mod')
import Data.Foldable (foldlM, foldrM)
import qualified Data.HashMap.Strict as M
import Data.IORef
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Data.Vector (unfoldrM)
import qualified Data.Vector as Vector
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
  | ParseInt
  | ParseFloat
  | IsNull
  | ZipAll
  | StringTrim
  | -- binary
    And
  | Or
  | Always
  | Eventually
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
  | Zip
  | StringSplit
  | -- HOFs (binary)
    Map
  | Unfoldr
  | -- ternary
    IfThenElse
  | Foldr
  | Foldl
  | Until
  | Release
  deriving (Show, Eq, Ord, Enum, Bounded)

primOpVar :: PrimOp -> Name
primOpVar op = case op of
  WhenAct -> "_when_"
  TimeoutAct -> "_timeout_"
  NextF -> "nextF_"
  NextT -> "nextT_"
  NextD -> "next_"
  Always -> "always{_}_"
  Eventually -> "eventually{_}_"
  Until -> "_until{_}_"
  Release -> "_release{_}_"
  Not -> "not_"
  IsNull -> "isNull"
  ParseInt -> "parseInt"
  ParseFloat -> "parseFloat"
  ZipAll -> "zipAll"
  StringTrim -> "trim"
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
  Zip -> "zip"
  StringSplit -> "split"
  Map -> "map"
  Unfoldr -> "unfoldr"
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
isUnary = (<= StringTrim)

isBinary :: PrimOp -> Bool
isBinary x = not (isUnary x) && x <= Unfoldr


data DerivedFormula 
  = AlwaysF Int Thunk
  | EventuallyF Int Thunk 
  | UntilF Int Thunk Thunk
  | ReleaseF Int Thunk Thunk
  | F Thunk
  deriving (Show)
data Residual
  = Next Strength DerivedFormula
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

data EvalError
  = Error String
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
evaluateActionBind g (Bind (MacroExpansionBP pat _) bod) = evaluateActionBind g (Bind pat bod)
evaluateActionBind g (Bind (Direct (MatchP (MacroExpansionP pat _))) bod) = evaluateActionBind g (Bind (Direct (MatchP pat)) bod)
evaluateActionBind g (Bind (Direct (MatchP (VarP n _))) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind (Direct (LazyP n _)) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind (FunP n _ _) _) = pure (M.insert n (Action n [] Nothing) g)
evaluateActionBind g (Bind _ _) = evalError "impossible"

evaluateBind' :: State -> Env -> Env -> Bind -> Eval Env
evaluateBind' s g g' (Bind (Direct (MacroExpansionTP pat _)) bod) = evaluateBind' s g g' (Bind (Direct pat) bod)
evaluateBind' s g g' (Bind (MacroExpansionBP pat _) bod) = evaluateBind' s g g' (Bind pat bod)
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
withPatterns s (VarP n p) (Closure (_, _, _) g' pats e) g = pure (Just $ M.insert n (Closure (n, p, 0) g' pats e) g)
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
appAll s v (x : xs) = force s v >>= \v' -> app s v' x >>= \v'' -> appAll s v'' xs

-- if the next argument to this thing should be delayed or not
isLazy :: Value -> Bool
isLazy (Op o _ []) | o `elem` [NextT, NextF, NextD, WhenAct, Until] = True
isLazy (Op o _ [_]) | o `elem` [And, Or, Implies, IfThenElse, WhenAct, Always] = True
isLazy (Op Until _ [_,_]) = True
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
    _ -> error "Impossible?"

evaluate :: State -> Env -> Expr TopPattern -> Eval Value
evaluate s g (Projection e t) = do
  v' <- force s =<< evaluate s g e
  let lookupIn m =
        case M.lookup t m of
          Just v -> pure v
          Nothing -> evalError ("Field '" <> Text.unpack t <> "' is not present in object: " <> show m)
  case v' of
    List (Object _ m : _) -> lookupIn m
    Object _ m -> lookupIn m
    _ -> evalError "Cannot take projection of a non-object (or list of objects)"
evaluate s g (Symbol _ t) = pure $ Constructor t []
evaluate s g (Var p "happened") = case fst (snd s) of
  Just v -> pure (List v)
  Nothing -> evalError "Set 'happened' of actions is not available when computing actions"
evaluate s g (Var p t) = case M.lookup t g of
  Just (Op o _ []) -> pure (Op o p [])
  Just v -> pure v
  Nothing -> error ("Impossible: variable '" <> Text.unpack t <> "' not found in environment (type checker found it though)")
evaluate s g (App e1 e2) = do
  v <- force s =<< evaluate s g e1
  v2 <- (if isLazy v then delayedEvaluate s g else force s <=< evaluate s g) e2 -- TODO avoid thunking everything when safe
  app s v v2
evaluate s g (Literal p (SelectorLit l@(Selector sel))) = case M.lookup l (snd (snd s)) of
  Nothing -> evalError ("Can't find '" <> Text.unpack sel <> "' in the state (analysis failed?)")
  Just ls -> pure ls
evaluate s g (Literal p l) = pure (LitVal l)
evaluate s g (MacroExpansion e _) = evaluate s g e
evaluate s g (Lam p pat e) = pure (Closure ("<fun>", p, 0) g pat e)
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
ternaryOp Release s f1 v f2 = do
  case (f1, f2) of
    (Thunk (T g e _), Thunk (T g' e' _)) -> do
      f2' <- force s f2
      case v of
        LitVal (IntLit n) -> do
          let mkResidual =
                let recur strength n' = Next strength <$> (ReleaseF n' <$> newThunk g e <*> newThunk g' e')
                 in if n > 0
                      then recur Demand (n - 1)
                      else recur AssumeFalse 0
          case f2' of
            Absurd -> pure Absurd
            Trivial -> do
              f1' <- force s f1
              case f1' of 
                Trivial -> pure Trivial
                Absurd -> Residual <$> mkResidual
                Residual r -> do 
                  residual <- mkResidual
                  pure (Residual (Disjunction r residual))
            Residual r -> do
              f1' <- force s f1
              case f1' of 
                Trivial -> pure (Residual r)
                Absurd -> Residual . Conjunction r <$> mkResidual
                Residual r' -> do 
                  residual <- mkResidual
                  pure (Residual (Conjunction r (Disjunction r' residual)))
            _ -> evalError ("Release expects formula, got: " <> show f2')
        _ -> evalError $ "Release count argument was not an integer"
    v -> evalError "Release argument was already evaluated!"
ternaryOp Until s f1 v f2 = do
  case (f1, f2) of
    (Thunk (T g e _), Thunk (T g' e' _)) -> do
      f2' <- force s f2
      case v of
        LitVal (IntLit n) -> do
          let mkResidual =
                let recur strength n' = Next strength <$> (UntilF n' <$> newThunk g e <*> newThunk g' e')
                 in if n > 0
                      then recur Demand (n - 1)
                      else recur AssumeTrue 0
          case f2' of
            Absurd -> do
              f1' <- force s f1
              case f1' of 
                Absurd -> pure Absurd
                Trivial -> Residual <$> mkResidual
                Residual r -> do 
                  residual <- mkResidual
                  pure (Residual (Conjunction r residual))
            Trivial -> pure Trivial
            Residual r -> do
              f1' <- force s f1
              case f1' of 
                Absurd -> pure (Residual r)
                Trivial -> Residual . Disjunction r <$> mkResidual
                Residual r' -> do 
                  residual <- mkResidual
                  pure (Residual (Disjunction r (Conjunction r' residual)))
            _ -> evalError ("Until expects formula, got: " <> show f2')
        _ -> evalError $ "Until count argument was not an integer"
    v -> evalError "Until argument was already evaluated!"
ternaryOp IfThenElse s v1' v2 v3 = do
  case v1' of
    Trivial -> force s v2
    Absurd -> force s v3
    _ -> evalError "Expected boolean in if condition"
ternaryOp Foldr s func zero list = do
  case list of
    List ls -> foldrM (\a b -> appAll s func [a, b]) zero ls
    x -> appAll s func [x, zero]
ternaryOp Foldl s func zero list = do
  case list of
    List ls -> foldlM (\a b -> appAll s func [a, b]) zero ls
    x -> appAll s func [zero, x]
ternaryOp _ _ _ _ _ = error "Impossible (ternary)"

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
    areEqual' (Object b as) (Object b' bs)
      | b == b' && b, M.keys as == M.keys bs = all snd . M.toList <$> M.traverseWithKey (\k a -> let Just c = M.lookup k bs in areEqual s a c) as
      | not b || not b' = evalError "Cannot compare partial objects for equality"
    areEqual' (LitVal l) (LitVal l') = pure $ l == l'
    areEqual' _ _ = pure False

unaryOp :: PrimOp -> State -> Value -> Eval Value
unaryOp IsNull s v = case v of Null -> pure Trivial; _ -> pure Absurd
unaryOp Not s v' = do
  case v' of
    Absurd -> pure Trivial
    Trivial -> pure Absurd
    Residual f -> Residual <$> negateResidual f
    _ -> evalError ("Not expects boolean, got: " <> show v')
unaryOp ParseInt s (LitVal (StringLit s'))
  | Right (v, residue) <- Text.decimal s' =
    if Text.null residue
      then pure (LitVal (IntLit v))
      else evalError ("parseInt could parse prefix but string has non-parsable suffix: " <> show residue)
  | otherwise = evalError ("parseInt could not parse: " <> show s')
unaryOp ParseFloat s (LitVal (StringLit s'))
  | Right (v, residue) <- Text.double s' =
    if Text.null residue
      then pure (LitVal (FloatLit v))
      else evalError ("parseFloat could parse prefix but string has non-parsable suffix: " <> show residue)
  | otherwise = evalError ("parseFloat could not parse: " <> show s')
unaryOp StringTrim _ (LitVal (StringLit s')) = pure (LitVal (StringLit (Text.strip s')))
unaryOp ZipAll s xs = do
  xs' <- force s xs
  case xs' of
    (List lists) -> do
      let maxIndex = minimum [length xs | List xs <- lists] - 1
          at i (List vs) = pure (vs !! i)
          at i _ = evalError "zipAll requires a list of lists as its argument"
      List <$> traverse (\i -> List <$> traverse (at i) lists) [0 .. maxIndex]
    _ -> evalError "zipAll requires a list of lists as its argument"
unaryOp NextF s (Thunk t) = pure (Residual (Next AssumeFalse (F t)))
unaryOp NextT s (Thunk t) = pure (Residual (Next AssumeTrue (F t)))
unaryOp NextD s (Thunk t) = pure (Residual (Next Demand (F t)))
unaryOp op _ arg = evalError ("Impossible unary operation " <> show op <> " on: " <> show arg)

negateResidual :: Residual -> Eval Residual
negateResidual (Conjunction a b) = Disjunction <$> negateResidual a <*> negateResidual b
negateResidual (Disjunction a b) = Conjunction <$> negateResidual a <*> negateResidual b
negateResidual (Implication a b) = Conjunction <$> pure a <*> negateResidual b
negateResidual (Next st f) = do
  let st' = case st of AssumeTrue -> AssumeFalse; AssumeFalse -> AssumeTrue; Demand -> Demand
  Next st' <$> negateDerivedFormula f
negateDerivedFormula :: DerivedFormula -> Eval DerivedFormula
negateDerivedFormula (AlwaysF n phi) = EventuallyF n <$> negateThunk phi
negateDerivedFormula (EventuallyF n phi) = AlwaysF n <$> negateThunk phi
negateDerivedFormula (UntilF n phi psi) = ReleaseF n <$> negateThunk phi <*> negateThunk psi
negateDerivedFormula (ReleaseF n phi psi) = UntilF n <$> negateThunk phi <*> negateThunk psi
negateDerivedFormula (F phi) = F <$> negateThunk phi
negateThunk :: Thunk -> Eval Thunk
negateThunk (T g e _) = newThunk g (App (Var dummyPosition "not_") e)

binaryOp :: PrimOp -> State -> Value -> Value -> Eval Value
binaryOp Eventually s v1' v2 =
  case v2 of
    Thunk (T g e _) -> do
      v2' <- force s v2
      case v1' of
        LitVal (IntLit n) -> do
          let mkResidual =
                let recur strength n' = Next strength . EventuallyF n' <$> newThunk g e
                 in if n > 0
                      then recur Demand (n - 1)
                      else recur AssumeFalse 0
          case v2' of
            Trivial -> pure Trivial
            Absurd -> do
              Residual <$> mkResidual
            Residual r -> do
              residual <- mkResidual
              pure (Residual (Disjunction r residual))
            _ -> evalError ("Eventually expects formula, got: " <> show v2')
        _ -> evalError $ "Eventually count argument was not an integer"
    v -> evalError "Eventually argument was already evaluated!"
binaryOp Always s v1' v2 =
  case v2 of
    Thunk (T g e _) -> do
      v2' <- force s v2
      case v1' of
        LitVal (IntLit n) -> do
          let mkResidual =
                let recur strength n' = Next strength . AlwaysF n' <$> newThunk g e
                 in if n > 0
                      then recur Demand (n - 1)
                      else recur AssumeTrue 0
          case v2' of
            Absurd -> pure Absurd
            Trivial -> do
              Residual <$> mkResidual
            Residual r -> do
              residual <- mkResidual
              pure (Residual (Conjunction r residual))
            _ -> evalError ("Always expects formula, got: " <> show v2')
        _ -> evalError $ "Always count argument was not an integer"
    v -> evalError "Always argument was already evaluated!"
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
        _ -> evalError ("And expects formulae, got as second argument: " <> show v2')
    _ -> evalError ("And expects formulae, got as first argument: " <> show v1')
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
  -- force shouldn't be needed here but added just in case
  let notNull x = force s x >>= \x' -> pure $ case x' of Null -> False; _ -> True
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
          let l = length ls
              i'' = if i < 0 then l + i else i
           in if i'' < l && i'' >= 0
                then pure (ls !! i'')
                else pure Null
        _ -> evalError "Lists are only indexable by integers"
    Object _ m -> do
      case i' of
        LitVal (StringLit str) -> case M.lookup str m of
          Just v -> pure v
          Nothing -> pure Null
        Constructor str [] -> case M.lookup str m of
          Just v -> pure v
          Nothing -> pure Null
        _ -> evalError ("Objects are only indexable by strings or raw constructors, got: " <> show i')
    _ -> evalError ("Indexing doesn't work on non-list/object values: " <> show v')
binaryOp Zip s e1 e2 = do
  xs' <- force s e1
  ys' <- force s e2
  case (xs', ys') of
    (List xs, List ys) -> do
      pure (List (zipWith (\x y -> List [x, y]) xs ys))
    _ -> evalError "zip requires two lists as arguments"
binaryOp StringSplit s e1 e2 = do
  needle' <- force s e1
  haystack' <- force s e2
  case (needle', haystack') of
    (LitVal (StringLit needle''), LitVal (StringLit haystack'')) ->
      pure (List (map (LitVal . StringLit) (Text.splitOn needle'' haystack'')))
    _ -> evalError $ "`split` expects two strings (needle and haystack)"
binaryOp Unfoldr s func start = do
  vs <-
    unfoldrM
      ( \v -> do
          f <- force s func
          v' <- force s =<< app s f v
          case v' of
            List [hd, next] -> pure (Just (hd, next))
            Null -> pure Nothing
            _ -> evalError $ "`unfoldr` expected a list of two values or null"
      )
      start
  pure (List (Vector.toList vs))
binaryOp _ _ _ _ = error "Impossible (binaryop)"

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
    cmpOp _ = error "Impossible"

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
    numOp _ = error "Impossible"

-- The output value will be Trivial if the formula is now true, Absurd if false,
-- Or another Residual if still requiring more states.
-- Any other value is presumably a type error.
step :: Residual -> State -> Eval Value
step (Next _ t) s = stepDerivedFormula t s
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

stepDerivedFormula :: DerivedFormula -> State -> Eval Value
stepDerivedFormula (UntilF n phi psi) s = ternaryOp Until s (Thunk phi) (LitVal (IntLit n)) (Thunk psi)
stepDerivedFormula (ReleaseF n phi psi) s = ternaryOp Release s  (Thunk phi) (LitVal (IntLit n)) (Thunk psi)
stepDerivedFormula (AlwaysF n phi) s = binaryOp Always s (LitVal (IntLit n)) (Thunk phi)
stepDerivedFormula (EventuallyF n phi) s = binaryOp Eventually s (LitVal (IntLit n)) (Thunk phi)
stepDerivedFormula (F phi) s = forceThunk phi s

stop :: Residual -> Maybe Bool
stop (Next s t) = case s of
  AssumeTrue -> pure True
  AssumeFalse -> pure False
  Demand -> mzero
stop (Conjunction r1 r2) = (&&) <$> stop r1 <*> stop r2
stop (Implication r1 r2) = (\a b -> not a || b) <$> stop r1 <*> stop r2
stop (Disjunction r1 r2) = (||) <$> stop r1 <*> stop r2
