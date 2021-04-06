{-# LANGUAGE OverloadedStrings #-}

module Specstrom.TypeInf where

import Control.Monad.Except
import Control.Monad.Gen
import Data.List (nub, (\\))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Specstrom.Lexer (Position)
import Specstrom.Syntax

type Context = M.Map Name QType

newtype Subst = Subst [(Name, Type)]

substGamma :: Subst -> Context -> Context
substGamma = fmap . substQType

substQType :: Subst -> QType -> QType
substQType s (Ty t) = Ty (subst s t)
substQType s (Forall x t) = Forall x (substQType (remove s) t)
  where
    remove (Subst sub) = Subst $ filter ((/= x) . fst) sub

subst :: Subst -> Type -> Type
subst s Value = Value
subst s (Arrow a b) = Arrow (subst s a) (subst s b)
subst (Subst s) (TyVar x)
  | Just t <- lookup x s = t
  | otherwise = TyVar x

instance Semigroup Subst where
  Subst a <> Subst b =
    Subst $
      map (fmap $ subst $ Subst b) a
        ++ map (fmap $ subst $ Subst a) b

instance Monoid Subst where
  mempty = Subst []
  mappend = (<>)

(=:) :: Name -> Type -> Subst
a =: b = Subst [(a, b)]

builtInTypes :: Context
builtInTypes =
  M.fromList
    [ ("_==_", val3),
      ("_!=_", val3),
      ("true", Ty Value),
      ("false", Ty Value),
      ("null", Forall "a" (Ty (TyVar "a"))),
      ("happened", Ty Value),
      ("_when_", val3),
      ("_timeout_", val3),
      ("_||_", val3),
      ("_&&_", val3),
      ("_==>_", val3),
      ("_>=_", val3),
      ("_<=_", val3),
      ("_<_", val3),
      ("_>_", val3),
      ("_+_", val3),
      ("_-_", val3),
      ("_/_", val3),
      ("_*_", val3),
      ("_%_", val3),
      ("nextF_", val2),
      ("nextT_", val2),
      ("next_", val2),
      ("always{_}_", val3),
      ("not_", val2),
      ("map", hof),
      ("foldl", hof2),
      ("foldr", hof2),
      ("if_{_}else{_}", Forall "a" (Ty (Arrow Value (Arrow (TyVar "a") (Arrow (TyVar "a") (TyVar "a"))))))
    ]
  where
    val3 = Ty (Arrow Value (Arrow Value Value))
    val2 = Ty (Arrow Value Value)
    hof = Ty (Arrow (Arrow Value Value) (Arrow Value Value))
    hof2 = Ty (Arrow (Arrow Value (Arrow Value Value)) (Arrow Value (Arrow Value Value)))

data Type = Arrow Type Type | Value | TyVar Name deriving (Show)

data QType = Forall Name QType | Ty Type deriving (Show)

data TypeErrorBit = StrE Text | VarNameE Name | TypeE Type | PtnE Pattern

type TC = ExceptT (Position, [TypeErrorBit]) (Gen Integer)

runTC :: TC a -> Either (Position, [TypeErrorBit]) a
runTC tc = runGen (runExceptT tc)

typeError :: Position -> [TypeErrorBit] -> TC a
typeError p es = throwError (p, es)

fresh :: TC Type
fresh = do
  x <- gen
  pure (TyVar $ T.pack $ show x)

instantiate :: Subst -> QType -> TC Type
instantiate s (Ty t) = pure (subst s t)
instantiate s (Forall n qt) = do
  alpha <- fresh
  instantiate (s <> (n =: alpha)) qt

generalise :: Context -> Type -> QType
generalise g t =
  let gs = nub (tvGamma g)
      ts = nub (tv t)
   in quantify (ts \\ gs) t
  where
    quantify [] ty = Ty ty
    quantify (x : xs) ty = Forall x (quantify xs ty)

unify :: Position -> Type -> Type -> TC Subst
unify p (Arrow e1 e2) (Arrow t1 t2) = do
  s1 <- unify p e1 t1
  s2 <- unify p (subst s1 e2) (subst s1 t2)
  pure (s1 <> s2)
unify p Value Value = pure mempty
unify p (TyVar n) (TyVar m)
  | n == m = pure mempty
  | otherwise = pure (n =: TyVar m)
unify p (TyVar n) t
  | n `elem` tv t = typeError p [StrE "Cannot construct infinite type (occurs check fail)", VarNameE n, TypeE t]
  | otherwise = pure (n =: t)
unify p t (TyVar n) = unify p (TyVar n) t
unify p t1 t2 = typeError p [StrE "Type mismatch, cannot unify", TypeE t1, StrE "and", TypeE t2]

tv :: Type -> [Name]
tv (Arrow e1 e2) = tv e1 ++ tv e2
tv (TyVar v) = [v]
tv Value = []

tvQ :: QType -> [Name]
tvQ (Ty t) = tv t
tvQ (Forall n t) = filter (/= n) (tvQ t)

tvGamma :: Context -> [Name]
tvGamma = foldMap tvQ

inferActionBind :: Context -> Bind -> TC (Context, Subst)
inferActionBind g (Bind (Direct (MacroExpansionTP p _)) bod) = inferActionBind g (Bind (Direct p) bod)
inferActionBind g (Bind (Direct (MatchP (MacroExpansionP p _))) bod) = inferActionBind g (Bind (Direct (MatchP p)) bod)
inferActionBind g (Bind (Direct (LazyP n p)) bod) = do
  (t, s) <- inferExp g bod
  ss <- unify p t Value
  pure (M.insert n (Ty Value) (substGamma (s <> ss) g), s <> ss)
inferActionBind g (Bind (Direct (MatchP (VarP n p))) bod) = do
  (t, s) <- inferExp g bod
  ss <- unify p t Value
  pure (M.insert n (Ty Value) (substGamma (s <> ss) g), s <> ss)
inferActionBind g (Bind (FunP n _ lams) bod) = do
  (t, s) <- inferActionFun g lams bod
  pure (M.insert n (Ty t) (substGamma s g), s)
inferActionBind g (Bind (Direct (MatchP p)) bod) = typeError (patternPos p) [StrE "Action binding cannot use a pattern of the form", PtnE p]

inferBind :: Context -> Bind -> TC (Context, Subst)
inferBind g (Bind (Direct (MacroExpansionTP p _)) bod) = inferBind g (Bind (Direct p) bod)
inferBind g (Bind (Direct (LazyP n _)) bod) = do
  (t, s) <- inferExp g bod
  let qt = generalise g t
  pure (M.insert n qt (substGamma s g), s)
inferBind g (Bind (Direct (MatchP (MacroExpansionP p _))) bod) = inferBind g (Bind (Direct (MatchP p)) bod)
inferBind g (Bind (Direct (MatchP (VarP n _))) bod) = do
  (t, s) <- inferExp g bod
  let qt = generalise g t
  pure (M.insert n qt (substGamma s g), s)
inferBind g (Bind (Direct (MatchP (IgnoreP _))) bod) = do
  (t, s) <- inferExp g bod
  pure (substGamma s g, s)
inferBind g (Bind (Direct (MatchP (NullP _))) bod) = do
  (t, s) <- inferExp g bod
  pure (substGamma s g, s)
inferBind g (Bind (Direct (MatchP pat)) bod) = do
  (t, s) <- inferExp g bod
  ss <- unify (exprPos bod) t Value
  pure (M.union (M.fromList (zip (patternVars pat) (repeat (Ty Value)))) (substGamma (s <> ss) g), s <> ss)
inferBind g (Bind (FunP n _ lams) bod) = do
  (t, s) <- inferFun g lams bod
  let qt = generalise g t
  pure (M.insert n qt (substGamma s g), s)

inferActionFun :: Context -> [TopPattern] -> Expr TopPattern -> TC (Type, Subst)
inferActionFun g [] bod = do
  (t, s) <- inferExp g bod
  ss <- unify (exprPos bod) t Value
  pure (Value, s <> ss)
inferActionFun g (pat : rest) bod = do
  let g' = M.union (M.fromList (zip (topPatternVars pat) (repeat (Ty Value)))) g
  (t, s) <- inferActionFun g' rest bod
  pure (Arrow Value t, s)

inferFun :: Context -> [TopPattern] -> Expr TopPattern -> TC (Type, Subst)
inferFun g [] bod = inferExp g bod
inferFun g (MacroExpansionTP pat _ : rest) bod = inferFun g (pat:rest) bod
inferFun g (MatchP (MacroExpansionP p _):rest) bod = inferFun g (MatchP p:rest) bod
inferFun g (MatchP (IgnoreP _) : rest) bod = do
  alpha <- fresh
  (t, s) <- inferFun g rest bod
  pure (Arrow (subst s alpha) t, s)
inferFun g (MatchP (NullP _) : rest) bod = do
  alpha <- fresh
  (t, s) <- inferFun g rest bod
  pure (Arrow (subst s alpha) t, s)
inferFun g (LazyP n _ : rest) bod = do
  alpha <- fresh
  (t, s) <- inferFun (M.insert n (Ty alpha) g) rest bod
  pure (Arrow (subst s alpha) t, s)
inferFun g (MatchP (VarP n _) : rest) bod = do
  alpha <- fresh
  (t, s) <- inferFun (M.insert n (Ty alpha) g) rest bod
  pure (Arrow (subst s alpha) t, s)
inferFun g (MatchP pat : rest) bod = do
  let g' = M.union (M.fromList (zip (patternVars pat) (repeat (Ty Value)))) g
  (t, s) <- inferFun g' rest bod
  pure (Arrow Value t, s)

inferExp :: Context -> Expr TopPattern -> TC (Type, Subst)
inferExp g (MacroExpansion e _) = inferExp g e
inferExp g (Projection e _) = do
  (t, s) <- inferExp g e
  s' <- unify (exprPos e) t Value
  pure (Value, s <> s')
inferExp g (Var pos t) = case M.lookup t g of
  Nothing -> typeError pos [StrE "Variable not in scope: ", VarNameE t]
  Just qt -> do
    ret <- instantiate mempty qt
    pure (ret, mempty)
inferExp g (Symbol pos n) = pure (Value, mempty)
inferExp g e | (Symbol pos n, as) <- peelAps e [] = do
  ss <- inferExpsValue g as
  pure (Value, ss)
inferExp g (App e1 e2) = do
  (t1, s1) <- inferExp g e1
  (t2, s2) <- inferExp (substGamma s1 g) e2
  alpha <- fresh
  s3 <- unify (exprPos e1) (subst s2 t1) (Arrow t2 alpha)
  pure (subst s3 alpha, s1 <> s2 <> s3)
inferExp g (Lam p pat e) = inferFun g [pat] e
inferExp g (Literal {}) = pure (Value, mempty)
inferExp g (ListLiteral _ es) = do
  ss <- inferExpsValue g es
  pure (Value, ss)
inferExp g (ObjectLiteral p es) =
  let ns = map fst es
   in if ns /= nub ns
        then typeError p $ [StrE "Duplicate fields in object literal:"] ++ map VarNameE (ns \\ nub ns)
        else do
          ss <- inferExpsValue g (map snd es)
          pure (Value, ss)
inferExp g (Freeze _ (LazyP n _) e1 e2) = do
  (t1, s1) <- inferExp g e1
  (t2, s2) <- inferExp (M.insert n (Ty t1) (substGamma s1 g)) e2
  pure (t2, s1 <> s2)
inferExp g (Freeze _ (MatchP (VarP n _)) e1 e2) = do
  (t1, s1) <- inferExp g e1
  (t2, s2) <- inferExp (M.insert n (Ty t1) (substGamma s1 g)) e2
  pure (t2, s1 <> s2)
inferExp g (Freeze _ (MatchP (IgnoreP _)) e1 e2) = do
  (t1, s1) <- inferExp g e1
  (t2, s2) <- inferExp (substGamma s1 g) e2
  pure (t2, s1 <> s2)
inferExp g (Freeze p pat e1 e2) = do
  (t1, s1) <- inferExp g e1
  ss <- unify p t1 Value
  let g' = M.union (M.fromList (zip (topPatternVars pat) (repeat (Ty Value)))) (substGamma (s1 <> ss) g)
  (t2, s2) <- inferExp g' e2
  pure (t2, s1 <> ss <> s2)
inferExp g (Index e1 e2) = do
  (t1, s1) <- inferExp g e1
  s2 <- unify (exprPos e1) t1 Value
  (t2, s3) <- inferExp (substGamma (s1 <> s2) g) e2
  s4 <- unify (exprPos e1) t2 Value
  pure (t2, s1 <> s2 <> s3 <> s4)

inferExpsValue :: Context -> [Expr TopPattern] -> TC Subst
inferExpsValue g [] = pure mempty
inferExpsValue g (e : es) = do
  (t, s) <- inferExp g e
  s' <- unify (exprPos e) t Value
  let ss = s <> s'
  s'' <- inferExpsValue (substGamma ss g) es
  pure (ss <> s'')

inferExpImmediate :: Context -> Expr TopPattern -> Either (Position, [TypeErrorBit]) Type
inferExpImmediate g e = runTC (fst <$> inferExp g e)

inferTopLevels :: Context -> [TopLevel] -> Either (Position, [TypeErrorBit]) Context
inferTopLevels g [] = pure g
inferTopLevels g (Binding b : rest) = do
  g' <- runTC (fst <$> inferBind g b)
  inferTopLevels g' rest
inferTopLevels g (ActionDecl b : rest) = do
  g' <- runTC (fst <$> inferActionBind g b)
  inferTopLevels g' rest
inferTopLevels g (Imported t ts : rest) = do
  g' <- inferTopLevels g ts
  inferTopLevels g' rest
inferTopLevels g (Properties pos g1 g2 Nothing : rest) = inferTopLevels g rest
inferTopLevels g (Properties pos g1 g2 (Just t) : rest) = do
  _ <- runTC (inferExp g t)
  inferTopLevels g rest
