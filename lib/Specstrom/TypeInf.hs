{-# LANGUAGE OverloadedStrings #-}
module Specstrom.TypeInf where
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text(Text)
import Control.Monad.Gen
import Control.Monad.Except
import Specstrom.Syntax
import Specstrom.Lexer(Position)
import Data.List(nub,(\\))
type Context = M.Map Name QType

newtype Subst = Subst [(Name, Type)]

substGamma :: Subst -> Context -> Context
substGamma = fmap . substQType

substQType :: Subst -> QType -> QType
substQType s (Ty t) = Ty (subst s t)
substQType s (Forall x t) = Forall x (substQType (remove x s) t)
  where remove x (Subst s) = Subst $ filter ((/= x) . fst) s

subst :: Subst -> Type -> Type
subst s Value = Value
subst s (Arrow a b) = Arrow (subst s a) (subst s b)
subst (Subst s) (TyVar x) | Just t <- lookup x s = t
                          | otherwise = TyVar x

instance Semigroup Subst where
  Subst a <> Subst b = Subst $ map (fmap $ subst $ Subst b) a
                            ++ map (fmap $ subst $ Subst a) b


instance Monoid Subst where
  mempty = Subst []
  mappend = (<>)


(=:) :: Name -> Type -> Subst
a =: b = Subst [(a,b)]

builtInTypes :: Context
builtInTypes = M.fromList
    [   ("_==_",val3),
        ("_!=_",val3),
        ("true", Ty Value),
        ("false", Ty Value),
        ("null", Ty Value),
        ("noop!", Ty Value),
        ("loaded?", Ty Value),
        ("_when_",val3),
        ("_timeout_",val3),
        ("_||_",val3),
        ("_&&_",val3),
        ("nextF_",val2),
        ("nextT_",val2),
        ("next_",val2),
        ("always_",val2),
        ("not_",val2),
        ("click!",val2),
        ("changed?",val2),
        ("if_then_else_", Forall "a" (Ty (Arrow Value (Arrow (TyVar "a") (Arrow (TyVar "a") (TyVar "a"))))))
    ]
 where 
     val3 = Ty (Arrow Value (Arrow Value Value))
     val2 = Ty (Arrow Value Value)

data Type = Arrow Type Type | Value | TyVar Name

data QType = Forall Name QType | Ty Type

data TypeErrorBit = StrE Text | VarNameE Name | TypeE Type

type TC = ExceptT (Position, [TypeErrorBit]) (Gen Integer)

runTC :: TC a -> Either (Position, [TypeErrorBit]) a
runTC tc = runGen (runExceptT tc)

typeError :: Position -> [TypeErrorBit] -> TC a
typeError p es = throwError (p,es)

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
generalise g t = let gs = nub (tvGamma g)
                     ts = nub (tv t)
                  in quantify (gs \\ ts) t
  where
    quantify [] t = Ty t
    quantify (x:xs) t = Forall x (quantify xs t)

unify :: Position -> Type -> Type -> TC Subst
unify p (Arrow e1 e2) (Arrow t1 t2) = do
    s1 <- unify p e1 t1
    s2 <- unify p (subst s1 e2) (subst s1 t2)
    pure (s1 <> s2)
unify p Value Value = pure mempty
unify p (TyVar n) (TyVar m) 
  | n == m = pure mempty
  | otherwise = pure (n =: TyVar m)
unify p (TyVar n) t | n `elem` tv t = typeError p [StrE "Cannot construct infinite type (occurs check fail)"]
                    | otherwise = pure (n =: t)
unify p t (TyVar n) = unify p (TyVar n) t
unify p t1 t2 = typeError p [StrE "Type mismatch, cannot unify", TypeE t1, StrE "and", TypeE t2]

tv :: Type -> [Name]
tv (Arrow e1 e2) = tv e1 ++ tv e2
tv (TyVar v) = [v]
tv Value = []

tvQ :: QType -> [Name]
tvQ (Ty t) = tv t
tvQ (Forall n t) = tvQ t \\ [n]

tvGamma :: Context -> [Name]
tvGamma = foldMap tvQ

inferBody :: Context -> Body -> TC (Type, Subst)
inferBody g (Done e) = inferExp g e
inferBody g (Local b rest) = do 
    (g', s) <- inferBind g b
    (t, s') <- inferBody g' rest
    pure (t, s <> s')

inferBind :: Context -> Bind -> TC (Context, Subst)
inferBind g (Bind (Direct (VarP n _)) bod) = do
    (t, s) <- inferBody g bod
    let qt = generalise g t
    pure (M.insert n qt (substGamma s g), s)
inferBind g (Bind (FunP n _ lams) bod) = do
    (t, s) <- inferFun g lams bod
    let qt = generalise g t
    pure (M.insert n qt (substGamma s g), s)

inferFun :: Context -> [Pattern] -> Body -> TC (Type, Subst)
inferFun g [] bod = inferBody g bod
inferFun g (VarP n _:rest) bod = do
    alpha <- fresh
    (t, s) <- inferFun (M.insert n (Ty alpha) g) rest bod
    pure (Arrow (subst s alpha) t, s)

inferExp :: Context -> Expr Pattern -> TC (Type, Subst)
inferExp g (Projection e t) = do
    (t, s) <- inferExp g e
    s' <- unify (exprPos e) t Value
    pure (Value, s <> s')
inferExp g (Var pos t) = case M.lookup t g of
    Nothing -> typeError pos [StrE "Variable not in scope: ", VarNameE t]
    Just qt -> do 
        ret <- instantiate mempty qt 
        pure (ret, mempty)
inferExp g (App e1 e2) = do
    (t1, s1) <- inferExp g e1
    (t2, s2) <- inferExp (substGamma s1 g) e2
    alpha <- fresh
    s3 <- unify (exprPos e1) (subst s2 t1) (Arrow t2 alpha)
    pure (subst s3 alpha, s1 <> s2 <> s3)
inferExp g (Lam p (VarP n _) e) = do
    alpha <- fresh
    (t, s) <- inferExp (M.insert n (Ty alpha) g) e
    pure (Arrow (subst s alpha) t, s)
inferExp g (Literal {}) = pure (Value, mempty)
inferExp g (Freeze _ (VarP n _) e1 e2) = do
    (t1, s1) <- inferExp g e1
    (t2, s2) <- inferExp (M.insert n (Ty t1) (substGamma s1 g)) e2
    pure (t2, s1 <> s2)

inferTopLevels :: Context -> [TopLevel] -> Either (Position,[TypeErrorBit]) Context
inferTopLevels g [] = pure g
inferTopLevels g (Binding b:rest) = do 
    g' <- runTC (fst <$> inferBind g b)
    inferTopLevels g' rest 
inferTopLevels g (Imported t ts:rest) = do 
    g' <- inferTopLevels g ts
    inferTopLevels g' rest
inferTopLevels g (Properties pos g1 g2 Nothing:rest) = inferTopLevels g rest
inferTopLevels g (Properties pos g1 g2 (Just t):rest) = do
    _ <- runTC (inferExp g t)
    inferTopLevels g rest