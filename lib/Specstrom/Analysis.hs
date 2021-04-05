{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Specstrom.Analysis where

import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as M
import Specstrom.Dependency
import Specstrom.Syntax

type AnalysisEnv = M.HashMap Name Annotation

data Projection = Field Name | ListElement | ActionElement Name Int | ConstructorElement Name Int

data Annotation
  = FromSelector Selector [Projection]
  | Function (Annotation -> Annotation)
  | List Annotation
  | Constant
  | Action Name [Annotation]
  | Constructor Name [Annotation]
  | Object [(Name, Annotation)]
  | Branch Annotation Annotation
  | Indirect Annotation Annotation
  | PossibleError

-- data Annotation = Value Dep Dep | Function (Annotation -> Annotation) Dep

class ToAnnotation a where
  toAnnotation :: a -> Annotation

instance ToAnnotation Annotation where
  toAnnotation = id

instance (ToAnnotation b) => ToAnnotation (Annotation -> b) where
  toAnnotation f = Function (toAnnotation . f)

builtIns :: AnalysisEnv
builtIns =
  M.fromList $
    zip values (repeat Constant)
      ++ zip binOps (repeat $ toAnnotation (\a b -> Indirect (Indirect Constant a) b))
      ++ zip unOps (repeat $ toAnnotation (Indirect Constant))
      ++ [ ("if_{_}else{_}", toAnnotation (\a t e -> Indirect (Branch t e) a)),
           ("_when_", toAnnotation Indirect)
         ]
      ++ zip hofs (repeat $ hofAnn)
      ++ [ ("foldl", toAnnotation (\f v x -> applyAnnotation (applyAnnotation f v) (projectAnnotation ListElement x))),
           ("foldr", toAnnotation (\f v x -> applyAnnotation (applyAnnotation f (projectAnnotation ListElement x)) v))
         ]
  where
    binOps = ["_==_", "_&&_", "_||_", "_until_", "_!=_", "_==>_", "_+_", "_-_", "_/_", "_*_", "_%_", "_>_", "_<_", "_>=_", "_<=_"]
    unOps = ["not_", "always{_}_", "next_", "nextT_", "nextF_"]
    values = ["true", "false", "null", "happened"]
    hofs = ["map"]
    hofAnn = toAnnotation (\f v -> List (applyAnnotation f (projectAnnotation ListElement v)))

depOf :: Annotation -> Dep
depOf (Function _) = mempty
depOf (Indirect a b) = depOf a <> depOf b
depOf (Branch a b) = depOf a <> depOf b
depOf (List a) = depOf a
depOf (Constant) = mempty
depOf (PossibleError) = mempty
depOf (Action _ as) = foldMap depOf as
depOf (Constructor _ as) = foldMap depOf as
depOf (Object as) = foldMap (depOf . snd) as
depOf (FromSelector sel proj) = go (dep sel) proj
  where
    go d [] = d
    go d (Field f : ps) = go (project f d) ps
    go d (_ : ps) = go d ps -- for now we only support fields.

analyseBody :: AnalysisEnv -> Body -> Annotation
analyseBody g (Done e) = analyseExpr g e
analyseBody g (Local b r) = analyseBody (analyseBind g b) r

analyseBodyWithParams :: AnalysisEnv -> [Pattern] -> Body -> Annotation
analyseBodyWithParams g [] b = analyseBody g b
analyseBodyWithParams g (p : ps) b = Function f
  where
    f a = withAnalysePatternLocal a p g $ \g' -> analyseBodyWithParams g' ps b

analyseBind :: AnalysisEnv -> Bind -> AnalysisEnv
analyseBind g (Bind (Direct pat) body) = analysePatternLet (analyseBody g body) pat g
analyseBind g (Bind (FunP n _ pats) body) =
  let a = analyseBodyWithParams g pats body
   in M.insert n a g

analysePattern :: Annotation -> Pattern -> [(Maybe Name, Annotation)]
analysePattern ann (VarP n _) = [(Just n, ann)]
analysePattern ann (IgnoreP _) = [(Nothing, ann)]
analysePattern ann (LitP _ _) = [(Nothing, ann)]
analysePattern ann (BoolP _ _) = [(Nothing, ann)]
analysePattern ann (NullP _) = [(Nothing, ann)]
analysePattern ann (ListP _ ps) = concatMap (analysePattern (projectAnnotation ListElement ann)) ps
analysePattern ann (ObjectP _ ps) = concatMap (\(n, p) -> analysePattern (projectAnnotation (Field n) ann) p) ps
analysePattern ann (ActionP n _ ps) = concatMap (\(i, p) -> analysePattern (projectAnnotation (ActionElement n i) ann) p) (zip [0 ..] ps)
analysePattern ann (SymbolP n _ ps) = concatMap (\(i, p) -> analysePattern (projectAnnotation (ConstructorElement n i) ann) p) (zip [0 ..] ps)

analysePatternLet :: Annotation -> Pattern -> AnalysisEnv -> AnalysisEnv
analysePatternLet a pat g =
  let x = analysePattern a pat
   in M.union (M.fromList [(n, foldr (flip Indirect) ann (map snd x)) | (Just n, ann) <- x]) g

withAnalysePatternLocal :: Annotation -> Pattern -> AnalysisEnv -> (AnalysisEnv -> Annotation) -> Annotation
withAnalysePatternLocal a pat g f =
  let x = analysePattern a pat
      g' = M.union (M.fromList [(n, ann) | (Just n, ann) <- x]) g
   in foldr (flip Indirect) (f g') (map snd x)

projectAnnotation :: Projection -> Annotation -> Annotation
projectAnnotation p (Branch b1 b2) = Branch (projectAnnotation p b1) (projectAnnotation p b2)
projectAnnotation p (Indirect d ind) = Indirect (projectAnnotation p d) ind
projectAnnotation p PossibleError = PossibleError
projectAnnotation p Constant = PossibleError
projectAnnotation p (FromSelector n ps) = FromSelector n (ps ++ [p])
projectAnnotation (Field f) (Object m) | Just v <- lookup f m = v
projectAnnotation (Field f) (List o) = projectAnnotation (Field f) o
projectAnnotation (ActionElement n i) (Action m ds)
  | n == m, i < length ds = ds !! i
  | otherwise = PossibleError
projectAnnotation (ConstructorElement n i) (Constructor m ds)
  | n == m, i < length ds = ds !! i
  | otherwise = PossibleError
projectAnnotation ListElement (List p) = p
projectAnnotation _ _ = PossibleError

branch :: Annotation -> Annotation -> Annotation
branch = Branch

applyAnnotation :: Annotation -> Annotation -> Annotation
applyAnnotation a b = case a of
  Function f -> f b
  Action n ls -> Action n (ls ++ [b])
  Constructor n ls -> Constructor n (ls ++ [b])
  Branch x y -> Branch (applyAnnotation x b) (applyAnnotation y b)
  Indirect x y -> Indirect (applyAnnotation x b) y
  _ -> PossibleError

analyseExpr :: AnalysisEnv -> Expr Pattern -> Annotation
analyseExpr g (Projection e t) = projectAnnotation (Field t) (analyseExpr g e)
analyseExpr g (Index a b) = Indirect (projectAnnotation ListElement (analyseExpr g a)) (analyseExpr g b)
analyseExpr g (Symbol _ t) = Constructor t []
analyseExpr g (App a b) = applyAnnotation (analyseExpr g a) (analyseExpr g b)
analyseExpr g (ListLiteral _ ls) = List (foldr branch Constant (map (analyseExpr g) ls))
analyseExpr g (ObjectLiteral _ ls) = Object $ map (fmap (analyseExpr g)) ls
analyseExpr _ (Literal _ (SelectorLit l)) = FromSelector l []
analyseExpr _ (Literal _ _) = Constant
analyseExpr g (Var _ t) | Just d <- M.lookup t g = d
analyseExpr g (Freeze _ pat e2 e3) = withAnalysePatternLocal (analyseExpr g e2) pat g $ \g' -> analyseExpr g' e3
analyseExpr g (Lam _ _ pat e) = Function f
  where
    f a = withAnalysePatternLocal a pat g $ \g' -> analyseExpr g' e
analyseExpr _ expr = error ("Impossible, can't analyse: " <> show expr)

analyseTopLevels :: [TopLevel] -> AnalysisEnv
analyseTopLevels = foldl' toAnalysisEnv builtIns
  where
    toAnalysisEnv :: AnalysisEnv -> TopLevel -> AnalysisEnv
    toAnalysisEnv env = \case
      Binding b -> analyseBind env b
      ActionDecl b -> analyseBind env b
      Imported _ ts' -> foldl' toAnalysisEnv env ts'
      Properties {} -> env
