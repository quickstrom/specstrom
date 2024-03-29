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
      ++ zip ternOps (repeat $ toAnnotation (\a b c -> Indirect (Indirect (Indirect Constant a) b) c))
      ++ zip unOps (repeat $ toAnnotation (Indirect Constant))
      ++ [ ("if_{_}else{_}", toAnnotation (\a t e -> Indirect (Branch t e) a)),
           ("nth", toAnnotation (\a b -> Indirect (projectAnnotation ListElement a) b)),
           ("_when_", toAnnotation Indirect)
         ]
      ++ zip hofs (repeat $ hofAnn)
      ++ [ ("foldl", toAnnotation (\f v x -> applyAnnotation (applyAnnotation f v) (projectAnnotation ListElement x))),
           ("foldr", toAnnotation (\f v x -> applyAnnotation (applyAnnotation f (projectAnnotation ListElement x)) v))
         ]
  where
    binOps = ["_==_", "_&&_", "_||_", "_until_", "_!=_", "_==>_", "_+_", "_-_", "_/_", "_*_", "_%_", "_>_", "_<_", "_>=_", "_<=_", "split", "zip", "always{_}_", "eventually{_}_", "_timeout_"]
    unOps = ["not_", "parseInt", "parseFloat", "trim", "next_", "nextT_", "nextF_", "isNull", "zipAll"]
    ternOps = ["_until{_}_", "_release{_}_"]
    values = ["true", "false", "null", "happened"]
    hofs = ["map", "unfoldr"]
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

analyseBodyWithParams :: AnalysisEnv -> [TopPattern] -> Expr TopPattern -> Annotation
analyseBodyWithParams g [] b = analyseExpr g b
analyseBodyWithParams g (p : ps) b = Function f
  where
    f a = withAnalysePatternLocal a p g $ \g' -> analyseBodyWithParams g' ps b

analyseBind :: AnalysisEnv -> Bind -> AnalysisEnv
analyseBind g (Bind (MacroExpansionBP e _) body) = analyseBind g (Bind e body)
analyseBind g (Bind (Direct pat) body) = analysePatternLet (analyseExpr g body) pat g
analyseBind g (Bind (FunP n _ pats) body) =
  let a = analyseBodyWithParams g pats body
   in M.insert n a g

analyseTopPattern :: Annotation -> TopPattern -> [(Maybe Name, Annotation)]
analyseTopPattern ann (LazyP n _) = [(Just n, ann)]
analyseTopPattern ann (MatchP p) = analysePattern ann p
analyseTopPattern ann (MacroExpansionTP t _) = analyseTopPattern ann t

analysePattern :: Annotation -> Pattern -> [(Maybe Name, Annotation)]
analysePattern ann (VarP n _) = [(Just n, ann)]
analysePattern ann (IgnoreP _) = [(Nothing, ann)]
analysePattern ann (LitP _ _) = [(Nothing, ann)]
analysePattern ann (BoolP _ _) = [(Nothing, ann)]
analysePattern ann (NullP _) = [(Nothing, ann)]
analysePattern ann (MacroExpansionP t _) = analysePattern ann t
analysePattern ann (ListP _ ps) = concatMap (analysePattern (projectAnnotation ListElement ann)) ps
analysePattern ann (ObjectP _ ps) = concatMap (\(n, p) -> analysePattern (projectAnnotation (Field n) ann) p) ps
analysePattern ann (ActionP n _ ps) = concatMap (\(i, p) -> analysePattern (projectAnnotation (ActionElement n i) ann) p) (zip [0 ..] ps)
analysePattern ann (SymbolP n _ ps) = concatMap (\(i, p) -> analysePattern (projectAnnotation (ConstructorElement n i) ann) p) (zip [0 ..] ps)

analysePatternLet :: Annotation -> TopPattern -> AnalysisEnv -> AnalysisEnv
analysePatternLet a pat g =
  let x = analyseTopPattern a pat
   in M.union (M.fromList [(n, foldr (flip Indirect) ann (map snd x)) | (Just n, ann) <- x]) g

withAnalysePatternLocal :: Annotation -> TopPattern -> AnalysisEnv -> (AnalysisEnv -> Annotation) -> Annotation
withAnalysePatternLocal a pat g f =
  let x = analyseTopPattern a pat
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

analyseExpr :: AnalysisEnv -> Expr TopPattern -> Annotation
analyseExpr g (Projection e t) = projectAnnotation (Field t) (analyseExpr g e)
analyseExpr g (MacroExpansion a b) = analyseExpr g a
analyseExpr g (Symbol _ t) = Constructor t []
analyseExpr g (App a b) = applyAnnotation (analyseExpr g a) (analyseExpr g b)
analyseExpr g (ListLiteral _ ls) = List (foldr branch Constant (map (analyseExpr g) ls))
analyseExpr g (ObjectLiteral _ ls) = Object $ map (fmap (analyseExpr g)) ls
analyseExpr _ (Literal _ (SelectorLit l)) = FromSelector l []
analyseExpr _ (Literal _ _) = Constant
analyseExpr g (Var _ t) | Just d <- M.lookup t g = d
analyseExpr g (Lam _ pats e) = analyseBodyWithParams g pats e
analyseExpr _ expr = error ("Impossible, can't analyse: " <> show expr)

analyseTopLevels :: [TopLevel] -> AnalysisEnv
analyseTopLevels = foldl' toAnalysisEnv builtIns
  where
    toAnalysisEnv :: AnalysisEnv -> TopLevel -> AnalysisEnv
    toAnalysisEnv env = \case
      Binding _ b -> analyseBind env b
      ActionDecl _ b -> analyseBind env b
      Imported _ _ ts' -> foldl' toAnalysisEnv env ts'
      Properties {} -> env
      DocBlock {} -> env
      SyntaxDecl {} -> env
      MacroDecl {} -> env
