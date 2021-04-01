{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Specstrom.Analysis where

import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as M
import Specstrom.Dependency
import Specstrom.Syntax

type AnalysisEnv = M.HashMap Name Annotation

data Annotation = Value Dep Dep | Function (Annotation -> Annotation) Dep

class ToAnnotation a where
  toAnnotation :: a -> Annotation

instance ToAnnotation Annotation where
  toAnnotation = id

instance (ToAnnotation b) => ToAnnotation (Annotation -> b) where
  toAnnotation f = Function (toAnnotation . f) mempty

builtIns :: AnalysisEnv
builtIns =
  M.fromList $
    zip values (repeat (Value mempty mempty))
      ++ zip binOps (repeat $ toAnnotation mergeIndirect)
      ++ zip unOps (repeat $ toAnnotation indirect)
      ++ [ ("if_then_else_", toAnnotation (\(Value a b) t e -> mergeDirect t e `unionDep` (a <> b))),
           ("_when_", toAnnotation (\(Value a b) (Value c d) -> Value a (b <> c <> d))),
           ("#act", toAnnotation (\(Value x1 y1) (Value x2 y2) (Value x3 y3) (Value x4 y4) -> Value (x1 <> x2 <> x3 <> x4) (y1 <> y2 <> y3 <> y4)))
         ]
      ++ zip hofs (repeat $ hofAnn)
      ++ [ ("foldl", toAnnotation (\(Function f id) v x -> case f v `unionDep` id of Function f' id' -> f' x `unionDep` id')),
           ("foldr", toAnnotation (\(Function f id) v x -> case f x `unionDep` id of Function f' id' -> f' v `unionDep` id'))
         ]
  where
    binOps = ["_==_", "_&&_", "_||_", "_until_", "_!=_", "_==>_", "_+_", "_-_", "_/_", "_*_", "_%_", "_>_", "_<_", "_>=_", "_<=_"]
    unOps = ["not_", "always_", "next_", "nextT_", "nextF_", "eventually_"]
    values = ["true", "false", "null"]
    hofs = ["map"]
    hofAnn = toAnnotation (\(Function f id) v -> f v `unionDep` id)

indirect :: Annotation -> Annotation
indirect (Value a b) = Value mempty (a <> b)
indirect x = x

mergeDirect :: Annotation -> Annotation -> Annotation
mergeDirect (Value a _) (Function f b) = error "Impossible"
mergeDirect (Function f a) (Value b _) = error "Impossible"
mergeDirect (Value a c) (Value b d) = Value (a <> b) (c <> d)
mergeDirect (Function f a) (Function g b) = Function (\x -> mergeDirect (f x) (g x)) (a <> b)

mergeIndirect :: Annotation -> Annotation -> Annotation
mergeIndirect (Value a _) (Function f b) = error "Impossible"
mergeIndirect (Function f a) (Value b _) = error "Impossible"
mergeIndirect (Value a c) (Value b d) = Value mempty (a <> c <> b <> d)
mergeIndirect (Function f a) (Function g b) = Function (\x -> mergeIndirect (f x) (g x)) (a <> b)

unionDep :: Annotation -> Dep -> Annotation
unionDep (Value x d) d' = Value x (d <> d')
unionDep (Function f d) d' = Function f (d <> d')

depOf :: Annotation -> Dep
depOf (Value d d') = d <> d'
depOf (Function _ d) = d

analyseBody :: AnalysisEnv -> Body -> Annotation
analyseBody g (Done e) = analyseExpr g e
analyseBody g (Local b r) = analyseBody (analyseBind g b) r

analyseBodyWithParams :: AnalysisEnv -> [Pattern] -> Body -> Annotation
analyseBodyWithParams g [] b = analyseBody g b
analyseBodyWithParams g (p : ps) b = Function f mempty
  where
    f a =
      let new = M.fromList (zip (patternVars p) (repeat a))
          g' = M.union g new
       in analyseBodyWithParams g' ps b

analyseBind :: AnalysisEnv -> Bind -> AnalysisEnv
analyseBind g (Bind (Direct pat) body) =
  let a = analyseBody g body
      new = M.fromList (zip (patternVars pat) (repeat a))
   in M.union g new
analyseBind g (Bind (FunP n _ pats) body) =
  let a = analyseBodyWithParams g pats body
      new = M.fromList [(n, a)]
   in M.union g new

analyseExpr :: AnalysisEnv -> Expr Pattern -> Annotation
analyseExpr g (Projection e t) | Value d ind <- analyseExpr g e = Value (project t d) ind
analyseExpr g (Var _ t) | Just d <- M.lookup t g = d
analyseExpr g (Index a b) = analyseExpr g a `unionDep` depOf (analyseExpr g b)
analyseExpr g e@(App {})
  | (Symbol pos n, as) <- peelAps e [] =
    foldr mergeDirect (Value mempty mempty) $ map (analyseExpr g) as
analyseExpr g (App a b) | Function f d <- analyseExpr g a = f (analyseExpr g b) `unionDep` d
analyseExpr g (ListLiteral _ ls) = foldr mergeDirect (Value mempty mempty) $ map (analyseExpr g) ls
analyseExpr g (ObjectLiteral _ ls) = foldr mergeDirect (Value mempty mempty) $ map (analyseExpr g) (map snd ls)
analyseExpr _ (Literal _ (SelectorLit l)) = Value (dep l) mempty
analyseExpr _ (Literal _ _) = Value mempty mempty
analyseExpr g (Freeze _ pat e2 e3) =
  let a = analyseExpr g e2
      new = M.fromList (zip (patternVars pat) (repeat a))
      g' = M.union g new
   in analyseExpr g' e3 `unionDep` depOf a
analyseExpr g (Lam _ _ pat e) = Function f mempty
  where
    f a =
      let new = M.fromList (zip (patternVars pat) (repeat a))
          g' = M.union g new
       in analyseExpr g' e
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
