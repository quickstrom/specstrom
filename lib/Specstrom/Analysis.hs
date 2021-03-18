{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Specstrom.Analysis where

import qualified Data.Map as M
import Specstrom.Dependency
import Specstrom.Syntax

type AnalysisEnv = M.Map Name Annotation

data Annotation = Value Dep | Function (Annotation -> Annotation) Dep

class ToAnnotation a where
  toAnnotation :: a -> Annotation

instance ToAnnotation Dep where
  toAnnotation = Value

instance ToAnnotation Annotation where
  toAnnotation = id

instance (ToAnnotation b) => ToAnnotation (Annotation -> b) where
  toAnnotation f = Function (toAnnotation . f) mempty

builtIns :: AnalysisEnv
builtIns =
  M.fromList $
    zip values (repeat (Value mempty))
      ++ [ ("click!", toAnnotation (\(Value b) -> Value (project "disabled" b)))]
      ++ zip binOps (repeat $ toAnnotation merge)
      ++ zip unOps (repeat $ toAnnotation (id :: Annotation -> Annotation))
      ++ [ ("if_then_else_", toAnnotation (\(Value b) t e -> (merge t e) `unionDep` b))
         ]
  where
    binOps = ["_==_", "_&&_", "_||_", "_when_", "_timeout_"] -- "_until_","_!=_","_==>_"]
    unOps = ["not_", "always_", "next_", "nextT_", "nextF_", "changed?"] -- "eventually_"]
    values = ["true", "false", "null", "loaded?", "noop!"]

merge :: Annotation -> Annotation -> Annotation
merge (Value a) (Function f b) = Function f (a <> b)
merge (Function f a) (Value b) = Function f (a <> b)
merge (Value a) (Value b) = Value (a <> b)
merge (Function f a) (Function g b) = Function (\x -> merge (f x) (g x)) (a <> b)

unionDep :: Annotation -> Dep -> Annotation
unionDep (Value d) d' = Value (d <> d')
unionDep (Function f d) d' = Function f (d <> d')

depOf :: Annotation -> Dep
depOf (Value d) = d
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
analyseExpr g (Projection e t) | Value d <- analyseExpr g e = Value (project t d)
analyseExpr g (Var _ t) | Just d <- M.lookup t g = d
analyseExpr g (App a b) | Function f d <- analyseExpr g a = f (analyseExpr g b) `unionDep` d
analyseExpr _ (Literal _ (SelectorLit l)) = Value (dep l)
analyseExpr _ (Literal _ _) = Value mempty
analyseExpr g (Freeze _ pat e2 e3) =
  let a = analyseExpr g e2
      new = M.fromList (zip (patternVars pat) (repeat a))
      g' = M.union g new
   in analyseExpr g' e3 `unionDep` depOf a
analyseExpr g (Lam _ pat e) = Function f mempty
  where
    f a =
      let new = M.fromList (zip (patternVars pat) (repeat a))
          g' = M.union g new
       in analyseExpr g' e
analyseExpr _ _ = error "impossible"
