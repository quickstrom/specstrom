{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Specstrom.Syntax where

import Data.Text (Text)
import Specstrom.Lexer (Position)

data Lit
  = IntLit Int
  | FloatLit Double
  | StringLit Text
  | CharLit Char
  | SelectorLit Text
  deriving (Show, Eq)

data Expr p
  = Projection (Expr p) Text
  | Var Position Text
  | App (Expr p) (Expr p)
  | Literal Position Lit
  | Freeze Position p (Expr p) (Expr p)
  | Lam Position p (Expr p)
  deriving (Show, Functor, Foldable, Traversable)

peelAps :: Expr p -> [Expr p] -> (Expr p, [Expr p])
peelAps (App x y) acc = peelAps x (y : acc)
peelAps v acc = (v, acc)

unpeelAps :: Expr p -> [Expr p] -> Expr p
unpeelAps e (x : xs) = unpeelAps (App e x) xs
unpeelAps e [] = e

exprPos :: Expr p -> Position
exprPos (Var p _) = p
exprPos (App e1 _e2) = exprPos e1
exprPos (Literal p _) = p
exprPos (Projection e _) = exprPos e
exprPos (Freeze p _ _ _) = p
exprPos (Lam p _ _) = p

type Name = Text

type Glob = [GlobTerm]

type GlobTerm = [Maybe Text]

data BindPattern
  = Direct Pattern
  | FunP Name Position [Pattern]
  deriving (Show)

data Pattern = VarP Name Position
  deriving (Show)

bindPatternVars :: BindPattern -> [Name]
bindPatternVars (FunP n p ps) = n : concatMap patternVars ps
bindPatternVars (Direct p) = patternVars p

bindPatternBoundVars :: BindPattern -> [Name]
bindPatternBoundVars (Direct p) = patternVars p
bindPatternBoundVars (FunP n p ps) = [n]

patternVars :: Pattern -> [Name]
patternVars (VarP n p) = [n]

data Bind = Bind BindPattern Body
  deriving (Show)

data Body = Local Bind Body | Done (Expr Pattern)
  deriving (Show)

data TopLevel
  = Binding Bind
  | Properties Position Glob Glob (Expr Pattern)
  | Imported Text [TopLevel]
  deriving (Show)
