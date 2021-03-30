{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Specstrom.Syntax where

import qualified Data.Aeson as JSON
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Specstrom.Lexer (Position)

newtype Selector = Selector Text
  deriving (Show, Eq, Ord, Generic, Hashable, JSON.FromJSON, JSON.ToJSON, JSON.FromJSONKey, JSON.ToJSONKey)

data Lit
  = IntLit Int
  | FloatLit Double
  | StringLit Text
  | CharLit Char
  | SelectorLit Selector
  deriving (Show, Eq)

data Expr p
  = Projection (Expr p) Text
  | Var Position Text
  | App (Expr p) (Expr p)
  | Literal Position Lit
  | Freeze Position p (Expr p) (Expr p)
  | Lam Position p (Expr p)
  | ListLiteral Position [Expr p]
  deriving (Eq, Show, Functor, Foldable, Traversable)

peelAps :: Expr p -> [Expr p] -> (Expr p, [Expr p])
peelAps (App x y) acc = peelAps x (y : acc)
peelAps v acc = (v, acc)

unpeelAps :: Expr p -> [Expr p] -> Expr p
unpeelAps e xs = foldl App e xs

exprPos :: Expr p -> Position
exprPos (Var p _) = p
exprPos (App e1 _e2) = exprPos e1
exprPos (Literal p _) = p
exprPos (Projection e _) = exprPos e
exprPos (Freeze p _ _ _) = p
exprPos (Lam p _ _) = p
exprPos (ListLiteral p _) = p

type Name = Text

type Glob = [GlobTerm]

type GlobTerm = [Maybe Text]

asName :: GlobTerm -> Either Name GlobTerm
asName [Just t] = Left t
asName t = Right t

matches :: Text -> GlobTerm -> Bool
matches t [] = T.null t
matches t (Nothing : rest) = matchStar t
  where
    matchStar t' = matches t' rest || matchStar (T.tail t')
matches t (Just u : rest) = case T.stripPrefix u t of
  Nothing -> False
  Just t' -> matches t' rest

expand :: [Name] -> Glob -> [Name]
expand names g =
  g >>= \x -> case asName x of
    Left n -> [n]
    Right g -> filter (flip matches g) names

data BindPattern
  = Direct Pattern
  | FunP Name Position [Pattern]
  deriving (Eq, Show)

data Pattern = VarP Name Position
             | ListP Position [Pattern]
             | ActionP Name Position [Pattern]
  deriving (Eq, Show)

bindPatternVars :: BindPattern -> [Name]
bindPatternVars (FunP n p ps) = n : concatMap patternVars ps
bindPatternVars (Direct p) = patternVars p

bindPatternBoundVars :: BindPattern -> [Name]
bindPatternBoundVars (Direct p) = patternVars p
bindPatternBoundVars (FunP n p ps) = [n]

patternPos :: Pattern -> Position
patternPos (VarP _ p) = p
patternPos (ListP p _) = p
patternPos (ActionP _ p _) = p

patternVars :: Pattern -> [Name]
patternVars (VarP n p) = [n]
patternVars (ListP p ps) = concatMap patternVars ps
patternVars (ActionP n p ps) = concatMap patternVars ps

data Bind = Bind BindPattern Body
  deriving (Eq, Show)

data Body = Local Bind Body | Done (Expr Pattern)
  deriving (Eq, Show)

bodyPosition :: Body -> Position
bodyPosition (Local b bod) = bodyPosition bod
bodyPosition (Done e) = exprPos e

data TopLevel
  = Binding Bind
  | ActionDecl Bind
  | Properties Position Glob Glob (Maybe (Expr Pattern))
  | Imported Text [TopLevel]
  deriving (Eq, Show)

class MapPosition a where
  mapPosition :: (Position -> Position) -> a -> a

instance MapPosition (Expr p) where
  mapPosition f expr = case expr of
    Projection e name -> Projection (mapPosition f e) name
    Var pos name -> Var (f pos) name
    App e1 e2 -> App (mapPosition f e1) (mapPosition f e2)
    Literal p lit -> Literal (f p) lit
    Freeze pos p e1 e2 -> Freeze pos p (mapPosition f e1) (mapPosition f e2)
    Lam pos p body -> Lam (f pos) p (mapPosition f body)
    ListLiteral p r -> ListLiteral (f p) (map (mapPosition f) r)

instance MapPosition Pattern where
  mapPosition f (VarP name pos) = VarP name (f pos)
  mapPosition f (ListP p ps) = ListP (f p) (map (mapPosition f) ps)
  mapPosition f (ActionP n p ps) = ActionP n (f p) (map (mapPosition f) ps)

instance MapPosition Body where
  mapPosition f (Local bind body) = Local (mapPosition f bind) (mapPosition f body)
  mapPosition f (Done expr) = Done (mapPosition f expr)

instance MapPosition BindPattern where
  mapPosition f (Direct pat) = Direct (mapPosition f pat)
  mapPosition f (FunP name pos patterns) = FunP name (f pos) (map (mapPosition f) patterns)

instance MapPosition Bind where
  mapPosition f (Bind pat body) = Bind (mapPosition f pat) (mapPosition f body)

instance MapPosition TopLevel where
  mapPosition f expr = case expr of
    Binding bind -> Binding (mapPosition f bind)
    ActionDecl bind -> ActionDecl (mapPosition f bind)
    Properties pos g1 g2 expr -> Properties (f pos) g1 g2 (fmap (mapPosition f) expr)
    Imported name ts -> Imported name (map (mapPosition f) ts)
