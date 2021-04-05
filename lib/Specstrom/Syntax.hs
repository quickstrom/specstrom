{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Specstrom.Syntax where

import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
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
  | Symbol Position Text
  | App (Expr p) (Expr p)
  | Index (Expr p) (Expr p)
  | Literal Position Lit
  | Freeze Position p (Expr p) (Expr p)
  | Lam Position Bool p (Expr p) -- Bool is if it is declared with "case" or not.
  | ListLiteral Position [Expr p]
  | ObjectLiteral Position [(Name, Expr p)]
  deriving (Eq, Show, Functor, Foldable, Traversable)

peelAps :: Expr p -> [Expr p] -> (Expr p, [Expr p])
peelAps (App x y) acc = peelAps x (y : acc)
peelAps v acc = (v, acc)

unpeelAps :: Expr p -> [Expr p] -> Expr p
unpeelAps e xs = foldl App e xs

exprPos :: Expr p -> Position
exprPos (Var p _) = p
exprPos (Symbol p _) = p
exprPos (App e1 _e2) = exprPos e1
exprPos (Index e1 _e2) = exprPos e1
exprPos (Literal p _) = p
exprPos (Projection e _) = exprPos e
exprPos (Freeze p _ _ _) = p
exprPos (Lam p _ _ _) = p
exprPos (ListLiteral p _) = p
exprPos (ObjectLiteral p _) = p

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
    Right g' -> filter (flip matches g') names

data BindPattern
  = Direct TopPattern
  | FunP Name Position [TopPattern]
  deriving (Eq, Show)

data TopPattern 
  = LazyP Name Position
  | MatchP Pattern
  deriving (Eq, Show)

data Pattern
  = VarP Name Position
  | ListP Position [Pattern]
  | ObjectP Position [(Name, Pattern)]
  | ActionP Name Position [Pattern]
  | SymbolP Name Position [Pattern]
  | IgnoreP Position
  | LitP Position Lit
  | BoolP Position Bool
  | NullP Position
  deriving (Eq, Show)

bindPatternVars :: BindPattern -> [Name]
bindPatternVars (FunP n p ps) = n : concatMap topPatternVars ps
bindPatternVars (Direct p) = topPatternVars p

topPatternVars :: TopPattern -> [Name]
topPatternVars (LazyP n p) = [n]
topPatternVars (MatchP p) = patternVars p

bindPatternBoundVars :: BindPattern -> [Name]
bindPatternBoundVars (Direct p) = topPatternVars p
bindPatternBoundVars (FunP n p ps) = [n]

patternPos :: Pattern -> Position
patternPos (VarP _ p) = p
patternPos (ListP p _) = p
patternPos (ObjectP p _) = p
patternPos (SymbolP _ p _) = p
patternPos (ActionP _ p _) = p
patternPos (LitP p _) = p
patternPos (BoolP p _) = p
patternPos (IgnoreP p) = p
patternPos (NullP p) = p

patternVars :: Pattern -> [Name]
patternVars (VarP n p) = [n]
patternVars (ListP p ps) = concatMap patternVars ps
patternVars (ObjectP p ps) = concatMap patternVars (map snd ps)
patternVars (ActionP n p ps) = concatMap patternVars ps
patternVars (SymbolP n p ps) = concatMap patternVars ps
patternVars (LitP p _) = []
patternVars (BoolP p _) = []
patternVars (IgnoreP p) = []
patternVars (NullP p) = []

data Bind = Bind BindPattern Body
  deriving (Eq, Show)

data Body = Local Bind Body | Done (Expr TopPattern)
  deriving (Eq, Show)

bodyPosition :: Body -> Position
bodyPosition (Local b bod) = bodyPosition bod
bodyPosition (Done e) = exprPos e

data TopLevel
  = Binding Bind
  | ActionDecl Bind
  | Properties Position Glob Glob (Maybe (Expr TopPattern))
  | Imported Text [TopLevel]
  deriving (Eq, Show)

class MapPosition a where
  mapPosition :: (Position -> Position) -> a -> a

instance MapPosition p => MapPosition (Expr p) where
  mapPosition f expr = case expr of
    Projection e name -> Projection (mapPosition f e) name
    Var pos name -> Var (f pos) name
    Symbol pos name -> Symbol (f pos) name
    App e1 e2 -> App (mapPosition f e1) (mapPosition f e2)
    Index e1 e2 -> Index (mapPosition f e1) (mapPosition f e2)
    Literal p lit -> Literal (f p) lit
    Freeze pos p e1 e2 -> Freeze pos (mapPosition f p) (mapPosition f e1) (mapPosition f e2)
    Lam pos b p body -> Lam (f pos) b (mapPosition f p) (mapPosition f body)
    ListLiteral p r -> ListLiteral (f p) (map (mapPosition f) r)
    ObjectLiteral p r -> ObjectLiteral (f p) (map (second (mapPosition f)) r)

instance MapPosition TopPattern where
  mapPosition f (LazyP n p) = LazyP n (f p)
  mapPosition f (MatchP p) = MatchP (mapPosition f p)

instance MapPosition Pattern where
  mapPosition f (VarP name pos) = VarP name (f pos)
  mapPosition f (IgnoreP pos) = IgnoreP (f pos)
  mapPosition f (NullP pos) = NullP (f pos)
  mapPosition f (BoolP pos b) = BoolP (f pos) b
  mapPosition f (LitP pos l) = LitP (f pos) l
  mapPosition f (ListP p ps) = ListP (f p) (map (mapPosition f) ps)
  mapPosition f (ObjectP p ps) = ObjectP (f p) (map (second (mapPosition f)) ps)
  mapPosition f (ActionP n p ps) = ActionP n (f p) (map (mapPosition f) ps)
  mapPosition f (SymbolP n p ps) = SymbolP n (f p) (map (mapPosition f) ps)

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
    Properties pos g1 g2 expr' -> Properties (f pos) g1 g2 (fmap (mapPosition f) expr')
    Imported name ts -> Imported name (map (mapPosition f) ts)
