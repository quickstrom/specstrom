{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Specstrom.Syntax where

import qualified Data.Aeson as JSON
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Generic.Functor
import Specstrom.Lexer (Position, dummyPosition)
import Text.Earley.Mixfix (Associativity)

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
  = MacroExpansion (Expr p) (Expr TempExpr)
  | Literal Position Lit
  | Var Position Text
  | App (Expr p) (Expr p)
  | Lam Position [p] (Expr p)
  | Symbol Position Text
  | ListLiteral Position [Expr p]
  | ObjectLiteral Position [(Name, Expr p)]
  | Projection (Expr p) Text
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

newtype TempExpr = E (Expr TempExpr) deriving (Eq, Show)

peelAps :: Expr p -> [Expr p] -> (Expr p, [Expr p])
peelAps (App x y) acc = peelAps x (y : acc)
peelAps v acc = (v, acc)

unpeelAps :: Expr p -> [Expr p] -> Expr p
unpeelAps e xs = foldl App e xs

exprPos :: Expr p -> Position
exprPos (Var p _) = p
exprPos (Symbol p _) = p
exprPos (App e1 _e2) = exprPos e1
exprPos (Literal p _) = p
exprPos (MacroExpansion _ e) = exprPos e
exprPos (Projection e _) = exprPos e
exprPos (Lam p _ _) = p
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
  | MacroExpansionBP BindPattern (Expr TempExpr)
  deriving (Eq, Show)

data TopPattern
  = LazyP Name Position
  | MatchP Pattern
  | MacroExpansionTP TopPattern (Expr TempExpr)
  deriving (Eq, Show)

data Pattern
  = VarP Name Position
  | ListP Position [Pattern]
  | MacroExpansionP Pattern (Expr TempExpr)
  | ObjectP Position [(Name, Pattern)]
  | ActionP Name Position [Pattern]
  | SymbolP Name Position [Pattern]
  | IgnoreP Position
  | LitP Position Lit
  | BoolP Position Bool
  | NullP Position
  deriving (Eq, Show)

bindPatternVars :: BindPattern -> [Name]
bindPatternVars (MacroExpansionBP p _) = bindPatternVars p
bindPatternVars (FunP n p ps) = n : concatMap topPatternVars ps
bindPatternVars (Direct p) = topPatternVars p

topPatternVars :: TopPattern -> [Name]
topPatternVars (LazyP n p) = [n]
topPatternVars (MatchP p) = patternVars p
topPatternVars (MacroExpansionTP p _) = topPatternVars p

bindPatternBoundVars :: BindPattern -> [Name]
bindPatternBoundVars (MacroExpansionBP p _) = bindPatternBoundVars p
bindPatternBoundVars (Direct p) = topPatternVars p
bindPatternBoundVars (FunP n p ps) = [n]

patternPos :: Pattern -> Position
patternPos (VarP _ p) = p
patternPos (ListP p _) = p
patternPos (ObjectP p _) = p
patternPos (SymbolP _ p _) = p
patternPos (MacroExpansionP _ e) = exprPos e
patternPos (ActionP _ p _) = p
patternPos (LitP p _) = p
patternPos (BoolP p _) = p
patternPos (IgnoreP p) = p
patternPos (NullP p) = p

patternVars :: Pattern -> [Name]
patternVars (VarP n p) = [n]
patternVars (ListP p ps) = concatMap patternVars ps
patternVars (MacroExpansionP p _) = patternVars p
patternVars (ObjectP p ps) = concatMap patternVars (map snd ps)
patternVars (ActionP n p ps) = concatMap patternVars ps
patternVars (SymbolP n p ps) = concatMap patternVars ps
patternVars (LitP p _) = []
patternVars (BoolP p _) = []
patternVars (IgnoreP p) = []
patternVars (NullP p) = []

type Bind = Bind' TopPattern BindPattern

data Bind' e bp = Bind bp (Expr e)
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving (Bifunctor, Bifoldable) via (GenericBifunctor Bind')

instance Bitraversable Bind' where
  bitraverse = gbitraverse

instance Bitraversable TopLevel' where
  bitraverse = gbitraverse

type Documentation = [Text]

type TopLevel = TopLevel' TopPattern BindPattern

data TopLevel' e bp
  = Binding Documentation (Bind' e bp)
  | ActionDecl Documentation (Bind' e bp)
  | SyntaxDecl Documentation Position [Text] Int Associativity
  | MacroDecl Documentation (Expr TempExpr) [Name] (Expr TempExpr)
  | DocBlock Documentation
  | Properties Position Glob Glob (Maybe (Expr e))
  | Imported Position Text [TopLevel' e bp]
  deriving (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving (Bifunctor, Bifoldable) via (GenericBifunctor TopLevel')

tlPos :: (bp -> Position) -> TopLevel' e bp -> Position
tlPos f (Binding _ (Bind b _)) = f b
tlPos f (ActionDecl _ (Bind b _)) = f b
tlPos f (MacroDecl _ e _ _) = exprPos e
tlPos f (SyntaxDecl _ p _ _ _) = p
tlPos f (DocBlock _) = dummyPosition
tlPos f (Properties p _ _ _) = p
tlPos f (Imported p _ _) = p

class MapPosition a where
  mapPosition :: (Position -> Position) -> a -> a

instance MapPosition TempExpr where
  mapPosition f (E e) = E (mapPosition f e)

instance MapPosition p => MapPosition (Expr p) where
  mapPosition f expr = case expr of
    Projection e name -> Projection (mapPosition f e) name
    Var pos name -> Var (f pos) name
    Symbol pos name -> Symbol (f pos) name
    App e1 e2 -> App (mapPosition f e1) (mapPosition f e2)
    MacroExpansion e1 e2 -> MacroExpansion (mapPosition f e1) (mapPosition f e2)
    Literal p lit -> Literal (f p) lit
    Lam pos p body -> Lam (f pos) (map (mapPosition f) p) (mapPosition f body)
    ListLiteral p r -> ListLiteral (f p) (map (mapPosition f) r)
    ObjectLiteral p r -> ObjectLiteral (f p) (map (second (mapPosition f)) r)

instance MapPosition TopPattern where
  mapPosition f (LazyP n p) = LazyP n (f p)
  mapPosition f (MacroExpansionTP n p) = MacroExpansionTP (mapPosition f n) (mapPosition f p)
  mapPosition f (MatchP p) = MatchP (mapPosition f p)

instance MapPosition Pattern where
  mapPosition f (VarP name pos) = VarP name (f pos)
  mapPosition f (IgnoreP pos) = IgnoreP (f pos)
  mapPosition f (MacroExpansionP t p) = MacroExpansionP (mapPosition f t) (mapPosition f p)
  mapPosition f (NullP pos) = NullP (f pos)
  mapPosition f (BoolP pos b) = BoolP (f pos) b
  mapPosition f (LitP pos l) = LitP (f pos) l
  mapPosition f (ListP p ps) = ListP (f p) (map (mapPosition f) ps)
  mapPosition f (ObjectP p ps) = ObjectP (f p) (map (second (mapPosition f)) ps)
  mapPosition f (ActionP n p ps) = ActionP n (f p) (map (mapPosition f) ps)
  mapPosition f (SymbolP n p ps) = SymbolP n (f p) (map (mapPosition f) ps)

instance MapPosition BindPattern where
  mapPosition f (Direct pat) = Direct (mapPosition f pat)
  mapPosition f (FunP name pos patterns) = FunP name (f pos) (map (mapPosition f) patterns)
  mapPosition f (MacroExpansionBP t p) = MacroExpansionBP (mapPosition f t) (mapPosition f p)

instance (MapPosition bp, MapPosition p) => MapPosition (Bind' p bp) where
  mapPosition f (Bind pat body) = Bind (mapPosition f pat) (mapPosition f body)

instance (MapPosition bp, MapPosition p) => MapPosition (TopLevel' p bp) where
  mapPosition f expr = case expr of
    Binding docs bind -> Binding docs (mapPosition f bind)
    ActionDecl docs bind -> ActionDecl docs (mapPosition f bind)
    DocBlock docs -> DocBlock docs
    SyntaxDecl docs pos tokens l assoc -> SyntaxDecl docs pos tokens l assoc
    MacroDecl docs e1 names e2 -> MacroDecl docs (mapPosition f e1) names (mapPosition f e2)
    Properties pos g1 g2 expr' -> Properties (f pos) g1 g2 (fmap (mapPosition f) expr')
    Imported pos name ts -> Imported pos name (map (mapPosition f) ts)
