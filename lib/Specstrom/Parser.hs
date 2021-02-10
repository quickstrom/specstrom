{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Specstrom.Parser where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad (guard)
import Data.List (nub, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Specstrom.Lexer
import Text.Earley
import Text.Earley.Mixfix

holey :: Text -> Holey Text
holey t =
  case Text.uncons t of
    Nothing -> []
    Just ('_', xs) -> Nothing : holey xs
    Just _ -> Just i : holey rest
      where
        (i, rest) = Text.span (/= '_') t

data Lit = IntLit Int | FloatLit Double | StringLit Text | CharLit Char | SelectorLit Text deriving (Show, Eq)

data Expr
  = Var Position Text
  | App Expr Expr
  | Literal Position Lit
  | Freeze Position Expr Expr Expr
  deriving (Show)

exprPos :: Expr -> Position
exprPos (Var p _) = p
exprPos (App e1 _e2) = exprPos e1
exprPos (Literal p _) = p
exprPos (Freeze p _ _ _) = p

type Name = Text

type Pattern = (Name, Position)

data Body = Bind Name Position [Pattern] Body Body | Done Expr
  deriving (Show)

data ParseError
  = MalformedSyntaxDeclaration Position
  | SyntaxAlreadyDeclared Name Position
  | ExpectedPattern Expr
  | ExpectedSemicolon Position
  | ExpectedEquals Position
  | ExpectedGot Position [Text] Token
  | ExpressionAmbiguous (NonEmpty Expr)
  | DuplicatePatternBinding Position [Text]
  deriving (Show)

type Table = [[(Holey Text, Associativity)]]

builtIns :: [[(Holey Text, Associativity)]]
builtIns =
  (map . map)
    (first holey)
    [
      [ 
        ("_==>_", RightAssoc)
      ],
      [("_||_", RightAssoc)],
      [("_&&_", RightAssoc)],
      [("_until_", RightAssoc)],
      [
        ("not_", RightAssoc),
        ("always_", RightAssoc),
        ("next_", RightAssoc),
        ("eventually_", RightAssoc)
      ],
      [
        ("_==_", NonAssoc),
        ("_!=_", NonAssoc)
      ]
    ]

parseBindingBody :: Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Body)
parseBindingBody t ((p, Reserved Syntax) : ts) = case ts of
  ((_, Ident n) : (_, IntLitTok i) : (_, Semi) : ts') ->
    insertSyntax p n i NonAssoc t >>= \t' -> parseBindingBody t' ts'
  ((_, Ident n) : (_, IntLitTok i) : (_, Ident "left") : (_, Semi) : ts') ->
    insertSyntax p n i LeftAssoc t >>= \t' -> parseBindingBody t' ts'
  ((_, Ident n) : (_, IntLitTok i) : (_, Ident "right") : (_, Semi) : ts') ->
    insertSyntax p n i RightAssoc t >>= \t' -> parseBindingBody t' ts'
  _ -> Left $ MalformedSyntaxDeclaration p
parseBindingBody t ((p, Reserved Let) : ts) = do
  (ts', n, ps) <- parsePatterns t ts
  case ts' of
    ((_, Reserved Define) : ts'') -> do
      (rest, body) <- parseBindingBody t ts''
      case rest of
        ((_, Semi) : rest') -> fmap (Bind n p ps body) <$> parseBindingBody t rest'
        ((p', _) : _) -> Left $ ExpectedSemicolon p'
    ((p', _) : _) -> Left $ ExpectedEquals p'
parseBindingBody t ts = fmap Done <$> parseExpressionTo Semi t ts

insertSyntax :: Position -> Name -> Int -> Associativity -> Table -> Either ParseError Table
insertSyntax p n i a t
  | any ((holey n `elem`) . map fst) t = Left $ SyntaxAlreadyDeclared n p
  | otherwise = Right $ go i t
  where
    go 0 (r : rs) = ((holey n, a) : r) : rs
    go n' (r : rs) = r : (go (n' -1) rs)
    go n' [] = go n' [[]]

parsePatterns :: Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Name, [Pattern])
parsePatterns t ts = do
  (ts', e) <- parseExpressionTo (Reserved Define) t ts
  case peelAps e [] of
    (Var p n, es) -> do
      es' <- mapM fromExpr es
      let es'' = nub $ map fst es'
          dupes = map fst es' \\ es''
      if nub (map fst es') /= map fst es'
        then Left (DuplicatePatternBinding p dupes)
        else pure (ts', n, es')
    (e', _es) -> Left $ ExpectedPattern e'
  where
    fromExpr (Var p n) = Right (n, p)
    fromExpr e = Left $ ExpectedPattern e

peelAps :: Expr -> [Expr] -> (Expr, [Expr])
peelAps (App x y) acc = peelAps x (y : acc)
peelAps v acc = (v, acc)

unpeelAps :: Expr -> [Expr] -> Expr
unpeelAps e (x : xs) = unpeelAps (App e x) xs
unpeelAps e [] = e

parseExpressionTo :: Token -> Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Expr)
parseExpressionTo terminator t ts =
  let (candidate, ts') = break ((\x -> x == terminator || x == EOF) . snd) ts
   in case fullParses (parser $ grammar t) candidate of
        ([one], _) -> Right (ts', one)
        ([], r) -> case unconsumed r of
          ((p, t') : _) -> Left (ExpectedGot p (expected r) t')
        (e : es, _r) -> Left (ExpressionAmbiguous (e :| es))

grammar :: Table -> Grammar r (Prod r Text (Position, Token) Expr)
grammar table = mdo
  literal <-
    rule $
      terminal (\t -> case t of (p, StringLitTok s) -> Just $ Literal p (StringLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, IntLitTok s) -> Just $ Literal p (IntLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, FloatLitTok s) -> Just $ Literal p (FloatLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, CharLitTok s) -> Just $ Literal p (CharLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, SelectorLitTok s) -> Just $ Literal p (SelectorLit s); _ -> Nothing)
  ident <-
    rule $
      (variable <?> "identifier")
        <|> (literal <?> "literal")
  atom <-
    rule $
      ident
        <|> lparen *> expr <* rparen
  normalApp <-
    rule $
      atom
        <|> App <$> normalApp <*> atom
  expr <- mixfixExpression tbl normalApp makeAp
  return expr
  where
    tbl =
      [[([Just (identToken "freeze"), Nothing, Just (isToken (Reserved Define) "="), Nothing, Just (identToken "in"), Nothing], RightAssoc)]]
        ++ map (map $ first $ map $ fmap identToken) table

    mixfixParts =
      [ s | xs <- table, (ys, _) <- xs, Just s <- ys
      ]
    lparen = satisfy ((== LParen) . snd) <?> "left parenthesis"
    rparen = satisfy ((== RParen) . snd) <?> "right parenthesis"
    identToken s = isToken (Ident s) s
    isToken s t = satisfy ((== s) . snd) <?> t

    variable = terminal $ \(p, t) ->
      case t of
        Ident s -> guard (s `notElem` mixfixParts) >> pure (Var p s)
        _ -> Nothing
    makeAp hol as = case (unholey hol, as) of
      (Var p "freeze_=_in_", [a1, a2, a3]) -> Freeze p a1 a2 a3
      _ -> unpeelAps (unholey hol) as
    unholey ls = Var (getPosition ls) (foldMap (fromMaybe "_") (map (fmap (unident . snd)) ls))
      where
        unident (Ident s) = s
        unident _ = "="
    getPosition ls = case filter isJust ls of
      [] -> error "No concrete token: the impossible happened"
      (Just (p, _) : _xs) -> p
      (_ : xs) -> getPosition xs
