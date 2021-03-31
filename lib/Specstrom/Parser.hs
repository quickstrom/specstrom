{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Specstrom.Parser where

import Control.Applicative
import Control.Monad.Except
import Data.Bifunctor (first, second)
import Data.List (intersperse, nub, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Specstrom.Lexer
import Specstrom.Syntax
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))
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

data ParseError
  = MalformedSyntaxDeclaration Position
  | SyntaxAlreadyDeclared Name Position
  | ExpectedPattern (Expr Pattern)
  | ExpectedPattern' (Expr TempExpr)
  | ExpectedSemicolon Position
  | ExpectedSemicolonOrWhen Position
  | ExpectedEquals Position
  | ExpectedWith Position
  | ExpectedModuleName Position
  | ExpectedGot Position [Text] Token
  | ExpressionAmbiguous (NonEmpty (Expr TempExpr))
  | DuplicatePatternBinding Position [Text]
  | TrailingGarbage Position Token
  | LexerFailure LexerError
  | ModuleNotFound Position Text
  deriving (Show)

type Table = ([[(Holey Text, Associativity)]], [Name])

builtIns :: ([[(Holey Text, Associativity)]], [Name])
builtIns =
  (\x -> (x, [])) $
    (map . map)
      (first holey)
      [ [ ("if_then_else_", RightAssoc)
        ],
        [ ("_==>_", RightAssoc)
        ],
        [("_||_", RightAssoc)],
        [("_&&_", RightAssoc)],
        [("_until_", RightAssoc)],
        [ ("not_", RightAssoc),
          ("always_", RightAssoc),
          ("nextT_", RightAssoc),
          ("nextF_", RightAssoc),
          ("next_", RightAssoc),
          ("eventually_", RightAssoc)
        ],
        [ ("_==_", NonAssoc),
          ("_!=_", NonAssoc),
          ("_<=_", NonAssoc),
          ("_>=_", NonAssoc),
          ("_<_", NonAssoc),
          ("_>_", NonAssoc)
        ],
        [ ("_-_", LeftAssoc),
          ("_+_", LeftAssoc)
        ],
        [ ("_*_", LeftAssoc),
          ("_/_", LeftAssoc),
          ("_%_", LeftAssoc)
        ]
      ]

parseGlob :: [(Position, Token)] -> Either ParseError ([(Position, Token)], Glob)
parseGlob ((_p, Ident n) : rest) = do
  fmap (asGlobTerm n :) <$> parseGlob rest
  where
    asGlobTerm _n = filter (/= Just "") $ intersperse Nothing (map Just (splitOn "*" n))
parseGlob rest = Right (rest, [])

wrap :: Either ParseError a -> ExceptT ParseError IO a
wrap = ExceptT . pure

loadModule :: [FilePath] -> Position -> Text -> Table -> ExceptT ParseError IO (Table, [TopLevel])
loadModule search p n t = do
  let candidates = map (<.> "strom") $ map (</> Text.unpack n) $ search
  available <- filterM (lift . doesFileExist) candidates
  case available of
    [] -> throwError $ ModuleNotFound p n
    (x : _) -> do
      contents <- lift $ Text.readFile x
      case lexer (x, 1, 1) contents of
        Left e -> throwError $ LexerFailure e
        Right toks -> do
          (t', inc) <- parseTopLevel search t toks
          pure (t', inc)

loadImmediate :: [FilePath] -> Table -> Text -> ExceptT ParseError IO (Table, [TopLevel])
loadImmediate search t txt = case lexer ("<immediate>", 1, 1) txt of
  Left e -> throwError $ LexerFailure e
  Right toks -> parseTopLevel search t toks

immediateExpr :: Table -> Text -> Either ParseError (Expr Pattern)
immediateExpr tbl txt = case lexer ("<immediate>", 1, 1) txt of
  Left e -> throwError $ LexerFailure e
  Right toks -> snd <$> parseExpressionTo EOF tbl toks

parseTopLevel :: [FilePath] -> Table -> [(Position, Token)] -> ExceptT ParseError IO (Table, [TopLevel])
parseTopLevel search t ((p, Reserved Import) : ts) = case ts of
  ((_, Ident n) : ts') -> case ts' of
    ((_, Semi) : ts'') -> do
      (t', inc) <- loadModule search p n t
      fmap (Imported n inc :) <$> parseTopLevel search t' ts''
    ((p', _) : _) -> throwError $ ExpectedSemicolon p'
  ((p', _) : _) -> throwError $ ExpectedModuleName p'
parseTopLevel search t ((p, Reserved Syntax) : ts) = do
  (ts', t') <- wrap (parseSyntax t p ts)
  parseTopLevel search t' ts'
parseTopLevel search t ((p, Reserved Let) : ts) = do
  (rest, b) <- wrap (parseBind t p ts)
  fmap (Binding b :) <$> parseTopLevel search t rest
parseTopLevel search t ((p, Reserved Action) : ts) = do
  (rest, b@(Bind pat _)) <- wrap (parseBind t p ts)
  let t' = second (bindPatternBoundVars pat ++) t
  fmap (ActionDecl b :) <$> parseTopLevel search t' rest
parseTopLevel search t ((p, Reserved Check) : ts) = do
  (rest, g1) <- wrap (parseGlob ts)
  case rest of
    ((_, Reserved With) : ts') -> do
      (rest', g2) <- wrap (parseGlob ts')
      case rest' of
        ((_, Reserved When) : ts'') -> do
          (rest'', g3) <- wrap (parseExpressionTo Semi t ts'')
          case rest'' of
            ((_, Semi) : rest''') -> fmap (Properties p g1 g2 (Just g3) :) <$> parseTopLevel search t rest'''
            ((p', _) : _) -> throwError $ ExpectedSemicolon p
        ((_, Semi) : rest'') -> fmap (Properties p g1 g2 Nothing :) <$> parseTopLevel search t rest'' --TODO add default
        ((p', _) : _) -> throwError $ ExpectedSemicolonOrWhen p'
    ((p', _) : _) -> throwError $ ExpectedWith p'
parseTopLevel search t [] = pure (t, [])
parseTopLevel search t [(p, EOF)] = pure (t, [])
parseTopLevel search t ((p, tok) : ts) = throwError $ TrailingGarbage p tok

parseBind :: Table -> Position -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Bind)
parseBind t p ts = do
  (ts', pat) <- parseBindPattern t ts
  case ts' of
    ((_, Reserved Define) : ts'') -> do
      (rest, body) <- parseBindingBody t ts''
      case rest of
        ((_, Semi) : rest') -> Right (rest', Bind pat body)
        ((p', _) : _) -> Left $ ExpectedSemicolon p'
    ((p', _) : _) -> Left $ ExpectedEquals p'

parseSyntax :: Table -> Position -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Table)
parseSyntax t p ts = case ts of
  ((_, Ident n) : (_, IntLitTok i) : (_, Semi) : ts') ->
    insertSyntax p n i NonAssoc t >>= \t' -> Right (ts', t')
  ((_, Ident n) : (_, IntLitTok i) : (_, Ident "left") : (_, Semi) : ts') ->
    insertSyntax p n i LeftAssoc t >>= \t' -> Right (ts', t')
  ((_, Ident n) : (_, IntLitTok i) : (_, Ident "right") : (_, Semi) : ts') ->
    insertSyntax p n i RightAssoc t >>= \t' -> Right (ts', t')
  _ -> Left $ MalformedSyntaxDeclaration p

parseBindingBody :: Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Body)
parseBindingBody t ((p, Reserved Syntax) : ts) = do
  (ts', t') <- parseSyntax t p ts
  parseBindingBody t' ts'
parseBindingBody t ((p, Reserved Let) : ts) = do
  (rest, b) <- parseBind t p ts
  fmap (Local b) <$> parseBindingBody t rest
parseBindingBody t ts = fmap Done <$> parseExpressionTo Semi t ts

insertSyntax :: Position -> Name -> Int -> Associativity -> Table -> Either ParseError Table
insertSyntax p n i a t
  | any ((holey n `elem`) . map fst) (fst t) = Left $ SyntaxAlreadyDeclared n p
  | otherwise = Right $ first (go i) t
  where
    go 0 (r : rs) = ((holey n, a) : r) : rs
    go n' (r : rs) = r : (go (n' -1) rs)
    go n' [] = go n' [[]]

parseBindPattern :: Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], BindPattern)
parseBindPattern t ts = do
  (ts', e) <- parseExpressionTo (Reserved Define) t ts
  case peelAps e [] of
    (Var p n, es) | not (null es),
                    n `notElem` (snd t) -> do
      es' <- mapM (patFromExpr (snd t)) es
      let ns = concatMap patternVars es'
          uniques = nub ns
          dupes = ns \\ uniques
      if uniques /= ns
        then Left (DuplicatePatternBinding p dupes)
        else pure (ts', FunP n p es')
    _ -> do
      p <- patFromExpr (snd t) e
      pure (ts', Direct p)

patFromAnyExpr :: [Name] -> Expr e -> Maybe Pattern
patFromAnyExpr t (Var p n)
  | n `elem` t = pure (ActionP n p [])
  | otherwise = pure (VarP n p)
patFromAnyExpr t (ListLiteral p es) = ListP p <$> mapM (patFromAnyExpr t) es
patFromAnyExpr t x@(App {}) = case peelAps x [] of
  (Var p n, args) | n `elem` t -> ActionP n p <$> mapM (patFromAnyExpr t) args
  _ -> Nothing
patFromAnyExpr t _ = Nothing

patFromExpr :: [Name] -> Expr Pattern -> Either ParseError Pattern
patFromExpr t e = case patFromAnyExpr t e of
  Just e' -> pure e'
  Nothing -> Left $ ExpectedPattern e

patFromExpr' :: [Name] -> Expr TempExpr -> Either ParseError Pattern
patFromExpr' t e = case patFromAnyExpr t e of
  Just e' -> pure e'
  Nothing -> Left $ ExpectedPattern' e

newtype TempExpr = E (Expr TempExpr) deriving (Show)

parseExpressionTo :: Token -> Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Expr Pattern)
parseExpressionTo terminator t ts =
  let (candidate, ts') = break ((\x -> x == terminator || x == EOF) . snd) ts
   in case fullParses (parser $ grammar t) candidate of
        ([one], _) -> (ts',) <$> traverse (\(E e) -> patFromExpr' (snd t) e) one
        ([], r) -> case unconsumed r of
          ((p, t') : _) -> Left (ExpectedGot p (expected r) t')
          [] -> error ("Expected: " <> show (expected r))
        (e : es, _r) -> Left (ExpressionAmbiguous (e :| es))

grammar :: Table -> Grammar r (Prod r Text (Position, Token) (Expr TempExpr))
grammar table = mdo
  literal <-
    rule $
      terminal (\t -> case t of (p, StringLitTok s) -> Just $ Literal p (StringLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, IntLitTok s) -> Just $ Literal p (IntLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, FloatLitTok s) -> Just $ Literal p (FloatLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, CharLitTok s) -> Just $ Literal p (CharLit s); _ -> Nothing)
        <|> terminal (\t -> case t of (p, SelectorLitTok s) -> Just $ Literal p (SelectorLit (Selector s)); _ -> Nothing)
  ident <-
    rule $
      (variable <?> "identifier")
        <|> (literal <?> "literal")
  atom <-
    rule $
      ident
        <|> lparen *> expr <* rparen
        <|> ListLiteral . fst <$> lbrack <*> argList <* rbrack
  normalProj <- rule $ atom <|> Projection <$> normalProj <*> projection
  argList <-
    rule $
      (:) <$> expr <*> (isToken Comma "," *> argList <|> pure [])
        <|> pure []
  normalApp <-
    rule $
      normalProj
        <|> app <$> normalApp <* lparen <*> argList <* rparen
  expr <- mixfixExpression tbl normalApp makeAp
  return expr
  where
    app x [] = x
    app f (x : xs) = app (App f x) xs
    tbl =
      [ [ ([Just (isToken (Reserved Fun) "fun"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
          ([Just (identToken "for"), Nothing, Just (identToken "in"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
          ([Just (identToken "forall"), Nothing, Just (identToken "in"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
          ([Just (identToken "exists"), Nothing, Just (identToken "in"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc)
        ],
        [ ([Nothing, Just (isToken (Reserved When) "when"), Nothing], LeftAssoc),
          ([Nothing, Just (identToken "timeout"), Nothing], LeftAssoc)
        ],
        [([Just (identToken "freeze"), Nothing, Just (isToken (Reserved Define) "="), Nothing, Just (isToken Dot "."), Nothing], RightAssoc)]
      ]
        ++ map (map $ first $ map $ fmap identToken) (fst table)

    mixfixParts =
      [ s | xs <- fst table, (ys, _) <- xs, Just s <- ys
      ]
    lbrack = satisfy ((== LBrack) . snd) <?> "left bracket"
    rbrack = satisfy ((== RBrack) . snd) <?> "right bracket"
    lparen = satisfy ((== LParen) . snd) <?> "left parenthesis"
    rparen = satisfy ((== RParen) . snd) <?> "right parenthesis"
    identToken s = isToken (Ident s) s
    isToken s t = satisfy ((== s) . snd) <?> t
    projection = terminal $ \(p, t) ->
      case t of
        ProjectionTok s -> pure s
        _ -> Nothing
    variable = terminal $ \(p, t) ->
      case t of
        Ident s -> guard (s `notElem` mixfixParts) >> pure (Var p s)
        _ -> Nothing
    makeAp hol as = case (unholey hol, as) of
      (Var p "freeze_=_._", [a1, a2, a3]) -> Freeze p (E a1) a2 a3
      (Var p "for_in_._", [a1, a2, a3]) -> App (App (Var p "map") (Lam p (E a1) a3)) a2
      (Var p "forall_in_._", [a1, a2, a3]) -> App (App (Var p "all") (Lam p (E a1) a3)) a2
      (Var p "exists_in_._", [a1, a2, a3]) -> App (App (Var p "any") (Lam p (E a1) a3)) a2
      (Var p "fun_._", [a1, a2]) -> Lam p (E a1) a2
      _ -> unpeelAps (unholey hol) as
    unholey ls = Var (getPosition ls) (foldMap (fromMaybe "_") (map (fmap (unident . snd)) ls))
      where
        unident (Ident s) = s
        unident (Reserved Fun) = "fun"
        unident Dot = "."
        unident (Reserved When) = "when"
        unident _ = "="
    getPosition ls = case filter isJust ls of
      [] -> error "No concrete token: the impossible happened"
      (Just (p, _) : _xs) -> p
      (_ : xs) -> getPosition xs
