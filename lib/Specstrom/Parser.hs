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

type Table = ([[(Holey Text, Associativity)]], ([[(Holey Text, Associativity)]], [Name]))

builtIns :: ([[(Holey Text, Associativity)]], ([[(Holey Text, Associativity)]], [Name]))
builtIns =
  (\x -> (x, ([], []))) $
    (map . map)
      (first (concatMap holey . Text.words))
      [ [ ("if_{_} else {_}", RightAssoc)
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
    [] -> error "impossible?"
  ((p', _) : _) -> throwError $ ExpectedModuleName p'
  [] -> error "impossible?"
parseTopLevel search t ((p, Reserved Syntax) : ts) = do
  (ts', t') <- wrap (parseSyntax t p ts)
  parseTopLevel search t' ts'
parseTopLevel search t ((p, Reserved Let) : ts) = do
  (rest, b) <- wrap (parseBind t p ts)
  fmap (Binding b :) <$> parseTopLevel search t rest
parseTopLevel search t ((p, Reserved Action) : ts) = do
  (rest, b@(Bind pat _)) <- wrap (parseBind t p ts)
  let t' = second (second (bindPatternBoundVars pat ++)) t
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
            [] -> error "impossible?"
        ((_, Semi) : rest'') -> fmap (Properties p g1 g2 Nothing :) <$> parseTopLevel search t rest'' --TODO add default
        ((p', _) : _) -> throwError $ ExpectedSemicolonOrWhen p'
        [] -> error "impossible?"
    ((p', _) : _) -> throwError $ ExpectedWith p'
    [] -> error "impossible?"
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
        [] -> error "impossible?"
    ((p', _) : _) -> Left $ ExpectedEquals p'
    [] -> error "impossible?"

parseSyntax :: Table -> Position -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Table)
parseSyntax t p ts = do
  let isIdent x = case x of Ident {} -> True; _ -> False
      fromIdent x = case x of Ident i -> i; _ -> error "Impossible"
  (n, assoc, i, ts') <- case span (isIdent . snd) ts of
    (ids@(_ : _), (_, IntLitTok i) : (_, Semi) : ts') -> pure (map (fromIdent . snd) ids, NonAssoc, i, ts')
    (ids@(_ : _), (_, IntLitTok i) : (_, Ident "left") : (_, Semi) : ts') -> pure (map (fromIdent . snd) ids, LeftAssoc, i, ts')
    (ids@(_ : _), (_, IntLitTok i) : (_, Ident "right") : (_, Semi) : ts') -> pure (map (fromIdent . snd) ids, RightAssoc, i, ts')
    _ -> Left $ MalformedSyntaxDeclaration p
  if i < 0
    then insertSyntax p n (- i) assoc (reverse $ fst (snd t)) >>= \t' -> Right (ts', second (first (const $ reverse t')) t)
    else insertSyntax p n i assoc (fst t) >>= \t' -> Right (ts', first (const t') t)

parseBindingBody :: Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], Body)
parseBindingBody t ((p, Reserved Syntax) : ts) = do
  (ts', t') <- parseSyntax t p ts
  parseBindingBody t' ts'
parseBindingBody t ((p, Reserved Let) : ts) = do
  (rest, b) <- parseBind t p ts
  fmap (Local b) <$> parseBindingBody t rest
parseBindingBody t ts = fmap Done <$> parseExpressionTo Semi t ts

insertSyntax :: Position -> [Name] -> Int -> Associativity -> [[(Holey Text, Associativity)]] -> Either ParseError [[(Holey Text, Associativity)]]
insertSyntax p n i a t
  | any ((concatMap holey n `elem`) . map fst) t = Left $ SyntaxAlreadyDeclared (mconcat n) p
  | otherwise = Right $ go i t
  where
    go 0 (r : rs) = ((concatMap holey n, a) : r) : rs
    go n' (r : rs) = r : (go (n' -1) rs)
    go n' [] = go n' [[]]

parseBindPattern :: Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], BindPattern)
parseBindPattern t ts = do
  (ts', e) <- parseExpressionTo (Reserved Define) t ts
  case peelAps e [] of
    (Var p n, es) | not (null es),
                    n `notElem` (snd $ snd t) -> do
      es' <- mapM (patFromExpr (snd $ snd t)) es
      let ns = concatMap patternVars es'
          uniques = nub ns
          dupes = ns \\ uniques
      if uniques /= ns
        then Left (DuplicatePatternBinding p dupes)
        else pure (ts', FunP n p es')
    _ -> do
      p <- patFromExpr (snd $ snd t) e
      pure (ts', Direct p)

patFromAnyExpr :: [Name] -> Expr e -> Maybe Pattern
patFromAnyExpr t (Var p n)
  | n == "_" = pure (IgnoreP p)
  | n == "null" = pure (NullP p)
  | n == "true" = pure (BoolP p True)
  | n == "false" = pure (BoolP p False)
  | n `elem` t = pure (ActionP n p [])
  | otherwise = pure (VarP n p)
patFromAnyExpr t (ObjectLiteral p es) = ObjectP p <$> mapM (traverse (patFromAnyExpr t)) es
patFromAnyExpr t (Literal p l) = pure $ LitP p l
patFromAnyExpr t (ListLiteral p es) = ListP p <$> mapM (patFromAnyExpr t) es
patFromAnyExpr t x@(App {}) = case peelAps x [] of
  (Var p n, args) | n `elem` t -> ActionP n p <$> mapM (patFromAnyExpr t) args
  (Symbol p n, args) -> SymbolP n p <$> mapM (patFromAnyExpr t) args
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
        ([one], _) -> (ts',) <$> traverse (\(E e) -> patFromExpr' (snd $ snd t) e) one
        ([], r) -> case unconsumed r of
          ((p, t') : _) -> Left (ExpectedGot p (expected r) t')
          [] -> Left (ExpectedGot dummyPosition (expected r) EOF) -- not sure how this happens
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
        <|> (symbol <?> "symbol")
        <|> (literal <?> "literal")
  atom <-
    rule $
      ident
        <|> ((lparen *> expr <* rparen) <?> "parenthesised expression")
        <|> ((ListLiteral . fst <$> lbrack <*> argList <* rbrack) <?> "list literal")
        <|> ((ObjectLiteral . fst <$> lbrace <*> fieldList <* rbrace) <?> "object literal")
  fieldList <-
    rule $
      (:) <$> ((,) <$> rawName <* isToken Colon ":" <*> expr) <*> (isToken Comma "," *> fieldList <|> pure [])
        <|> pure []
  argList <-
    rule $
      (:) <$> expr <*> (isToken Comma "," *> argList <|> pure [])
        <|> pure []
  normalApp <-
    rule $
      atom
        <|> ((Projection <$> normalApp <*> projection) <?> "field projection")
        <|> ((app <$> normalApp <* lparen <*> argList <* rparen) <?> "function application")
        <|> ((Index <$> normalApp <* lbrack <*> expr <* rbrack) <?> "array indexing")
  expr' <- mixfixExpression tbl normalApp makeAp
  expr <- rule $ expr' <?> "expression"
  return expr
  where
    app x [] = x
    app f (x : xs) = app (App f x) xs
    tbl =
      map (map $ first $ map $ fmap identToken) (fst (snd table))
        ++ [ [ ([Just (isToken (Reserved Fun) "fun"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
               ([Just (isToken (Reserved Fun) "fun"), Nothing, Just lbrace, Nothing, Just rbrace], RightAssoc),
               ([Just (isToken (Reserved Case) "case"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
               ([Just (isToken (Reserved Case) "case"), Nothing, Just lbrace, Nothing, Just rbrace], RightAssoc),
               ([Just (identToken "for"), Nothing, Just (identToken "in"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
               ([Just (identToken "for"), Nothing, Just (identToken "in"), Nothing, Just lbrace, Nothing, Just rbrace], RightAssoc),
               ([Just (identToken "forall"), Nothing, Just (identToken "in"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
               ([Just (identToken "forall"), Nothing, Just (identToken "in"), Nothing, Just lbrace, Nothing, Just rbrace], RightAssoc),
               ([Just (identToken "exists"), Nothing, Just (identToken "in"), Nothing, Just (isToken Dot "."), Nothing], RightAssoc),
               ([Just (identToken "exists"), Nothing, Just (identToken "in"), Nothing, Just lbrace, Nothing, Just rbrace], RightAssoc)
             ],
             [ ([Nothing, Just (isToken (Reserved When) "when"), Nothing], LeftAssoc),
               ([Nothing, Just (identToken "timeout"), Nothing], LeftAssoc)
             ],
             [ ([Just (identToken "freeze"), Nothing, Just (isToken (Reserved Define) "="), Nothing, Just (isToken Dot "."), Nothing], RightAssoc)
             , ([Just (identToken "freeze"), Nothing, Just (isToken (Reserved Define) "="), Nothing, Just lbrace, Nothing, Just rbrace], RightAssoc)]
           ]
        ++ map (map $ first $ map $ fmap identToken) (fst table)

    mixfixParts =
      [ s | xs <- fst table, (ys, _) <- xs, Just s <- ys
      ]
    lbrack = satisfy ((== LBrack) . snd) <?> "left bracket"
    rbrack = satisfy ((== RBrack) . snd) <?> "right bracket"
    lbrace = identToken "{"
    rbrace = identToken "}"
    lparen = satisfy ((== LParen) . snd) <?> "left parenthesis"
    rparen = satisfy ((== RParen) . snd) <?> "right parenthesis"
    colon = satisfy ((== Colon) . snd) <?> "colon"
    identToken s = isToken (Ident s) s
    isToken s t = satisfy ((== s) . snd) <?> t
    projection = terminal $ \(p, t) ->
      case t of
        ProjectionTok s -> pure s
        _ -> Nothing
    rawName = terminal $ \(p, t) ->
      case t of
        Ident s -> pure s
        _ -> Nothing
    symbol = Symbol . fst <$> colon <*> rawName
    variable = terminal $ \(p, t) ->
      case t of
        Ident s -> guard (s `notElem` mixfixParts) >> pure (Var p s)
        _ -> Nothing
    makeAp hol as = case (unholey hol, as) of
      (Var p "freeze_=_._", [a1, a2, a3]) -> Freeze p (E a1) a2 a3
      (Var p "for_in_._", [a1, a2, a3]) -> App (App (Var p "map") (Lam p False (E a1) a3)) a2
      (Var p "forall_in_._", [a1, a2, a3]) -> App (App (Var p "all") (Lam p False (E a1) a3)) a2
      (Var p "exists_in_._", [a1, a2, a3]) -> App (App (Var p "any") (Lam p False (E a1) a3)) a2
      (Var p "fun_._", [a1, a2]) -> Lam p False (E a1) a2
      (Var p "case_._", [a1, a2]) -> Lam p True (E a1) a2
      (Var p "freeze_=_{_}", [a1, a2, a3]) -> Freeze p (E a1) a2 a3
      (Var p "for_in_{_}", [a1, a2, a3]) -> App (App (Var p "map") (Lam p False (E a1) a3)) a2
      (Var p "forall_in_{_}", [a1, a2, a3]) -> App (App (Var p "all") (Lam p False (E a1) a3)) a2
      (Var p "exists_in_{_}", [a1, a2, a3]) -> App (App (Var p "any") (Lam p False (E a1) a3)) a2
      (Var p "fun_{_}", [a1, a2]) -> Lam p False (E a1) a2
      (Var p "case_{_}", [a1, a2]) -> Lam p True (E a1) a2
      _ -> unpeelAps (unholey hol) as
    unholey ls = Var (getPosition ls) (foldMap (fromMaybe "_") (map (fmap (unident . snd)) ls))
      where
        unident (Ident s) = s
        unident (Reserved Fun) = "fun"
        unident (Reserved Case) = "case"
        unident Dot = "."
        unident (Reserved When) = "when"
        unident _ = "="
    getPosition ls = case filter isJust ls of
      [] -> error "No concrete token: the impossible happened"
      (Just (p, _) : _xs) -> p
      (_ : xs) -> getPosition xs
