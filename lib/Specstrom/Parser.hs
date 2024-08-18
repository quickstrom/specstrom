{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Specstrom.Parser (loadImmediate, ParseError (..), loadModule, Table, builtIns, parseTopLevel, GrammarResult) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Bitraversable
import qualified Data.HashMap.Strict as M
import Data.List (nub, (\\))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Data.Text (Text)
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
  = SyntaxAlreadyDeclared Name Position
  | ExpectedPattern (Expr TempExpr)
  | InvalidMacroLHS Position
  | ExpectedGot Position [Text] Token
  | Ambiguous (NonEmpty GrammarResult)
  | DuplicatePatternBinding Position [Text]
  | LexerFailure LexerError
  | ModuleNotFound Position Text
  deriving (Show)

data Table = Table
  { positiveHoles :: [[(Holey Text, Associativity)]],
    negativeHoles :: [[(Holey Text, Associativity)]],
    actionNames :: [Name],
    macros :: M.HashMap Name ([Name], Expr TempExpr)
  }

builtIns :: Table
builtIns =
  (\x -> Table x [] [] mempty) $
    (map . map)
      (first (concatMap holey . Text.words))
      [ [ ("always {_} _", NonAssoc),
          ("eventually {_} _", NonAssoc),
          ("_ until {_} _", RightAssoc),
          ("_ release {_} _", RightAssoc)
        ],
        [("_timeout_", NonAssoc)],
        [ ("_==>_", RightAssoc)
        ],
        [("_||_", RightAssoc)],
        [("_&&_", RightAssoc)],
        [("_until_", RightAssoc)],
        [ ("not_", RightAssoc),
          ("nextT_", RightAssoc),
          ("nextF_", RightAssoc),
          ("next_", RightAssoc)
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
        ],
        [("if_ {_} else {_}", RightAssoc)],
        [("~_", NonAssoc)]
      ]

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

loadImmediate :: Int -> [FilePath] -> Table -> Text -> ExceptT ParseError IO (Table, Either [TopLevel] (Expr TopPattern))
loadImmediate lno search t txt = case lexer ("<immediate>", lno, 1) txt of
  Left e -> throwError $ LexerFailure e
  Right toks -> parseTL True search t toks

insertSyntax :: Position -> [Name] -> Int -> Associativity -> [[(Holey Text, Associativity)]] -> Either ParseError [[(Holey Text, Associativity)]]
insertSyntax p n i a t
  | any ((concatMap holey n `elem`) . map fst) t = Left $ SyntaxAlreadyDeclared (mconcat n) p
  | otherwise = Right $ go i t
  where
    go 0 (r : rs) = ((concatMap holey n, a) : r) : rs
    go n' (r : rs) = r : (go (n' - 1) rs)
    go n' [] = go n' [[]]

bindPatFromExpr :: [Name] -> Expr TempExpr -> Either ParseError BindPattern
bindPatFromExpr acts (MacroExpansion e' e) = (flip MacroExpansionBP e) <$> bindPatFromExpr acts e'
bindPatFromExpr acts e = do
  case peelAps e [] of
    (Var p n, es)
      | not (null es),
        n /= "~_",
        n `notElem` acts -> do
        es' <- mapM (patFromExpr acts) es
        let ns = concatMap topPatternVars es'
            uniques = nub ns
            dupes = ns \\ uniques
        if uniques /= ns
          then Left (DuplicatePatternBinding p dupes)
          else pure (FunP n p es')
    _ -> do
      p <- patFromExpr acts e
      pure (Direct p)

patFromAnyExpr :: [Name] -> Expr e -> Maybe TopPattern
patFromAnyExpr ns (MacroExpansion p e) = MacroExpansionTP <$> patFromAnyExpr ns p <*> pure e
patFromAnyExpr ns (App (Var p "~_") (Var _ n)) | n `notElem` ns = Just $ LazyP n p
patFromAnyExpr ns e = MatchP <$> helper ns e
  where
    helper t (Var p n)
      | n == "_" = pure (IgnoreP p)
      | n == "null" = pure (NullP p)
      | n == "true" = pure (BoolP p True)
      | n == "false" = pure (BoolP p False)
      | n `elem` t = pure (ActionP n p [])
      | otherwise = pure (VarP n p)
    helper t (MacroExpansion p e') = MacroExpansionP <$> helper t p <*> pure e'
    helper t (ObjectLiteral p es) = ObjectP p <$> mapM (traverse (helper t)) es
    helper t (Literal p l) = pure $ LitP p l
    helper t (ListLiteral p es) = ListP p <$> mapM (helper t) es
    helper t x@(App {}) = case peelAps x [] of
      (Var p n, args) | n `elem` t -> ActionP n p <$> mapM (helper t) args
      (Symbol p n, args) -> SymbolP n p <$> mapM (helper t) args
      _ -> Nothing
    helper t _ = Nothing

patFromExpr :: [Name] -> Expr TempExpr -> Either ParseError TopPattern
patFromExpr t e = case patFromAnyExpr t e of
  Just e' -> pure e'
  Nothing -> Left $ ExpectedPattern e

macroExpand' :: M.HashMap Name (Expr TempExpr) -> M.HashMap Name ([Name], Expr TempExpr) -> TempExpr -> Either ParseError TempExpr
macroExpand' locs env (E t) = E <$> macroExpand locs env t

macroExpand :: M.HashMap Name (Expr TempExpr) -> M.HashMap Name ([Name], Expr TempExpr) -> Expr TempExpr -> Either ParseError (Expr TempExpr)
macroExpand locs env expr = case expr of
  Projection e name -> Projection <$> macroExpand locs env e <*> pure name
  MacroExpansion e1 e2 -> MacroExpansion <$> macroExpand locs env e1 <*> pure e2
  Lam pos p body -> Lam pos <$> traverse (macroExpand' locs env) p <*> macroExpand locs env body
  ListLiteral p r -> ListLiteral p <$> mapM (macroExpand locs env) r
  ObjectLiteral p r -> ObjectLiteral p <$> mapM (traverse (macroExpand locs env)) r
  Literal p lit -> pure $ Literal p lit
  Symbol pos name -> pure $ Symbol pos name
  e
    | (Var _ name, args) <- peelAps e [],
      Just (argsN, term) <- M.lookup name env,
      length args == length argsN ->
      do
        args' <- traverse (macroExpand locs env) args
        flip MacroExpansion expr <$> macroExpand (M.fromList (zip argsN args')) env term
    | (Var p name, []) <- peelAps e [], Just term <- M.lookup name locs -> pure term
  (Var p name) -> pure $ Var p name
  App e1 e2 -> App <$> macroExpand locs env e1 <*> macroExpand locs env e2

parseTopLevel :: [FilePath] -> Table -> [(Position, Token)] -> ExceptT ParseError IO (Table, [TopLevel])
parseTopLevel search t toks = fmap (\(Left e) -> e) <$> parseTL False search t toks

parseTL :: Bool -> [FilePath] -> Table -> [(Position, Token)] -> ExceptT ParseError IO (Table, Either [TopLevel] (Expr TopPattern))
parseTL repl search t ((_, EOF) : _) = pure (t, Left [])
parseTL repl search t ts = do
  (ts', etl) <- wrap (parseTL' repl t ts)
  let handle tl = case tl of
        SyntaxDecl ds p toks i assoc -> do
          t' <-
            wrap $
              if i < 0
                then insertSyntax p toks (-i) assoc (reverse $ negativeHoles t) >>= \t' -> pure $ t {negativeHoles = reverse t'}
                else insertSyntax p toks i assoc (positiveHoles t) >>= \t' -> pure $ t {positiveHoles = t'}
          fmap (SyntaxDecl ds p toks i assoc :) <$> parseTopLevel search t' ts'
        Binding ds bind@(Bind pat bod) -> do
          bind' <- wrap (Bind <$> macroEx pat <*> macroEx bod)
          bind''@(Bind pat' _) <- wrap $ bitraverse interpretP interpretBP bind'
          fmap (Binding ds bind'' :) <$> parseTopLevel search t ts'
        ActionDecl ds bind@(Bind pat bod) -> do
          bind' <- wrap (Bind <$> macroEx pat <*> macroEx bod)
          bind''@(Bind pat' _) <- wrap $ bitraverse interpretP interpretBP bind'
          let t' = t {actionNames = bindPatternBoundVars pat' ++ actionNames t}
          fmap (ActionDecl ds bind'' :) <$> parseTopLevel search t' ts'
        MacroDecl ds e1 as e2 -> do
          let fromVar x = case x of Var _ n -> Just n; _ -> Nothing
          case peelAps e1 [] of
            (Var _ macroName, args)
              | Just args' <- mapM fromVar args,
                args' == nub args' -> do
                let t' = t {macros = M.insert macroName (args', e2) (macros t)}
                fmap (MacroDecl ds e1 args' e2 :) <$> parseTopLevel search t' ts'
            _ -> throwError $ InvalidMacroLHS (exprPos e1)
        Properties p g1 g2 e -> do
          e' <- wrap (traverse (macroEx >=> traverse interpretP) e)
          fmap (Properties p g1 g2 e' :) <$> parseTopLevel search t ts'
        DocBlock doc ->
          if Text.take 1 (head doc) == "#"
            then fmap (DocBlock doc :) <$> parseTopLevel search t ts'
            else
              parseTopLevel search t ts' >>= \x -> pure $ case x of
                (t', DocBlock ds : rest) -> (t', DocBlock (doc ++ ds) : rest)
                (t', MacroDecl ds mac args bod : rest) -> (t', MacroDecl (doc ++ ds) mac args bod : rest)
                (t', SyntaxDecl ds p a b c : rest) -> (t', SyntaxDecl (doc ++ ds) p a b c : rest)
                (t', ActionDecl ds b : rest) -> (t', ActionDecl (doc ++ ds) b : rest)
                (t', Binding ds b : rest) -> (t', Binding (doc ++ ds) b : rest)
                (t', rest) -> (t', DocBlock doc : rest)
        Imported p n _ -> do
          (t', inc) <- loadModule search p n t
          fmap (Imported p n inc :) <$> parseTopLevel search t' ts'
  case etl of
    Left tl -> fmap Left <$> handle tl
    Right e -> (t,) . Right <$> wrap (macroEx e >>= traverse interpretP)
  where
    macroEx :: Expr TempExpr -> Either ParseError (Expr TempExpr)
    macroEx = macroExpand mempty (macros t)

    interpretP :: TempExpr -> Either ParseError TopPattern
    interpretP (E e) = patFromExpr (actionNames t) e

    interpretBP :: Expr TempExpr -> Either ParseError BindPattern
    interpretBP e = bindPatFromExpr (actionNames t) e

parseTL' :: Bool -> Table -> [(Position, Token)] -> Either ParseError ([(Position, Token)], GrammarResult)
parseTL' repl t ts =
  case allParses (parser $ fmap (if repl then fst else snd) $ grammar t) ts of
    ([], r) -> case unconsumed r of
      ((p, t') : _) -> Left (ExpectedGot p (expected r) t')
      [] -> Left (ExpectedGot dummyPosition (expected r) EOF) -- not sure how this happens
    (ls, r) ->
      let i = maximum (map snd ls)
       in case map fst (filter ((== i) . snd) ls) of
            [] -> error "impossible"
            [one] -> let ts' = drop i ts in pure (ts', one)
            (e : es) -> Left (Ambiguous (e :| es))

type GrammarResult = Either (TopLevel' TempExpr (Expr TempExpr)) (Expr TempExpr)

grammar :: Table -> Grammar r (Prod r Text (Position, Token) GrammarResult, Prod r Text (Position, Token) GrammarResult)
grammar table = mdo
  intLit <- rule $ terminal (\t -> case t of (p, IntLitTok s) -> Just s; _ -> Nothing)
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
        <|> ((Lam . fst <$> identToken "fun" <* lparen <*> (map E <$> argList) <* rparen <* lbrace <*> expr <* rbrace) <?> "anonymous function")
  fieldList <-
    rule $
      (:) <$> ((,) <$> rawName <* identToken ":" <*> expr) <*> (identToken "," *> fieldList <|> pure [])
        <|> pure []
  argList <-
    rule $
      (:) <$> expr <*> (identToken "," *> argList <|> pure [])
        <|> pure []
  normalApp <-
    rule $
      atom
        <|> ((Projection <$> normalApp <*> projection) <?> "field projection")
        <|> ((app <$> normalApp <* lparen <*> argList <* rparen) <?> "function application")
  expr' <- mixfixExpression tbl normalApp makeAp
  expr <- rule $ expr' <?> "expression"
  associativity <-
    rule $
      LeftAssoc <$ identToken "left"
        <|> RightAssoc <$ identToken "right"
        <|> pure NonAssoc
  glob <-
    rule $
      (:) <$> many globName <*> (identToken "," *> glob <|> pure [])
  topLevel <-
    rule $
      SyntaxDecl [] . fst <$> identToken "syntax" <*> many rawName <*> intLit <*> associativity <* semicolon
        <|> ActionDecl [] <$ identToken "action" <*> (Bind <$> expr <* identToken "=" <*> expr <* semicolon)
        <|> Binding [] <$ identToken "let" <*> (Bind <$> expr <* identToken "=" <*> expr <* semicolon)
        <|> MacroDecl [] <$ identToken "macro" <*> expr <*> pure [] <* identToken "=" <*> expr <* semicolon
        <|> Properties . fst <$> identToken "check" <*> glob <* identToken "with" <*> glob <*> (Just <$ identToken "when" <*> expr <* semicolon <|> Nothing <$ semicolon)
        <|> Imported . fst <$> identToken "import" <*> (mconcat <$> many importName) <*> pure [] <* semicolon
        <|> DocBlock . pure <$> docToken
        <?> "Top level declaration"
  replCommand <- rule $ Left <$> topLevel <|> Right <$> expr <* eofTok
  topLevel' <- rule $ Left <$> topLevel
  return (replCommand, topLevel')
  where
    app x [] = x
    app f (x : xs) = app (App f x) xs
    tbl =
      map (map $ first $ map $ fmap identToken) (negativeHoles table)
        ++ map (map $ first $ map $ fmap identToken) (positiveHoles table)

    mixfixParts =
      [ s | xs <- positiveHoles table ++ negativeHoles table, (ys, _) <- xs, Just s <- ys
      ]
    lbrace = identToken "{"
    rbrace = identToken "}"
    lbrack = identToken "["
    rbrack = identToken "]"
    eofTok = satisfy ((== EOF) . snd) <?> "end of file"
    lparen = satisfy ((== LParen) . snd) <?> "left parenthesis"
    rparen = satisfy ((== RParen) . snd) <?> "right parenthesis"
    colon = satisfy ((== Ident ":") . snd) <?> "colon"
    semicolon = satisfy ((== Ident ";") . snd) <?> "semicolon"
    identToken s = isToken (Ident s) s
    isToken s t = satisfy ((== s) . snd) <?> t
    projection = terminal $ \(p, t) ->
      case t of
        ProjectionTok s -> pure s
        _ -> Nothing
    docToken = terminal $ \(p, t) ->
      case t of
        DocTok s -> pure s
        _ -> Nothing
    globName = terminal $ \(p, t) ->
      case t of
        Ident "*" -> pure Nothing
        Ident s | s `notElem` [",", "with", "when"] -> pure (Just s)
        _ -> Nothing
    rawName = terminal $ \(p, t) ->
      case t of
        Ident s -> pure s
        _ -> Nothing
    importName = terminal $ \(p, t) ->
      case t of
        Ident s | s `notElem` [";", ","] -> pure s
        _ -> Nothing
    symbol = Symbol . fst <$> colon <*> rawName
    variable = terminal $ \(p, t) ->
      case t of
        Ident s -> guard (s `notElem` mixfixParts) >> pure (Var p s)
        _ -> Nothing
    makeAp hol as = unpeelAps (unholey hol) as
    unholey ls = Var (getPosition ls) (foldMap (fromMaybe "_") (map (fmap (unident . snd)) ls))
    getPosition ls = case filter isJust ls of
      [] -> error "No concrete token: the impossible happened"
      (Just (p, _) : _xs) -> p
      (_ : xs) -> getPosition xs

unident :: Token -> Text
unident (Ident s) = s
unident _ = "???"
