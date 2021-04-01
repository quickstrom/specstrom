{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module Specstrom.PrettyPrinter where

import Data.Bifunctor (second)
import qualified Data.HashMap.Strict as M
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Prettyprinter.Render.Terminal
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Lexer
import Specstrom.Parser
import Specstrom.Syntax
import Specstrom.TypeInf

prettyValue :: Evaluator.Value -> Doc AnsiStyle
prettyValue (Evaluator.Action n [] _) = pretty n
prettyValue (Evaluator.Action n vs _) = pretty n <> "(" <> sep (punctuate comma (map prettyValue vs)) <> ")"
prettyValue (Evaluator.Closure (n, _, i) _ _ _) = "<<function:" <> pretty n <> "|" <> pretty (show i) <> ">>"
prettyValue (Evaluator.Trivial) = "true"
prettyValue (Evaluator.Absurd) = "false"
prettyValue (Evaluator.Constructor n []) = symbol (":" <> pretty n)
prettyValue (Evaluator.Constructor n vs) = symbol (":" <> pretty n) <> "(" <> sep (punctuate comma (map prettyValue vs)) <> ")"
prettyValue (Evaluator.Null) = "null"
prettyValue (Evaluator.List vs) = "[" <> sep (punctuate comma (map prettyValue vs)) <> "]"
prettyValue (Evaluator.LitVal l) = prettyLit l
prettyValue (Evaluator.Object o) = "{" <> sep (punctuate comma (map (\(k, v) -> pretty k <> ":" <+> prettyValue v) (M.toList o))) <> "}"
prettyValue v = pretty (show v) -- for now

prettyPos :: Position -> Doc AnsiStyle
prettyPos (f, l, c) = pretty f <> ":" <> pretty l <> ":" <> pretty c

errorMessage :: Position -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
errorMessage p s extra =
  annotate (bold <> color Red) (prettyPos p <> ":" <+> s) <> line <> indent 2 (vcat extra)

errorMessageNoPos :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
errorMessageNoPos s extra =
  annotate (bold <> color Red) s <> line <> indent 2 (vcat extra)

prettyTypeError :: (Position, [TypeErrorBit]) -> Doc AnsiStyle
prettyTypeError (p, (StrE s : rest)) = errorMessage p (pretty s) (map prettyTyBit rest)
prettyTypeError (p, rest) = errorMessage p "Type error" (map prettyTyBit rest)

prettyTyBit :: TypeErrorBit -> Doc AnsiStyle
prettyTyBit (StrE s) = pretty s
prettyTyBit (TypeE t) = prettyType t
prettyTyBit (PtnE t) = prettyPattern t
prettyTyBit (VarNameE s) = ident s

prettyType :: Type -> Doc AnsiStyle
prettyType (Arrow t1 t2) = parens (prettyType t1 <+> keyword "->" <+> prettyType t2)
prettyType (Value) = keyword "@"
prettyType (TyVar n) = ident n

prettyLexerError :: LexerError -> Doc AnsiStyle
prettyLexerError (InvalidIntLit p s) = errorMessage p "invalid integer literal:" [pretty s]
prettyLexerError (InvalidCharLit p s) = errorMessage p "invalid char literal:" [pretty s]
prettyLexerError (InvalidStringLit p s) = errorMessage p "invalid string literal:" [pretty s]
prettyLexerError (InvalidFloatLit p s) = errorMessage p "invalid float literal:" [pretty s]
prettyLexerError (UnterminatedCharLit p) = errorMessage p "no closing quote for char literal" []
prettyLexerError (UnterminatedStringLit p) = errorMessage p "no closing quote for string literal" []
prettyLexerError (UnterminatedSelectorLit p) = errorMessage p "no closing backtick for selector literal" []

prettyParseError :: ParseError -> Doc AnsiStyle
prettyParseError (LexerFailure e) = prettyLexerError e
prettyParseError (ModuleNotFound p n) = errorMessage p "module not found" [ident n]
prettyParseError (MalformedSyntaxDeclaration p) = errorMessage p "malformed syntax declaration" []
prettyParseError (SyntaxAlreadyDeclared n p) = errorMessage p "syntax already declared:" [ident n]
prettyParseError (ExpectedPattern e) = errorMessage (exprPos e) "expected pattern, got:" [prettyExpr e]
prettyParseError (ExpectedPattern' e) = errorMessage (exprPos e) "expected pattern, got:" [prettyExpr e]
prettyParseError (ExpectedSemicolon p) = errorMessage p "expected semicolon." []
prettyParseError (ExpectedSemicolonOrWhen p) = errorMessage p "expected semicolon or 'when'." []
prettyParseError (ExpectedEquals p) = errorMessage p "expected equals sign." []
prettyParseError (ExpectedModuleName p) = errorMessage p "expected module name." []
prettyParseError (ExpectedWith p) = errorMessage p "expected 'with'." []
prettyParseError (ExpectedGot p s t) =
  errorMessage
    p
    "expected one of:"
    [sep (punctuate comma (map pretty s)), annotate (bold <> color Red) "but got:", prettyToken t]
prettyParseError (ExpressionAmbiguous (e :| es)) =
  errorMessage (exprPos e) "ambiguous expression; can be parsed as:" $
    punctuate (line <> annotate (bold <> color Red) "or:") (map prettyExpr (e : es))
prettyParseError (DuplicatePatternBinding p [b]) = errorMessage p "duplicate bound variable in pattern:" [pretty b]
prettyParseError (DuplicatePatternBinding p bs) =
  errorMessage p "duplicate bound variables in pattern:" [sep (punctuate comma (map pretty bs))]
prettyParseError (TrailingGarbage p t) = errorMessage p "trailing tokens in file:" [prettyToken t]

prettyToken :: Token -> Doc AnsiStyle
prettyToken (Ident s) = ident s
prettyToken (Reserved Define) = keyword "="
prettyToken (Reserved Let) = keyword "let"
prettyToken (Reserved Fun) = keyword "fun"
prettyToken (Reserved When) = keyword "when"
prettyToken (Reserved Check) = keyword "check"
prettyToken (Reserved With) = keyword "with"
prettyToken (Reserved Case) = keyword "case"
prettyToken (Reserved Import) = keyword "import"
prettyToken (Reserved Action) = keyword "action"
prettyToken (ProjectionTok t) = projection ("." <> t)
prettyToken (Reserved Syntax) = keyword "syntax"
prettyToken (StringLitTok str) = literal (pretty (show str))
prettyToken (CharLitTok str) = literal (pretty (show str))
prettyToken (IntLitTok str) = literal (pretty (show str))
prettyToken (FloatLitTok str) = literal (pretty (show str))
prettyToken (SelectorLitTok str) = literal ("`" <> pretty str <> "`")
prettyToken LBrace = "{"
prettyToken RBrace = "}"
prettyToken Colon = ":"
prettyToken LParen = "("
prettyToken RParen = ")"
prettyToken LBrack = "["
prettyToken RBrack = "]"
prettyToken Semi = ";"
prettyToken Comma = ","
prettyToken Dot = "."
prettyToken EOF = "EOF"

prettyBind :: Bind -> Doc AnsiStyle
prettyBind b = keyword "let" <+> prettyBind' b

prettyBind' :: Bind -> Doc AnsiStyle
prettyBind' (Bind bp bs) =
  prettyBindPattern bp
    <+> nest
      3
      ( keyword "=" <> softline
          <> (prettyBody bs <> keyword ";")
      )

prettyAll :: [TopLevel] -> Doc AnsiStyle
prettyAll = vcat . map prettyToplevel

prettyGlob :: Glob -> Doc AnsiStyle
prettyGlob = hsep . map prettyGlobTerm
  where
    prettyGlobTerm = hcat . map (maybe "*" ident)

prettyToplevel :: TopLevel -> Doc AnsiStyle
prettyToplevel (Properties _p g1 g2 g3) =
  keyword "check" <+> prettyGlob g1 <+> keyword "with" <+> prettyGlob g2
    <> maybe mempty ((space <>) . (keyword "when" <+>) . prettyExpr) g3
    <> keyword ";"
prettyToplevel (Binding b) = prettyBind b
prettyToplevel (ActionDecl b) = keyword "action" <+> prettyBind' b
prettyToplevel (Imported i bs) = keyword "import" <+> literal (pretty i) <> keyword ";" <> line <> indent 2 (prettyAll bs)

prettyBody :: Body -> Doc AnsiStyle
prettyBody (Local b e) =
  prettyBind b
    <> line
    <> prettyBody e
prettyBody (Done e) = prettyExpr e

prettyLit :: Lit -> Doc AnsiStyle
prettyLit (CharLit s) = literal (pretty (show s))
prettyLit (StringLit s) = literal (pretty (show s))
prettyLit (SelectorLit (Selector s)) = literal ("`" <> pretty s <> "`")
prettyLit (IntLit s) = literal (pretty (show s))
prettyLit (FloatLit s) = literal (pretty (show s))

patternToExpr :: Pattern -> Expr TempExpr
patternToExpr (VarP p n) = Var n p
patternToExpr (IgnoreP p) = Var p "_"
patternToExpr (LitP p l) = Literal p l
patternToExpr (BoolP p l) = if l then Var p "true" else Var p "false"
patternToExpr (NullP p) = Var p "null"
patternToExpr (ListP p ps) = ListLiteral p (map patternToExpr ps)
patternToExpr (ObjectP p ps) = ObjectLiteral p (map (second patternToExpr) ps)
patternToExpr (ActionP n p ps) = unpeelAps (Var p n) (map patternToExpr ps)
patternToExpr (SymbolP n p ps) = unpeelAps (Symbol p n) (map patternToExpr ps)

bindPatternToExpr :: BindPattern -> Expr TempExpr
bindPatternToExpr (FunP n p ps) = unpeelAps (Var p n) (map patternToExpr ps)
bindPatternToExpr (Direct p) = patternToExpr p

class PrettyPattern a where
  prettyPattern :: a -> Doc AnsiStyle

instance PrettyPattern Pattern where
  prettyPattern p = prettyExpr (patternToExpr p)

instance PrettyPattern TempExpr where
  prettyPattern (E e) = prettyExpr e

prettyBindPattern :: BindPattern -> Doc AnsiStyle
prettyBindPattern p = prettyExpr (bindPatternToExpr p)

prettyExpr :: (PrettyPattern p) => Expr p -> Doc AnsiStyle
prettyExpr trm = renderTerm True trm
  where
    renderTerm outer t
      | (x, []) <- peelAps t [] = case x of
        Var _ s -> ident s
        Symbol _ s -> symbol (":" <> pretty s)
        Literal _p l -> prettyLit l
        Projection e pr -> renderTerm False e <> projection ("." <> pr)
        Index e e' -> renderTerm False e <> "[" <> renderTerm True e <> "]"
        App {} -> mempty -- Handled by peelAps
        ListLiteral _ ls -> "[" <> hsep (punctuate comma $ map (renderTerm True) ls) <> "]"
        ObjectLiteral _ ls -> "{" <> hsep (punctuate comma $ map (\(i, e) -> pretty i <> ":" <+> renderTerm True e) ls) <> "}"
        Lam _ b n e ->
          (if outer then id else parens) $
            if b then "case" else "fun" <+> prettyPattern n <> "." <+> prettyExpr e
        Freeze _ n e b ->
          (if outer then id else parens) $
            "freeze" <+> prettyPattern n <+> "=" <+> prettyExpr e <> "." <+> prettyExpr b
      | (Var _ name, [Lam _ _ pat e2, e3]) <- peelAps t [],
        name `elem` ["map", "any", "all"] =
        (if outer then id else parens) $
          (case name of "map" -> "for"; "any" -> "exists"; "all" -> "forall"; _ -> "") <+> prettyPattern pat <+> "in" <+> prettyExpr e3 <> "." <+> prettyExpr e2
      | (Var _ n, args) <- peelAps t [],
        Text.length (Text.filter (== '_') n) == length args =
        (if outer then id else parens) $ hsep $ infixTerms n args
      | (x, args) <- peelAps t [] =
        --        (if outer then id else parens) $
        renderTerm False x <> "(" <> hsep (punctuate comma $ map (renderTerm True) args) <> ")"

    infixTerms :: (PrettyPattern p) => Text -> [Expr p] -> [Doc AnsiStyle]
    infixTerms str [] = if Text.null str then [] else [ident str]
    infixTerms (Text.uncons -> Just ('_', str)) (x : xs) = renderTerm False x : infixTerms str xs
    infixTerms str args | (first, rest) <- Text.span (/= '_') str = ident first : infixTerms rest args

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens . align
parensIf False = id

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate bold

literal :: Doc AnsiStyle -> Doc AnsiStyle
literal = annotate (colorDull Cyan)

symbol :: Doc AnsiStyle -> Doc AnsiStyle
symbol = annotate (colorDull Magenta)

ident :: Pretty p => p -> Doc AnsiStyle
ident = annotate (color Black) . pretty

projection :: Pretty p => p -> Doc AnsiStyle
projection = annotate (color Green) . pretty
