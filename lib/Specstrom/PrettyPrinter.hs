{-# LANGUAGE FlexibleInstances #-}
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
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Specstrom.Evaluator as Evaluator
import Specstrom.Lexer
import Specstrom.Load (LoadError (LoadParseError, LoadTypeInfError))
import Specstrom.Parser
import Specstrom.Syntax
import Specstrom.TypeInf
import Text.Earley.Mixfix (Associativity (..))

prettyEvalError :: [Doc AnsiStyle] -> Evaluator.EvalError -> Doc AnsiStyle
prettyEvalError bts (Evaluator.Backtrace p n r) = prettyEvalError ((pretty n <+> "@" <+> prettyPos p) : bts) r
prettyEvalError bts (Evaluator.Error e) = annotate (bold <> color Red) (pretty e) <> line <> indent 2 (vcat bts)

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
prettyValue (Evaluator.Object b o) = "{" <> sep (punctuate comma (map (\(k, v) -> pretty k <> ":" <+> prettyValue v) (M.toList o))) <> (if b then "}" else " ... }")
prettyValue v = pretty (show v) -- for now

prettyPos :: Position -> Doc AnsiStyle
prettyPos (f, l, c) = pretty f <> ":" <> pretty l <> ":" <> pretty c

errorMessage :: Position -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
errorMessage p s extra =
  annotate (bold <> color Red) (prettyPos p <> ":" <+> s) <> line <> indent 2 (vcat extra)

errorMessageNoPos :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
errorMessageNoPos s extra =
  annotate (bold <> color Red) s <> line <> indent 2 (vcat extra)

prettyTypeError :: TypeInfError -> Doc AnsiStyle
prettyTypeError (TypeInfError p (StrE s : rest)) = errorMessage p (pretty s) (map prettyTyBit rest)
prettyTypeError (TypeInfError p rest) = errorMessage p "Type error" (map prettyTyBit rest)

prettyTyBit :: TypeErrorBit -> Doc AnsiStyle
prettyTyBit (StrE s) = pretty s
prettyTyBit (TypeE t) = prettyType t
prettyTyBit (PtnE t) = prettyPattern t
prettyTyBit (VarNameE s) = ident s

prettyType :: Type -> Doc AnsiStyle
prettyType (Arrow t1 t2) = parens (prettyType t1 <+> keyword "->" <+> prettyType t2)
prettyType Value = keyword "@"
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
prettyParseError (SyntaxAlreadyDeclared n p) = errorMessage p "syntax already declared:" [ident n]
prettyParseError (InvalidMacroLHS p) = errorMessage p "Invalid macro definition" []
prettyParseError (ExpectedPattern e) = errorMessage (exprPos e) "expected pattern, got:" [prettyExpr e]
prettyParseError (ExpectedGot p s t) =
  errorMessage
    p
    "expected one of:"
    [sep (punctuate comma (map pretty s)), annotate (bold <> color Red) "but got:", prettyToken t]
prettyParseError (Ambiguous (e :| es)) =
  errorMessage (either (tlPos exprPos) exprPos e) "ambiguous expression; can be parsed as:" $
    punctuate (line <> annotate (bold <> color Red) "or:") (map (either prettyToplevel prettyExpr) (e : es))
prettyParseError (DuplicatePatternBinding p [b]) = errorMessage p "duplicate bound variable in pattern:" [pretty b]
prettyParseError (DuplicatePatternBinding p bs) =
  errorMessage p "duplicate bound variables in pattern:" [sep (punctuate comma (map pretty bs))]

prettyLoadError :: LoadError -> Doc AnsiStyle
prettyLoadError (LoadParseError e) = prettyParseError e
prettyLoadError (LoadTypeInfError e) = prettyTypeError e

prettyToken :: Token -> Doc AnsiStyle
prettyToken (Ident s) = ident s
prettyToken (ProjectionTok t) = projection ("." <> t)
prettyToken (StringLitTok str) = literal (pretty (show str))
prettyToken (CharLitTok str) = literal (pretty (show str))
prettyToken (IntLitTok str) = literal (pretty (show str))
prettyToken (FloatLitTok str) = literal (pretty (show str))
prettyToken (SelectorLitTok str) = literal ("`" <> pretty str <> "`")
prettyToken LParen = "("
prettyToken (DocTok str) = docStyle $ "///" <> pretty str
prettyToken RParen = ")"
prettyToken EOF = "EOF"

prettyBind :: (PrettyPattern p, PrettyPattern q) => Bind' p q -> Doc AnsiStyle
prettyBind b = keyword "let" <+> prettyBind' b

prettyBindBrief :: (PrettyPattern p, PrettyPattern q) => Bind' p q -> Doc AnsiStyle
prettyBindBrief (Bind bp ps) = prettyPattern bp

prettyBind' :: (PrettyPattern p, PrettyPattern q) => Bind' p q -> Doc AnsiStyle
prettyBind' (Bind bp bs) =
  prettyPattern bp
    <+> nest
      3
      ( keyword "=" <> softline
          <> (prettyExpr bs <> keyword ";")
      )

prettyAll :: (PrettyPattern p, PrettyPattern q) => [TopLevel' p q] -> Doc AnsiStyle
prettyAll = vcat . map prettyToplevel

prettyGlob :: Glob -> Doc AnsiStyle
prettyGlob = hsep . map prettyGlobTerm
  where
    prettyGlobTerm = hcat . map (maybe "*" ident)

prettyDocs :: Text -> Doc AnsiStyle
prettyDocs t = docStyle $ "///" <> pretty t

prettyToplevelHeader :: TopLevel -> Doc AnsiStyle
prettyToplevelHeader (Properties _p g1 g2 g3) =
  keyword "check" <+> prettyGlob g1 <+> keyword "with" <+> prettyGlob g2
    <> maybe mempty ((space <>) . (keyword "when" <+>) . prettyExpr) g3
    <> keyword ";"
prettyToplevelHeader (Binding docs b) = prettyBindBrief b
prettyToplevelHeader (DocBlock docs) = ""
prettyToplevelHeader (ActionDecl docs b) = prettyBindBrief b
prettyToplevelHeader (MacroDecl docs lhs _vars _rhs) = prettyExpr lhs
prettyToplevelHeader (SyntaxDecl docs _ tokens lv assoc) = hsep (map pretty tokens) <+> prettyLit (IntLit lv) <+> prettyAssoc assoc <> keyword ";"
prettyToplevelHeader (Imported _ i bs) = literal (pretty i) <> keyword ";"

prettyAssoc :: Associativity -> Doc AnsiStyle
prettyAssoc LeftAssoc = "left"
prettyAssoc RightAssoc = "right"
prettyAssoc _ = ""

prettyToplevel :: (PrettyPattern p, PrettyPattern q) => TopLevel' p q -> Doc AnsiStyle
prettyToplevel (Properties _p g1 g2 g3) =
  keyword "check" <+> prettyGlob g1 <+> keyword "with" <+> prettyGlob g2
    <> maybe mempty ((space <>) . (keyword "when" <+>) . prettyExpr) g3
    <> keyword ";"
prettyToplevel (Binding docs b) = vcat $ map prettyDocs docs ++ [prettyBind b]
prettyToplevel (DocBlock docs) = vcat $ map prettyDocs docs
prettyToplevel (ActionDecl docs b) = vcat $ map prettyDocs docs ++ [keyword "action" <+> prettyBind' b]
prettyToplevel (MacroDecl docs lhs _vars rhs) =
  vcat $
    map prettyDocs docs
      ++ [keyword "macro" <+> prettyExpr lhs <+> keyword "=" <+> prettyExpr rhs <> keyword ";"]
prettyToplevel (SyntaxDecl docs _ tokens lv assoc) =
  vcat $
    map prettyDocs docs
      ++ [keyword "syntax" <+> hsep (map pretty tokens) <+> prettyLit (IntLit lv) <+> prettyAssoc assoc <> keyword ";"]
prettyToplevel (Imported _ i bs) = keyword "import" <+> literal (pretty i) <> keyword ";" <> line <> indent 2 (prettyAll bs)

prettyLit :: Lit -> Doc AnsiStyle
prettyLit (CharLit s) = literal (pretty (show s))
prettyLit (StringLit s) = literal (pretty (show s))
prettyLit (SelectorLit (Selector s)) = literal ("`" <> pretty s <> "`")
prettyLit (IntLit s) = literal (pretty (show s))
prettyLit (FloatLit s) = literal (pretty (show s))

topPatternToExpr :: TopPattern -> Expr TempExpr
topPatternToExpr (LazyP p n) = App (Var n "~_") (Var n p)
topPatternToExpr (MatchP p) = patternToExpr p
topPatternToExpr (MacroExpansionTP p e) = MacroExpansion (topPatternToExpr p) e

patternToExpr :: Pattern -> Expr TempExpr
patternToExpr (VarP p n) = Var n p
patternToExpr (MacroExpansionP p e) = MacroExpansion (patternToExpr p) e
patternToExpr (IgnoreP p) = Var p "_"
patternToExpr (LitP p l) = Literal p l
patternToExpr (BoolP p l) = if l then Var p "true" else Var p "false"
patternToExpr (NullP p) = Var p "null"
patternToExpr (ListP p ps) = ListLiteral p (map patternToExpr ps)
patternToExpr (ObjectP p ps) = ObjectLiteral p (map (second patternToExpr) ps)
patternToExpr (ActionP n p ps) = unpeelAps (Var p n) (map patternToExpr ps)
patternToExpr (SymbolP n p ps) = unpeelAps (Symbol p n) (map patternToExpr ps)

bindPatternToExpr :: BindPattern -> Expr TempExpr
bindPatternToExpr (MacroExpansionBP _ e) = e
bindPatternToExpr (FunP n p ps) = unpeelAps (Var p n) (map topPatternToExpr ps)
bindPatternToExpr (Direct p) = topPatternToExpr p

class PrettyPattern a where
  prettyPattern :: a -> Doc AnsiStyle

instance PrettyPattern TopPattern where
  prettyPattern p = prettyExpr (topPatternToExpr p)

instance PrettyPattern Pattern where
  prettyPattern p = prettyExpr (patternToExpr p)

instance PrettyPattern (Expr TempExpr) where
  prettyPattern p = prettyExpr p

instance PrettyPattern TempExpr where
  prettyPattern (E e) = prettyExpr e

instance PrettyPattern BindPattern where
  prettyPattern p = prettyExpr (bindPatternToExpr p)

prettyExpr :: (PrettyPattern p) => Expr p -> Doc AnsiStyle
prettyExpr trm = renderTerm True trm
  where
    renderTerm :: (PrettyPattern q) => Bool -> Expr q -> Doc AnsiStyle
    renderTerm outer t
      | (x, []) <- peelAps t [] = case x of
        Var _ s -> ident s
        Symbol _ s -> symbol (":" <> pretty s)
        Literal _p l -> prettyLit l
        MacroExpansion _ e -> renderTerm outer e
        Projection e pr -> renderTerm False e <> projection ("." <> pr)
        App {} -> mempty -- Handled by peelAps
        ListLiteral _ ls -> "[" <> hsep (punctuate comma $ map (renderTerm True) ls) <> "]"
        ObjectLiteral _ ls -> "{" <> hsep (punctuate comma $ map (\(i, e) -> pretty i <> ":" <+> renderTerm True e) ls) <> "}"
        Lam _ n e ->
          (if outer then id else parens) $
            "fun(" <> hsep (punctuate comma $ map prettyPattern n) <> ") {" <+> renderTerm True e <+> "}"
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

docStyle :: Doc AnsiStyle -> Doc AnsiStyle
docStyle = annotate (colorDull Yellow)
