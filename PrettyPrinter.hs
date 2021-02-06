{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module PrettyPrinter where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Lexer
import Parser
import Prettyprinter.Render.Terminal
import Data.List.NonEmpty (NonEmpty((:|)))

prettyPos :: Position -> Doc AnsiStyle
prettyPos (f, l, c) = pretty f <> ":" <> pretty l <> ":" <> pretty c

errorMessage :: Position -> Text -> [Doc AnsiStyle] -> Doc AnsiStyle
errorMessage p s extra =
  annotate (bold <> color Red) (prettyPos p <> ":" <+> pretty s) <> line <> indent 2 (vcat extra)

prettyLexerError :: LexerError -> Doc AnsiStyle
prettyLexerError (InvalidIntLit p s) = errorMessage p "invalid integer literal:" [pretty s]
prettyLexerError (InvalidCharLit p s) = errorMessage p "invalid char literal:" [pretty s]
prettyLexerError (InvalidStringLit p s) = errorMessage p "invalid string literal:" [pretty s]
prettyLexerError (InvalidFloatLit p s) = errorMessage p "invalid float literal:" [pretty s]
prettyLexerError (UnterminatedCharLit p) = errorMessage p "no closing quote for char literal" []
prettyLexerError (UnterminatedStringLit p) = errorMessage p "no closing quote for string literal" []
prettyLexerError (UnterminatedSelectorLit p) = errorMessage p "no closing backtick for selector literal" []

prettyParseError :: ParseError -> Doc AnsiStyle
prettyParseError (MalformedSyntaxDeclaration p) = errorMessage p "malformed syntax declaration" []
prettyParseError (SyntaxAlreadyDeclared n p) = errorMessage p "syntax already declared:" [ident n]
prettyParseError (ExpectedPattern e) = errorMessage (exprPos e) "expected pattern, got:" [prettyExpr e]
prettyParseError (ExpectedSemicolon p) = errorMessage p "expected semicolon." []
prettyParseError (ExpectedEquals p) = errorMessage p "expected equals sign." []
prettyParseError (ExpectedGot p s t) =
  errorMessage
    p
    "expected one of:"
    [sep (punctuate comma (map pretty s)), annotate (bold <> color Red) "but got:", prettyToken t]
prettyParseError (ExpressionAmbiguous (e :| es)) =
  errorMessage (exprPos e) "ambiguous expression; can be parsed as:" $
    punctuate (line <> annotate (bold <> color Red) "or:") (map prettyExpr es)
prettyParseError (DuplicatePatternBinding p [b]) = errorMessage p "duplicate bound variable in pattern:" [pretty b]
prettyParseError (DuplicatePatternBinding p bs) =
  errorMessage p "duplicate bound variables in pattern:" [sep (punctuate comma (map pretty bs))]

prettyToken :: Token -> Doc AnsiStyle
prettyToken (Ident s) = ident s
prettyToken (Reserved Define) = keyword ("=" :: Text)
prettyToken (Reserved Let) = keyword ("let" :: Text)
prettyToken (Reserved Syntax) = keyword ("syntax" :: Text)
prettyToken (StringLitTok str) = literal (show str)
prettyToken (CharLitTok str) = literal (show str)
prettyToken (IntLitTok str) = literal (show str)
prettyToken (FloatLitTok str) = literal (show str)
prettyToken (SelectorLitTok str) = literal ("`" <> str <> "`")
prettyToken LParen = "("
prettyToken RParen = ")"
prettyToken Semi = ";"
prettyToken EOF = "EOF"

prettyBody :: Body -> Doc AnsiStyle
prettyBody (Bind n _p ps bs e) =
  keyword ("let" :: Text) <+> prettyExpr (unpeelAps (var' n) (map (var' . fst) ps))
    <+> nest
      3
      ( keyword ("=" :: Text) <> softline
          <> (prettyBody bs <> keyword (";" :: Text))
      )
    <> line
    <> prettyBody e
  where
    var' = Var undefined
prettyBody (Done e) = prettyExpr e

prettyLit :: Lit -> Doc AnsiStyle
prettyLit (CharLit s) = literal (show s)
prettyLit (StringLit s) = literal (show s)
prettyLit (SelectorLit s) = literal ("`" <> s <> "`")
prettyLit (IntLit s) = literal (show s)
prettyLit (FloatLit s) = literal (show s)

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr trm = renderTerm True trm
  where
    renderTerm outer t
      | (x, []) <- peelAps t [] = case x of
        Var _ s -> ident s
        Literal _p l -> prettyLit l
        Freeze _ n e b -> "freeze" <+> prettyExpr n <+> "=" <+> prettyExpr e <+> "in" <+> prettyExpr b
        App{} -> mempty -- Handled by peelAps
      | (Var _ n, args) <- peelAps t [],
        Text.length (Text.filter (== '_') n) == length args =
        (if outer then id else parens) $ hsep $ infixTerms n args
      | (x, args) <- peelAps t [] =
        (if outer then id else parens) $
          hsep $
            renderTerm False x : map (renderTerm False) args

    infixTerms :: Text -> [Expr] -> [Doc AnsiStyle]
    infixTerms str [] = if Text.null str then [] else [ident str]
    infixTerms (Text.uncons -> Just ('_', str)) (x : xs) = renderTerm False x : infixTerms str xs
    infixTerms str args | (first, rest) <- Text.span (/= '_') str = ident first : infixTerms rest args

keyword :: Pretty p => p -> Doc AnsiStyle
keyword = annotate bold . pretty

literal :: Pretty p => p -> Doc AnsiStyle
literal = annotate (colorDull Cyan) . pretty

ident :: Pretty p => p -> Doc AnsiStyle
ident = annotate (color Black) . pretty
