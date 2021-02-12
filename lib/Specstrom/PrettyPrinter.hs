{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

module Specstrom.PrettyPrinter where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Prettyprinter.Render.Terminal
import Specstrom.Evaluator
import Specstrom.Lexer
import Specstrom.Parser

prettyPos :: Position -> Doc AnsiStyle
prettyPos (f, l, c) = pretty f <> ":" <> pretty l <> ":" <> pretty c

errorMessage :: Position -> Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
errorMessage p s extra =
  annotate (bold <> color Red) (prettyPos p <> ":" <+> s) <> line <> indent 2 (vcat extra)

errorMessageNoPos :: Doc AnsiStyle -> [Doc AnsiStyle] -> Doc AnsiStyle
errorMessageNoPos s extra =
  annotate (bold <> color Red) s <> line <> indent 2 (vcat extra)

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
prettyParseError (ExpectedSemicolon p) = errorMessage p "expected semicolon." []
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
    punctuate (line <> annotate (bold <> color Red) "or:") (map prettyExpr es)
prettyParseError (DuplicatePatternBinding p [b]) = errorMessage p "duplicate bound variable in pattern:" [pretty b]
prettyParseError (DuplicatePatternBinding p bs) =
  errorMessage p "duplicate bound variables in pattern:" [sep (punctuate comma (map pretty bs))]
prettyParseError (TrailingGarbage p t) = errorMessage p "trailing tokens in file:" [prettyToken t]

prettyToken :: Token -> Doc AnsiStyle
prettyToken (Ident s) = ident s
prettyToken (Reserved Define) = keyword "="
prettyToken (Reserved Let) = keyword "let"
prettyToken (Reserved Check) = keyword "check"
prettyToken (Reserved With) = keyword "with"
prettyToken (Reserved Import) = keyword "import"
prettyToken (ProjectionTok t) = projection ("." <> t)
prettyToken (Reserved Syntax) = keyword "syntax"
prettyToken (StringLitTok str) = literal (pretty (show str))
prettyToken (CharLitTok str) = literal (pretty (show str))
prettyToken (IntLitTok str) = literal (pretty (show str))
prettyToken (FloatLitTok str) = literal (pretty (show str))
prettyToken (SelectorLitTok str) = literal ("`" <> pretty str <> "`")
prettyToken LParen = "("
prettyToken RParen = ")"
prettyToken Semi = ";"
prettyToken Dot = "."
prettyToken EOF = "EOF"

prettyBind :: Bind -> Doc AnsiStyle
prettyBind (Bind n _p ps bs) =
  keyword "let" <+> prettyExpr (unpeelAps (var' n) (map (var' . fst) ps))
    <+> nest
      3
      ( keyword "=" <> softline
          <> (prettyBody bs <> keyword ";")
      )
  where
    var' = Var undefined

prettyAll :: [TopLevel] -> Doc AnsiStyle
prettyAll = vcat . map prettyToplevel

prettyGlob :: Glob -> Doc AnsiStyle
prettyGlob = hsep . map prettyGlobTerm
  where
    prettyGlobTerm = hcat . map (maybe "*" ident)

prettyToplevel :: TopLevel -> Doc AnsiStyle
prettyToplevel (Properties _p g1 g2) = keyword "check" <+> prettyGlob g1 <+> keyword "with" <+> prettyGlob g2 <> keyword ";"
prettyToplevel (Binding b) = prettyBind b
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
prettyLit (SelectorLit s) = literal ("`" <> pretty s <> "`")
prettyLit (IntLit s) = literal (pretty (show s))
prettyLit (FloatLit s) = literal (pretty (show s))

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr trm = renderTerm True trm
  where
    renderTerm outer t
      | (x, []) <- peelAps t [] = case x of
        Var _ s -> ident s
        Literal _p l -> prettyLit l
        Projection e pr -> renderTerm False e <> projection ("." <> pr)
        App {} -> mempty -- Handled by peelAps
        Freeze _ n e b ->
          (if outer then id else parens) $
            "freeze" <+> prettyExpr n <+> "=" <+> prettyExpr e <+> "in" <+> prettyExpr b
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

prettyEvalError :: EvalError -> Doc AnsiStyle
prettyEvalError = \case
  ScopeError pos name -> errorMessage pos "name not in scope" [pretty name]
  ModuloOnFloats d1 d2 -> errorMessageNoPos "invalid modulo on floats" [pretty d1 <+> "%" <+> pretty d2]
  NotAFunction v -> errorMessageNoPos ("not a function") [prettyValue v]
  BinaryOpOnInvalidTypes op v1 v2 -> errorMessageNoPos ("binary operation" <+> prettyBinaryOp op <+> "is not valid on parameters:") [prettyValue v1, prettyValue v2]
  UnaryOpOnInvalidTypes op v -> errorMessageNoPos ("unary operation" <+> prettyUnaryOp op <+> "is not valid on parameters:") [prettyValue v]
  FreezeLHSNotVar pos -> errorMessage pos "left-hand side of freeze is not a variable name" []
  FreezeRHSNotValue pos -> errorMessage pos "right-hand side of freeze is not a value" []
  FreezeInNotFormula pos -> errorMessage pos "body of freeze is not a formula" []
  VariableAlreadyDefined name pos -> errorMessage pos ("variable already defined:" <+> pretty name) []

prettyFormulaExpr :: Int -> FormulaExpr Accessor -> Doc AnsiStyle
prettyFormulaExpr p = \case
  LocExpr _name _pos expr -> prettyFormulaExpr p expr
  Accessor a -> parensIf (p >= 0) ("access" <+> literal (enclose "`" "`" (pretty a)))
  Op op args ->
    case args of
      [e] -> parensIf (p >= opPrecedence op) (prettyUnaryOp op <+> prettyFormulaExpr (opPrecedence op) e)
      [e1, e2] -> parensIf (p >= opPrecedence op) (prettyFormulaExpr (opPrecedence op) e1 <+> prettyBinaryOp op <+> prettyFormulaExpr (opPrecedence op) e2)
      _ -> "<invalid>"
  Constant val -> prettyIValue val
  FreezeVar name _ -> pretty name

opPrecedence :: Op -> Int
opPrecedence = \case
  NotOp -> 6
  AlwaysOp -> 6
  EventuallyOp -> 6
  NextOp -> 6
  MkAccessor -> 1
  Equals -> 7
  NotEquals -> 7
  Plus -> 8
  Times -> 8
  Divide -> 8
  Modulo -> 8
  Less -> 7
  LessEq -> 7
  Greater -> 7
  GreaterEq -> 7
  AndOp -> 4
  OrOp -> 3
  ImpliesOp -> 2
  UntilOp -> 5

prettyUnaryOp :: Op -> Doc ann
prettyUnaryOp NotOp = "not"
prettyUnaryOp AlwaysOp = "always"
prettyUnaryOp EventuallyOp = "eventually"
prettyUnaryOp NextOp = "next"
prettyUnaryOp MkAccessor = "access"

prettyBinaryOp :: Op -> Doc ann
prettyBinaryOp Equals = "=="
prettyBinaryOp NotEquals = "!="
prettyBinaryOp Plus = "+"
prettyBinaryOp Times = "*"
prettyBinaryOp Divide = "/"
prettyBinaryOp Modulo = "%"
prettyBinaryOp Less = "<"
prettyBinaryOp LessEq = "<="
prettyBinaryOp Greater = ">"
prettyBinaryOp GreaterEq = ">="
prettyBinaryOp AndOp = "&&"
prettyBinaryOp OrOp = "||"
prettyBinaryOp ImpliesOp = "==>"
prettyBinaryOp UntilOp = "until"

prettyIValue :: IValue -> Doc AnsiStyle
prettyIValue = \case
  LitVal lit -> prettyLit lit
  BoolVal True -> "true"
  BoolVal False -> "false"
  Null -> "null"

prettyValue :: Value -> Doc AnsiStyle
prettyValue = \case
  Independent v -> prettyIValue v
  StateDependent _ expr -> prettyFormulaExpr 0 expr
  Formula _ f -> prettyFormula 0 f
  Closure _pos _env _args _body -> ""
  PartialOp _op _params -> "<partial op>"

prettyFormula :: Int -> Formula Accessor -> Doc AnsiStyle
prettyFormula p = \case
  Atomic e -> prettyFormulaExpr p e
  Until f1 f2 -> parensIf (p >= 5) (prettyFormula 5 f1 <+> "until" <+> prettyFormula 5 f2)
  Not f -> parensIf (p >= 6) ("not" <+> prettyFormula 6 f)
  And f1 f2 -> parensIf (p >= 4) (prettyFormula 4 f1 <> line <> "&&" <+> align (prettyFormula 4 f2))
  Or f1 f2 -> parensIf (p >= 3) (prettyFormula 3 f1 <> line <> "||" <+> align (prettyFormula 3 f2))
  Implies f1 f2 -> parensIf (p >= 2) (prettyFormula 2 f1 <+> "==>" <+> prettyFormula 2 f2)
  Always f -> parens ("always" <+> prettyFormula 6 f)
  Eventually f -> parensIf (p >= 6) ("eventually" <+> prettyFormula 6 f)
  Next f -> parensIf (p >= 6) ("next" <+> prettyFormula 6 f)
  Trivial -> "true"
  Absurd -> "false"
  LocFormula _name _pos f -> prettyFormula p f
  FreezeIn _pos name e f -> parensIf (p >= 0) (keyword "freeze" <+> pretty name <+> keyword "=" <+> prettyFormulaExpr 0 e <+> keyword "in" <+> prettyFormula 0 f)

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens . align
parensIf False = id

keyword :: Doc AnsiStyle -> Doc AnsiStyle
keyword = annotate bold

literal :: Doc AnsiStyle -> Doc AnsiStyle
literal = annotate (colorDull Cyan)

ident :: Pretty p => p -> Doc AnsiStyle
ident = annotate (color Black) . pretty

projection :: Pretty p => p -> Doc AnsiStyle
projection = annotate (color Green) . pretty
