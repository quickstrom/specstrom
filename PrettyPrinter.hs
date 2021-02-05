module PrettyPrinter where

import Parser
import Lexer
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as P


prettyPos :: Position -> Doc
prettyPos (f,l,c) = text (f ++ ":" ++ show l ++ ":" ++ show c)

errorMessage p s extra = 
  bold (red (prettyPos p <> text ":" <+> text s )) P.<$> indent 2 (vcat extra)

prettyLexerError :: LexerError -> Doc
prettyLexerError (InvalidIntLit p s) = errorMessage p "invalid integer literal:" [text s]
prettyLexerError (InvalidCharLit p s) = errorMessage p "invalid char literal:" [text s]
prettyLexerError (InvalidStringLit p s) = errorMessage p "invalid string literal:" [text s]
prettyLexerError (InvalidFloatLit p s) = errorMessage p "invalid float literal:" [text s]
prettyLexerError (UnterminatedCharLit p) = errorMessage p "no closing quote for char literal" []
prettyLexerError (UnterminatedStringLit p) = errorMessage p "no closing quote for string literal" []

prettyParseError :: ParseError -> Doc
prettyParseError (MalformedSyntaxDeclaration p) = errorMessage p "malformed syntax declaration" []
prettyParseError (SyntaxAlreadyDeclared n p) = errorMessage p "syntax already declared:" [ident n]
prettyParseError (ExpectedPattern e) = errorMessage (exprPos e) "expected pattern, got:" [prettyExpr e]
prettyParseError (ExpectedSemicolon p) = errorMessage p "expected semicolon." []
prettyParseError (ExpectedEquals p) = errorMessage p "expected equals sign." []
prettyParseError (ExpectedGot p s t) = errorMessage p "expected one of:"  
     [sep (punctuate comma (map text s)), bold (red (text "but got:")), prettyToken t]
prettyParseError (ExpressionAmbiguous es@(e:_)) = errorMessage (exprPos e) "ambiguous expression; can be parsed as:"
     $ punctuate (line <> bold (red (text "or:"))) (map prettyExpr es)
prettyParseError (DuplicatePatternBinding p [b]) = errorMessage p "duplicate bound variable in pattern:" [text b]
prettyParseError (DuplicatePatternBinding p bs) 
     = errorMessage p "duplicate bound variables in pattern:" [sep (punctuate comma (map text bs))]

prettyToken :: Token -> Doc
prettyToken (Ident s) = ident s
prettyToken (Reserved Define) = keyword "="
prettyToken (Reserved Let) = keyword "let"
prettyToken (Reserved Syntax) = keyword "syntax"
prettyToken (StringLitTok str) = literal (show str)
prettyToken (CharLitTok str) = literal (show str)
prettyToken (IntLitTok str) = literal (show str)
prettyToken (FloatLitTok str) = literal (show str)
prettyToken (SelectorLitTok str) = literal ("`" <> str <> "`")
prettyToken (LParen) = text "("
prettyToken (RParen) = text ")"
prettyToken (Semi) = text ";"
prettyToken (EOF) = text "EOF"

prettyBody :: Body -> Doc
prettyBody (Bind n ps bs e) = keyword "let" <+> prettyExpr (unpeelAps (var' n) (map var' ps)) <+> nest 3 (keyword "="  </>
                           (prettyBody bs <> keyword ";")) P.<$> prettyBody e
  where var' = Var undefined
prettyBody (Done e) = prettyExpr e

prettyLit :: Lit -> Doc
prettyLit (CharLit s)   = literal (show s)
prettyLit (StringLit s) = literal (show s)
prettyLit (SelectorLit s) = literal ("`" <> s <> "`")
prettyLit (IntLit s)    = literal (show s)
prettyLit (FloatLit s)  = literal (show s)

prettyExpr :: Expr -> Doc
prettyExpr trm = renderTerm True trm
  where
    renderTerm outer t
      | (x, []) <- peelAps t [] = case x of
        Var _ s -> ident s
        Literal p l -> prettyLit l
      | (Var _ n, args) <- peelAps t []
      , length (filter (== '_') n) == length args
      = (if outer then id else parens) $ hsep $ infixTerms n args

      | (x, args) <- peelAps t []
      = (if outer then id else parens) $ hsep $
          renderTerm False x : map (renderTerm False ) args
      where
        infixTerms "" [] = []
        infixTerms str [] = [ident str]
        infixTerms ('_':str) (x : xs) = renderTerm False x : infixTerms str xs
        infixTerms str args | (first, rest) <- span (/= '_') str = ident first : infixTerms rest args


keyword = bold . text
literal = dullcyan . text
ident = black . text
