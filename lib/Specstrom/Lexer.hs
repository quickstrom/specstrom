{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Specstrom.Lexer where

import Control.Arrow (first)
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Text.Read (readMaybe)

data Kwd = Define | Syntax | Let | Import | Check | With | Fun | When deriving (Show, Eq)

data Token
  = Ident Text
  | Reserved Kwd
  | ProjectionTok Text
  | StringLitTok Text
  | CharLitTok Char
  | IntLitTok Int
  | FloatLitTok Double
  | SelectorLitTok Text
  | LParen
  | RParen
  | Semi
  | Dot
  | EOF
  deriving (Show, Eq)

type Position = (FilePath, Int, Int)

dummyPosition :: Position
dummyPosition = ("", 0, 0)

addRow :: Int -> Position -> Position
addRow i (f, l, _c) = (f, l + i, 0)

nextRow :: Position -> Position
nextRow = addRow 1

addCol :: Int -> Position -> Position
addCol i (f, l, c) = (f, l, c + i)

nextCol :: Position -> Position
nextCol = addCol 1

advance :: Text -> Position -> Position
advance t = case Text.uncons t of
  Nothing -> id
  Just ('\n', cs) -> advance cs . addRow 1
  Just (_c, cs) -> advance cs . addCol 1

data LexerError
  = InvalidIntLit Position Text
  | InvalidCharLit Position Text
  | InvalidStringLit Position Text
  | InvalidFloatLit Position Text
  | UnterminatedCharLit Position
  | UnterminatedStringLit Position
  | UnterminatedSelectorLit Position
  deriving (Show)

readLiteral :: Char -> Text -> Maybe (Text, Text)
readLiteral d = fmap (first ((<> Text.singleton d) . (Text.singleton d <>))) . readLiteral' d

readLiteral' :: Char -> Text -> Maybe (Text, Text)
readLiteral' delimiter rest =
  let (chunk, rest') = Text.break (`elem` [delimiter, '\\']) rest
   in case Text.uncons rest' of
        Just ('\\', Text.uncons -> Just (x, xs)) -> first ((chunk <>) . (Text.singleton '\\' <>) . (Text.singleton x <>)) <$> readLiteral' delimiter xs
        Just (x, xs) | x == delimiter -> Just (chunk, xs)
        _ -> Nothing

reserved :: Char -> Bool
reserved c = isSpace c || c `elem` ("();." :: [Char])

lexer :: Position -> Text -> Either LexerError [(Position, Token)]
lexer p t
  | Text.take 2 t == "//" =
    lexer (nextRow p) (Text.drop 1 (Text.dropWhile (/= '\n') (Text.drop 2 t)))
  | otherwise = case Text.uncons t of
    Nothing -> Right [(p, EOF)]
    Just ('\n', cs) -> lexer (nextRow p) cs
    Just (c, cs) | isSpace c -> lexer (nextCol p) cs
    Just (i, _cs) | isDigit i -> case Text.span isDigit t of
      (digits, Text.uncons -> Just ('.', rest)) ->
        let (decimals, rest') = Text.span (\x -> isDigit x || x `elem` ("eE+-" :: [Char])) rest
            candidate = digits <> "." <> decimals
         in case Text.double candidate of
              Left _ -> Left $ InvalidFloatLit p candidate
              Right (v, _) -> ((p, FloatLitTok v) :) <$> lexer (advance candidate p) rest'
      (digits, rest) -> case Text.decimal digits of
        Left _ -> Left $ InvalidIntLit p digits
        Right (v, _) -> ((p, IntLitTok v) :) <$> lexer (advance digits p) rest
    Just ('\'', cs) -> case readLiteral '\'' cs of
      Nothing -> Left $ UnterminatedCharLit p
      Just (lit, rest) -> case readMaybe (Text.unpack lit) of
        Nothing -> Left $ InvalidCharLit p lit
        Just c -> ((p, CharLitTok c) :) <$> lexer (advance lit p) rest
    Just ('\"', cs) -> case readLiteral '\"' cs of
      Nothing -> Left $ UnterminatedStringLit p
      Just (lit, rest) -> case readMaybe (Text.unpack lit) of
        Nothing -> Left $ InvalidStringLit p lit
        Just c -> ((p, StringLitTok c) :) <$> lexer (advance lit p) rest
    Just ('`', cs) -> case readLiteral '`' cs of
      Nothing -> Left $ UnterminatedSelectorLit p
      Just (lit, rest) -> ((p, SelectorLitTok (Text.drop 1 (Text.take (Text.length lit - 1) lit))) :) <$> lexer (advance lit p) rest
    Just ('(', cs) -> ((p, LParen) :) <$> lexer (nextCol p) cs
    Just (')', cs) -> ((p, RParen) :) <$> lexer (nextCol p) cs
    Just (';', cs) -> ((p, Semi) :) <$> lexer (nextCol p) cs
    Just ('.', cs) ->
      let (candidate, rest) = Text.break reserved cs
       in if Text.null candidate
            then ((p, Dot) :) <$> lexer (nextCol p) cs
            else ((p, ProjectionTok candidate) :) <$> lexer (advance candidate p) rest
    Just (c, cs) ->
      let (candidate, rest) = Text.break reserved t
       in ((p, fromCandidate candidate) :) <$> lexer (advance candidate p) rest

fromCandidate :: Text -> Token
fromCandidate "=" = Reserved Define
fromCandidate "syntax" = Reserved Syntax
fromCandidate "let" = Reserved Let
fromCandidate "import" = Reserved Import
fromCandidate "check" = Reserved Check
fromCandidate "with" = Reserved With
fromCandidate "when" = Reserved When
fromCandidate "fun" = Reserved Fun
fromCandidate s = Ident s
