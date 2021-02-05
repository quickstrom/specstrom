{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import Data.Char
import Text.Read (readMaybe)
import Control.Arrow (first)
import Data.Text (Text)
data Kwd = Define | Syntax | Let deriving (Show, Eq)
data Token = Ident Text | Reserved Kwd 
           | StringLitTok Text | CharLitTok Char | IntLitTok Int | FloatLitTok Double | SelectorLitTok Text
           | LParen | RParen | Semi | EOF deriving (Show, Eq)

type Position = (Text, Int, Int)

addRow :: Int -> Position -> Position
addRow i (f, l,c) = (f, l+i,0)
nextRow :: Position -> Position
nextRow = addRow 1
addCol :: Int -> Position -> Position
addCol i (f, l,c) = (f,l,c+i)
nextCol :: Position -> Position
nextCol = addCol 1

advance :: Text -> Position -> Position
advance [] = id
advance ('\n':cs) = advance cs . addRow 1
advance (c:cs) = advance cs . addCol 1

data LexerError = InvalidIntLit Position Text
                | InvalidCharLit Position Text
                | InvalidStringLit Position Text
                | InvalidFloatLit Position Text 
                | UnterminatedCharLit Position
                | UnterminatedStringLit Position 
                | UnterminatedSelectorLit Position 
                deriving (Show)

readLiteral :: Char -> Text -> Maybe (Text, Text)
readLiteral d = fmap (first ((++[d]) . (d:))) . readLiteral' d

readLiteral' :: Char -> Text -> Maybe (Text, Text)
readLiteral' delimiter rest = let (chunk, rest') = break (`elem` [delimiter, '\\']) rest
                              in case rest' of 
                                  ('\\':x:xs) -> first ((chunk++) . ('\\':). (x:)) <$> readLiteral' delimiter xs
                                  (x:xs) | x == delimiter -> Just (chunk, xs)
                                  _ -> Nothing

reservedInitial c = isSpace c || c `elem` "()123456789'\";"
reserved c = isSpace c || c `elem` "();"

lexer :: Position -> Text -> Either LexerError [(Position, Token)]
lexer p [] = Right [(p,EOF)]
lexer p ('\n':cs) = lexer (nextRow p) cs
lexer p (c:cs) | isSpace c = lexer (nextCol p) cs
lexer p ('/':'/':cs) = lexer (nextRow p) (drop 1 (dropWhile (/= '\n') cs))
lexer p (i:cs) | isDigit i = case span isDigit (i:cs) of
   (digits, '.':rest) -> let 
         (decimals, rest') = span (\x -> isDigit x || x `elem` "eE+-") rest
         candidate = digits ++ '.':decimals
      in case readMaybe candidate of
           Nothing -> Left $ InvalidFloatLit p candidate
           Just v  -> ((p,FloatLitTok v):) <$> lexer (advance candidate p) rest'
   (digits, rest) -> case readMaybe digits of 
      Nothing -> Left $ InvalidIntLit p digits
      Just v  -> ((p,IntLitTok v):) <$> lexer (advance digits p) rest
lexer p ('\'':cs) = case readLiteral '\'' cs of 
                      Nothing -> Left $ UnterminatedCharLit p
                      Just (lit,rest) -> case readMaybe lit of
                        Nothing -> Left $ InvalidCharLit p lit
                        Just c  -> ((p, CharLitTok c):) <$> lexer (advance lit p) rest
lexer p ('\"':cs) = case readLiteral '\"' cs of 
                      Nothing -> Left $ UnterminatedStringLit p
                      Just (lit,rest) -> case readMaybe lit of
                        Nothing -> Left $ InvalidStringLit p lit
                        Just c  -> ((p, StringLitTok c):) <$> lexer (advance lit p) rest
lexer p ('`':cs) = case readLiteral '`' cs of
                     Nothing -> Left $ UnterminatedSelectorLit p
                     Just (lit, rest) -> ((p,SelectorLitTok (drop 1 (take (length lit - 1) lit))):) <$> lexer (advance lit p) rest
lexer p ('(':cs) = ((p,LParen):) <$> lexer (nextCol p) cs
lexer p (')':cs) = ((p,RParen):) <$> lexer (nextCol p) cs
lexer p (';':cs) = ((p,Semi):) <$> lexer (nextCol p) cs
lexer p (c:cs) = let (candidate, rest) = break reserved (c:cs)
                  in ((p,fromCandidate candidate):) <$> lexer p rest

fromCandidate :: Text -> Token
fromCandidate "=" = Reserved Define
fromCandidate "syntax" = Reserved Syntax 
fromCandidate "let" = Reserved Let
fromCandidate s = Ident s



