module Lexer where
import Data.Char
import Text.Read (readMaybe)
import Control.Arrow (first)
data Kwd = Define | Syntax | Let deriving (Show, Eq)
data Token = Ident String | Reserved Kwd 
           | StringLitTok String | CharLitTok Char | IntLitTok Int | FloatLitTok Double | SelectorLitTok String
           | LParen | RParen | Semi | EOF deriving (Show, Eq)

type Position = (String, Int, Int)

addRow :: Int -> Position -> Position
addRow i (f, l,c) = (f, l+i,0)
nextRow :: Position -> Position
nextRow = addRow 1
addCol :: Int -> Position -> Position
addCol i (f, l,c) = (f,l,c+i)
nextCol :: Position -> Position
nextCol = addCol 1

advance :: String -> Position -> Position
advance [] = id
advance ('\n':cs) = advance cs . addRow 1
advance (c:cs) = advance cs . addCol 1

data LexerError = InvalidIntLit Position String
                | InvalidCharLit Position String
                | InvalidStringLit Position String
                | InvalidFloatLit Position String 
                | UnterminatedCharLit Position
                | UnterminatedStringLit Position 
                | UnterminatedSelectorLit Position 
                deriving (Show)

readLiteral :: Char -> String -> Maybe (String, String)
readLiteral d = fmap (first ((++[d]) . (d:))) . readLiteral' d

readLiteral' :: Char -> String -> Maybe (String, String)
readLiteral' delimiter rest = let (chunk, rest') = break (`elem` [delimiter, '\\']) rest
                              in case rest' of 
                                  ('\\':x:xs) -> first ((chunk++) . ('\\':). (x:)) <$> readLiteral' delimiter xs
                                  (x:xs) | x == delimiter -> Just (chunk, xs)
                                  _ -> Nothing

reservedInitial c = isSpace c || c `elem` "()123456789'\";"
reserved c = isSpace c || c `elem` "();"

lexer :: Position -> String -> Either LexerError [(Position, Token)]
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

fromCandidate :: String -> Token
fromCandidate "=" = Reserved Define
fromCandidate "syntax" = Reserved Syntax 
fromCandidate "let" = Reserved Let
fromCandidate s = Ident s



