use crate::error::{SourceError, Position};
use crate::files::{*};
use std::iter::Peekable;
use peeking_take_while::PeekableExt;
#[derive(Clone, Debug)]
pub struct Token<'a> {
  pub position: Position<'a>,
  pub symbol: Symbol
}

#[derive(Clone, Debug)]
pub enum Symbol {
  Ident(String),
  Projection(String),
  StringLit(String),
  Doc(String),
  CharLit(char),
  IntLit(i64),
  FloatLit(f64),
  SelectorLit(String),
  LParen,
  RParen,
  EOF
}
impl Symbol {
  pub fn width(&self) -> usize {
    match self {
      Symbol::Ident(str) => str.len(),
      Symbol::LParen => 1,
      Symbol::RParen => 1,
      _ => 0
    } 
  }
}


pub struct Lexer<'a,'b> {
  iterator : SourceFileChars<'a,'b>,
}


#[derive(Clone, Debug)]
pub enum LexerError {
  InvalidIntLit(String),
  InvalidCharLit(String),
  InvalidStringLit(String),
  InvalidFloatLit(String),
  UnterminatedCharLit,
  UnterminatedStringLit,
  UnterminatedSelectorLit,
}
impl<'a,'b> Lexer <'a,'b> {
  pub fn source_file(file: &'b SourceFile<'a>) -> Lexer<'a,'b> {
    Lexer { iterator: file.chars() }
  }
}
impl<'a,'b> Iterator for Lexer<'a,'b> {

  type Item = Result<Token<'a>,SourceError<'a,LexerError>>;

  
  fn next (&mut self) -> Option<Self::Item> {
    let mut ch;
    let mut position;
    loop {
      position = self.iterator.position.clone();
      ch = self.iterator.next()?;
      if ch.is_whitespace() {
        // do nothing
      } else if ch == '/' {
        if self.iterator.peek() == Some(&'/') {
          self.iterator.next();
          // we are in a comment, or a doc-string
	  if self.iterator.peek() == Some(&'/') {
            self.iterator.next();
            // it's a doc-string
            let doc = self.iterator.until_eol();
            let tok = Token { position, symbol: Symbol::Doc(doc) };
            return Some(Ok(tok));
          } else {
            // it's a comment
            self.iterator.until_eol();
          }
        } else { break; }
      } else { break; }
    }
    let mut negate = false;
    if ch == '-' && self.iterator.peek().map_or(false,|&x| x.is_ascii_digit()) {
      negate = true;
      position = self.iterator.position.clone();
      ch = self.iterator.next()?;
    }
    if ch.is_ascii_digit() { 
      let mut rest_digits = self.iterator.next_while(|&x| x.is_ascii_digit());
      let sign = if negate { '-' } else { '+' };
      if self.iterator.peek() == Some(&'.') {
        self.iterator.next();
        let rest = self.iterator.next_while(|&x| x.is_ascii_digit());
        let input = if self.iterator.peek() == Some(&'e') || self.iterator.peek() == Some(&'E') {
          self.iterator.next();
          let exponent = self.iterator.next_while(|&x| x.is_ascii_digit());
          format!("{}{}{}.{}e{}",sign,ch,rest_digits,rest,exponent)
        } else {
          format!("{}{}{}.{}",sign,ch,rest_digits,rest)
        };
        let symbol = input.parse::<f64>().map(|f| Token { position: position.clone(), symbol: Symbol::FloatLit(f)});
        Some(symbol.map_err(|_| SourceError { position, content: LexerError::InvalidFloatLit(input) }))
      } else {
        rest_digits.insert(0,ch); 
        rest_digits.insert(0,sign); 
        let symbol = rest_digits.parse::<i64>().map(|f| Token { position: position.clone(), symbol: Symbol::IntLit(f)});
        Some(symbol.map_err(|_| SourceError { position, content: LexerError::InvalidIntLit(rest_digits) }))
      }
    } else { 
      match ch {
        '(' => Some(Ok(Token { position, symbol: Symbol::LParen })),
        ')' => Some(Ok(Token { position, symbol: Symbol::RParen })),
        _ => None,
      }
    } 
  }
}

