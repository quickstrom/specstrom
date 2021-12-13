use crate::error::{Error, SourceError};
use crate::files::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
  pub position: Position<'a>,
  pub symbol: Symbol,
}

#[derive(Clone, Debug, PartialEq)]
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
}

pub struct Lexer<'a, 'b> {
  iterator: SourceFileChars<'a, 'b>,
}

enum LexComment<'a> {
  Doc(Token<'a>),
  Comment,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexerError {
  InvalidIntLit(String),
  InvalidCharLit(String),
  InvalidStringLit(String),
  InvalidSelectorLit(String),
  InvalidFloatLit(String),
}

impl Error for LexerError {
  fn display(&self) -> String {
    match self {
      LexerError::InvalidCharLit(s) => format!("Invalid char literal {}", s),
      LexerError::InvalidIntLit(s) => format!("Invalid int literal {}", s),
      LexerError::InvalidStringLit(s) => format!("Invalid string literal {}", s),
      LexerError::InvalidSelectorLit(s) => format!("Invalid selector literal {}", s),
      LexerError::InvalidFloatLit(s) => format!("Invalid float literal {}", s),
    }
  }
}

type LexItem<'a> = Result<Token<'a>, SourceError<'a, LexerError>>;

impl<'a, 'b> Lexer<'a, 'b> {
  pub fn source_file(file: &'b SourceFile<'a>) -> Lexer<'a, 'b> {
    Lexer {
      iterator: file.chars(),
    }
  }
  fn is_single_ident(ch: char) -> bool {
    "[]{};,".contains(ch)
  }
  fn is_alpha_ident(ch: char) -> bool {
    ch.is_alphanumeric() || "_'!?@#$".contains(ch)
  }
  fn is_symbol_ident(ch: char) -> bool {
    !(ch.is_whitespace() || ch.is_alphanumeric() || "[]{};,".contains(ch) || "()\"\'`".contains(ch))
  }

  fn next_equals(&mut self, other: char) -> Option<bool> {
    return self.iterator.peek_same_line().map(|x| x == other);
  }

  fn lex_doc(&mut self, ch: char) -> Option<LexComment<'a>> {
    let position = self.iterator.position.previous_column();
    if ch == '/' {
      if self.next_equals('/')? {
        self.iterator.next();
        // we are in a comment, or a doc-string
        if self.next_equals('/')? {
          self.iterator.next();
          // it's a doc-string
          let doc = self.iterator.until_eol();
          let tok = Token {
            position,
            symbol: Symbol::Doc(doc),
          };
          return Some(LexComment::Doc(tok));
        } else {
          // it's a comment
          self.iterator.until_eol();
          Some(LexComment::Comment)
        }
      } else {
        None
      }
    } else {
      None
    }
  }

  fn lex_num_lit(&mut self, ch: char) -> Option<LexItem<'a>> {
    let mut first_digit = ch;
    let negation = if ch == '-' && self.iterator.peek().map_or(false, |x| x.is_ascii_digit()) {
      let pos = self.iterator.position.previous_column();
      first_digit = self.iterator.next()?;
      Some(pos)
    } else {
      None
    };
    if first_digit.is_ascii_digit() {
      let (position, sign) = match negation {
        Some(pos) => (pos, '-'),
        None => (self.iterator.position.previous_column(), '+'),
      };
      let rest_digits = self.iterator.next_while(|&x| x.is_ascii_digit());
      if self.iterator.peek_same_line() == Some('.') {
        self.iterator.next();
        let rest = self.iterator.next_while(|&x| x.is_ascii_digit());
        let input = if self.iterator.peek_same_line() == Some('e') || self.iterator.peek_same_line() == Some('E') {
          self.iterator.next();
          let exponent = self.iterator.next_while(|&x| x.is_ascii_digit());
          format!(
            "{}{}{}.{}e{}",
            sign, first_digit, rest_digits, rest, exponent
          )
        } else {
          format!("{}{}{}.{}", sign, first_digit, rest_digits, rest)
        };
        let symbol = input.parse::<f64>().map(|f| Token {
          position: position.clone(),
          symbol: Symbol::FloatLit(f),
        });
        let length = input.len();
        Some(symbol.map_err(|_| SourceError {
          position,
          content: LexerError::InvalidFloatLit(input),
          length,
        }))
      } else {
        let symbol = format!("{}{}{}", sign, first_digit, rest_digits)
          .parse::<i64>()
          .map(|f| Token {
            position: position.clone(),
            symbol: Symbol::IntLit(f),
          });
        Some(symbol.map_err(|_| SourceError {
          position,
          content: LexerError::InvalidIntLit(format!("{}{}", first_digit, rest_digits)),
          length: rest_digits.len() + 1,
        }))
      }
    } else {
      None
    }
  }

  fn lex_string_lit(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    if ch == '"' {
      let mut rest = self.iterator.next_while_escaped(|&x| x != '"');
      rest.insert(0, ch);
      let length = rest.len();
      if self.iterator.peek_same_line() != Some('"') {
        Some(Err(SourceError {
          position,
          content: LexerError::InvalidStringLit(rest),
          length,
        }))
      } else {
        self.iterator.next();
        rest.push('"');
        let length = rest.len();
        Some(
          serde_json::from_str(&rest)
            .map(|s| Token {
              position: position.clone(),
              symbol: Symbol::StringLit(s),
            })
            .map_err(|_| SourceError {
              position,
              content: LexerError::InvalidStringLit(rest),
              length,
            }),
        )
      }
    } else {
      None
    }
  }

  fn lex_char_lit(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    if ch == '\'' {
      let mut rest = self.iterator.next_while_escaped(|&x| x != '\'');
      if self.iterator.peek() != Some('\'') {
        rest.insert(0, ch);
        let length = rest.len();
        Some(Err(SourceError {
          position,
          content: LexerError::InvalidCharLit(rest),
          length,
        }))
      } else {
        self.iterator.next();
        if rest == "\"" {
          Some(Ok(Token {
            position,
            symbol: Symbol::CharLit('"'),
          }))
        } else if rest == "\\'" {
          Some(Ok(Token {
            position,
            symbol: Symbol::CharLit('\''),
          }))
        } else {
          let parse: Result<String, _> = serde_json::from_str(&format!("\"{}\"", &rest));
          Some(
            parse
              .ok()
              .and_then(|s| {
                let mut i = s.chars();
                let c = i.next();
                c.filter(|_| i.next().is_none())
              })
              .ok_or_else(|| SourceError {
                position: position.clone(),
                content: LexerError::InvalidCharLit(format!("'{}'", &rest)),
                length: rest.len() + 2,
              })
              .map(|c| Token {
                position: position,
                symbol: Symbol::CharLit(c),
              }),
          )
        }
      }
    } else {
      None
    }
  }

  fn lex_selector_lit(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    if ch == '`' {
      let mut rest = self.iterator.next_while_escaped(|&x| x != '`');
      if self.iterator.peek() != Some('`') {
        rest.insert(0, ch);
        let length = rest.len();
        Some(Err(SourceError {
          position,
          content: LexerError::InvalidSelectorLit(rest),
          length,
        }))
      } else {
        self.iterator.next();
        Some(Ok(Token {
          position,
          symbol: Symbol::SelectorLit(rest),
        }))
      }
    } else {
      None
    }
  }

  fn lex_projection(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    if ch == '.'
      && self
        .iterator
        .peek_same_line()
        .filter(|&c| c.is_alphanumeric())
        .is_some()
    {
      let rest = self.iterator.next_while(|&x| x.is_alphanumeric());
      Some(Ok(Token {
        position,
        symbol: Symbol::Projection(rest),
      }))
    } else {
      None
    }
  }

  fn lex_parens(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    match ch {
      '(' => Some(Ok(Token {
        position,
        symbol: Symbol::LParen,
      })),
      ')' => Some(Ok(Token {
        position,
        symbol: Symbol::RParen,
      })),
      _ => None,
    }
  }

  fn lex_single_ident(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    return if Self::is_single_ident(ch) {
      Some(Ok(Token {
        position,
        symbol: Symbol::Ident(format!("{}", ch)),
      }))
    } else {
      None
    };
  }

  fn lex_alpha_ident(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    return if Self::is_alpha_ident(ch) {
      let mut rest = self.iterator.next_while(|&x| Self::is_alpha_ident(x));
      rest.insert(0, ch);
      Some(Ok(Token {
        position,
        symbol: Symbol::Ident(rest),
      }))
    } else {
      None
    };
  }

  fn lex_symbol_ident(&mut self, ch: char) -> Option<LexItem<'a>> {
    let position = self.iterator.position.previous_column();
    return if Self::is_symbol_ident(ch) {
      let mut rest = self.iterator.next_while(|&x| Self::is_symbol_ident(x));
      rest.insert(0, ch);
      Some(Ok(Token {
        position,
        symbol: Symbol::Ident(rest),
      }))
    } else {
      None
    };
  }
}

impl<'a, 'b> Iterator for Lexer<'a, 'b> {
  type Item = LexItem<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    let mut ch;

    loop {
      ch = self.iterator.next()?;
      if ch.is_whitespace() {
        // Ignore whitespace.
        continue;
      } else {
        match self.lex_doc(ch) {
          Some(LexComment::Doc(t)) => return Some(Ok(t)),
          Some(LexComment::Comment) => continue,
          None => break,
        }
      }
    }

    self
      .lex_num_lit(ch)
      .or_else(|| self.lex_string_lit(ch))
      .or_else(|| self.lex_char_lit(ch))
      .or_else(|| self.lex_selector_lit(ch))
      .or_else(|| self.lex_projection(ch))
      .or_else(|| self.lex_parens(ch))
      .or_else(|| self.lex_single_ident(ch))
      .or_else(|| self.lex_alpha_ident(ch))
      .or_else(|| self.lex_symbol_ident(ch))
  }
}

#[cfg(test)]
mod tests {
  use crate::lexer::{Lexer, SourceFile, Symbol, Token};

  type TestToken = (usize, usize, Symbol);

  fn expect_lex(lines: Vec<&'static str>, expected: Vec<TestToken>) {
    let file = SourceFile::dummy_file(lines);
    let lexer = Lexer::source_file(&file);
    let actual: Vec<TestToken> = lexer
      .map(|r| {
        let token = r.unwrap();
        (token.position.line, token.position.column, token.symbol)
      })
      .collect();
    assert_eq!(actual, expected);
  }

  #[test]
  fn lex_char() {
    expect_lex(vec!["'a'"], vec![(0, 0, Symbol::CharLit('a'))])
  }

  #[test]
  fn lex_string() {
    expect_lex(
      vec!["\"test\""],
      vec![(0, 0, Symbol::StringLit(String::from("test")))],
    )
  }

  #[test]
  fn lex_selector() {
    expect_lex(
      vec!["`test`"],
      vec![(0, 0, Symbol::SelectorLit(String::from("test")))],
    )
  }

  #[test]
  fn lex_doc() {
    expect_lex(
      vec!["/// hello"],
      vec![(0, 0, Symbol::Doc(String::from(" hello")))],
    )
  }

  #[test]
  fn lex_idents_multiline() {
    expect_lex(
      vec!["foo", "bar"],
      vec![
        (0, 0, Symbol::Ident(String::from("foo"))),
        (1, 0, Symbol::Ident(String::from("bar"))),
      ],
    )
  }

  #[test]
  fn lex_projection() {
    expect_lex(
      vec!["foo.bar"],
      vec![
        (0, 0, Symbol::Ident(String::from("foo"))),
        (0, 3, Symbol::Projection(String::from("bar"))),
      ],
    )
  }

  #[test]
  fn lex_subtraction() {
    expect_lex(
      vec!["foo - bar"],
      vec![
        (0, 0, Symbol::Ident(String::from("foo"))),
        (0, 4, Symbol::Ident(String::from("-"))),
        (0, 6, Symbol::Ident(String::from("bar"))),
      ],
    )
  }

  #[test]
  fn lex_int_lit_valid() {
    expect_lex(vec!["123456"], vec![(0, 0, Symbol::IntLit(123456))])
  }

  #[test]
  fn lex_float_lit_valid() {
    expect_lex(vec!["123.456"], vec![(0, 0, Symbol::FloatLit(123.456))])
  }

  #[test]
  fn lex_int_neg_lit_valid() {
    expect_lex(vec!["-123456"], vec![(0, 0, Symbol::IntLit(-123456))])
  }

  #[test]
  fn lex_float_neg_lit_valid() {
    expect_lex(vec!["-123.456"], vec![(0, 0, Symbol::FloatLit(-123.456))])
  }

  #[test]
  fn lex_parens() {
    expect_lex(
      vec!["()"],
      vec![(0, 0, Symbol::LParen), (0, 1, Symbol::RParen)],
    )
  }

  #[test]
  fn lex_multiple_comments() {
    expect_lex(
      vec!["// foo", "// bar", "baz"],
      vec![(2, 0, Symbol::Ident(String::from("baz")))],
    )
  }

  use proptest::prelude::*;

  prop_compose! {
    fn valid_selector_strings()(i in "[[:ascii]]") -> String { i }
  }

  prop_compose! {
    fn valid_identifiers()(i in "[a-zA-Z][0-9a-zA-Z]{0,20}") -> String { i }
  }

  fn symbols() -> impl Strategy<Value = Symbol> {
    prop_oneof![
      Just(Symbol::LParen),
      Just(Symbol::RParen),
      valid_identifiers().prop_map(Symbol::Ident),
      valid_identifiers().prop_map(Symbol::Projection),
      any::<String>().prop_map(Symbol::StringLit),
      valid_selector_strings().prop_map(Symbol::SelectorLit),
      any::<char>().prop_map(Symbol::CharLit),
      any::<i64>().prop_map(Symbol::IntLit),
      any::<f64>().prop_map(Symbol::FloatLit),
    ]
  }
  fn lines() -> impl Strategy<Value = Vec<Symbol>> {
    prop_oneof![
      any::<String>().prop_map(|s| vec![Symbol::Doc(s)]),
      prop::collection::vec(symbols(), 1..10),
    ]
  }

  fn pp_symbol(symbol: Symbol) -> String {
    match symbol {
      Symbol::LParen => String::from("("),
      Symbol::RParen => String::from(")"),
      Symbol::Ident(n) => format!("{}", n),
      Symbol::Projection(n) => format!(".{}", n),
      Symbol::Doc(d) => format!("///{}", d),
      Symbol::StringLit(s) => serde_json::to_string(&s).unwrap(),
      Symbol::SelectorLit(s) => {
        let s = serde_json::to_string(&s.replace("`", "\\`")).unwrap();
        format!("`{}`", s[1..s.len() - 1].to_string())
      }
      Symbol::CharLit(c) => match c {
        '\'' => String::from("'\\''"),
        '"' => String::from("'\"'"),
        _ => {
          let s = serde_json::to_string(&c).unwrap();
          format!("'{}'", s[1..s.len() - 1].to_string())
        }
      },
      Symbol::IntLit(n) => format!("{} ", n),
      Symbol::FloatLit(n) => {
        // Enforce decimal part to get it parsed as float.
        let s = format!("{}", n);
        if s.contains(".") {
          return s;
        } else {
          return format!("{}.0", s);
        }
      }
    }
  }
  fn pp_line(line: Vec<Symbol>) -> String {
    line
      .into_iter()
      .map(pp_symbol)
      .collect::<Vec<String>>()
      .join(" ") // just to make sure lexemes aren't joined together (i.e. 1 and 0 shouldn't become 10)
  }

  proptest! {

      #[test]
      fn lexer_roundtrip(input in prop::collection::vec(lines(), 1..10)) {
        let lines: Vec<String> = input.clone().into_iter().map(pp_line).collect();
        let lines_as_strs: Vec<&str> = lines.iter().map(|s| &**s).collect();
        let file = SourceFile::dummy_file(lines_as_strs);
        let lexer = Lexer::source_file(&file);
        let actual: Vec<Symbol> = lexer
          .map(|r| {
            let token = r.unwrap();
            token.symbol
          })
          .collect();
      let expected: Vec<Symbol> = input.concat();
      assert_eq!(actual, expected);
    }
  }
}
