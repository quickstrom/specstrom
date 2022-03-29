extern crate peeking_take_while;
extern crate serde_json;
extern crate earlgrey_generic;
extern crate ordered_float;
mod error;
mod files;
mod lexer;
mod parser;
use parser::*;
use std::path::Path;
fn main() {
  let mut files = files::Files::new();
  let path = Path::new("test.strom");
  files.load(path).unwrap();
  let lex = lexer::Lexer::source_file(&files.get(path).unwrap());
  let lex2 = lex.map(|t| match t {
    Err(e) => { e.display(&files); panic!("Lexer error") }
    Ok(t) => t
  });
  let (g,f) = parser::generate_grammar(vec![vec![vec![MixfixPiece::NonTerminal(Hole::Same), MixfixPiece::Terminal("+".to_string()),MixfixPiece::NonTerminal(Hole::Lower)]]]);
  let trees = earlgrey_generic::EarleyParser::new(g).parse(lex2).unwrap();
  println!("{:?}", f.eval(&trees).unwrap());
}
