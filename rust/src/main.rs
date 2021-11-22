extern crate peeking_take_while;
extern crate serde_json;
mod error;
mod files;
mod lexer;
use std::path::Path;
fn main() {
  let mut files = files::Files::new();
  let path = Path::new("test.strom");
  files.load(path).unwrap();
  let lex = lexer::Lexer::source_file(&files.get(path).unwrap()); 
  for x in lex {
    if let Err(e) = x {
      e.display(&files);
    } else {
      println!("{:?}",&x); 
    }
  }
  println!("Hello, world!");
}
