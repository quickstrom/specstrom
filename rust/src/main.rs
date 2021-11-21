extern crate peeking_take_while;
mod error;
mod files;
mod lexer;
use std::path::Path;
fn main() {
  let file = files::SourceFile::load(Path::new("test.strom")).unwrap(); 
  let lex = lexer::Lexer::source_file(&file); 
  for x in lex {
    println!("{:?}",&x); 
  }
  println!("Hello, world!");
}
