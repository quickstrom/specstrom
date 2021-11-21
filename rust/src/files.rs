use std::path::Path;
use std::iter::Peekable;
use peeking_take_while::PeekableExt;
use peeking_take_while::PeekingTakeWhile;
use crate::error::Position;
use std::io::{BufRead, BufReader};
use std::str::Chars;
pub struct SourceFile<'a> {
  pub path : &'a Path,
  pub lines : Vec<String>
}

impl<'a> SourceFile<'a> {
  pub fn load(path: &'a Path) -> std::io::Result<SourceFile<'a>> {
    let lines: std::io::Result<Vec<String>> = BufReader::new(std::fs::File::open(path)?).lines().collect();
    Ok(SourceFile {path, lines: lines?})
  }
  pub fn chars<'b> (&'b self) -> SourceFileChars<'a,'b> {
    let pos = Position::in_file(self.path);
    let iter = self.lines[0].chars().peekable();
    SourceFileChars {
      file: self,
      position: pos,
      iterator: iter
    }
  }
}


pub struct SourceFileChars<'a,'b> {
  pub file: &'b SourceFile<'a>,
  pub position : Position<'a>,
  iterator: Peekable<Chars<'b>>
}

impl<'a,'b> Iterator for SourceFileChars<'a, 'b> where {

  type Item = char;
  
  fn next(&mut self) -> Option<char> {
    if let Some(ch) = self.iterator.next() {
      self.position = self.position.next_column();
      Some(ch)
    } else {
      self.position = self.position.next_line();
      if self.position.line < self.file.lines.len() {
        self.iterator = self.file.lines[self.position.line].chars().peekable();
        self.next()
      } else {
        None
      }
    }
  }

}
impl <'a, 'b> SourceFileChars<'a, 'b> where {
  pub fn peek(&mut self) -> Option<&char> {
    self.iterator.peek()
  }
  pub fn until_eol(&mut self) -> String { 
    let str : String = self.iterator.clone().collect();
    self.position = self.position.next_line();
    if self.position.line < self.file.lines.len() {
      self.iterator = self.file.lines[self.position.line].chars().peekable();
    } 
    str
  }
  pub fn next_while<P: FnMut(&char) -> bool>(&mut self, pred : P) -> String { 
    let str : String = self.iterator.by_ref().peeking_take_while(pred).collect();
    self.position = self.position.after_columns(str.len());
    str
  }
  
}
