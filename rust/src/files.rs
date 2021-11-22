use peeking_take_while::PeekableExt;
use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::iter::Peekable;
use std::path::Path;
use std::str::Chars;

#[derive(Clone, Debug, Copy, PartialEq)]
pub struct Position<'a> {
  pub file: Option<&'a Path>,
  pub line: usize,
  pub column: usize,
}

impl<'a> Position<'a> {
  pub fn display_short(&self) {
    if let Some(p) = self.file {
      print!("{}:{}:{}", p.display(), self.line + 1, self.column + 1)
    } else {
      print!(":{}:{}", self.line + 1, self.column + 1)
    }
  }
  pub fn display_context(&self, files: &Files<'a>, length: usize) {
    if let Some(sf) = self.file.and_then(|p| files.get(p)) {
      let lineno = format!("{}", self.line + 1);
      println!("{:1$} |", "", lineno.len());
      println!("{} | {}", lineno, sf.lines[self.line]);
      println!(
        "{:l$} | {:r$}{:^<u$}",
        "",
        "",
        "",
        r = self.column,
        l = lineno.len(),
        u = length
      );
    }
  }

  pub fn new(file: &'a Path, line: usize, column: usize) -> Position<'a> {
    Position {
      file: Some(file),
      line,
      column,
    }
  }
  pub fn in_file(file: &'a Path) -> Position<'a> {
    Self::new(file, 0, 0)
  }
  pub fn dummy() -> Position<'a> {
    Position {
      file: None,
      line: 0,
      column: 0,
    }
  }
  pub fn next_line(&self) -> Position<'a> {
    Position {
      file: self.file,
      line: self.line + 1,
      column: 0,
    }
  }
  pub fn next_column(&self) -> Position<'a> {
    self.after_columns(1)
  }
  pub fn previous_column(&self) -> Position<'a> {
    Position {
      file: self.file,
      line: self.line,
      column: self.column - 1,
    }
  }
  pub fn after_columns(&self, i: usize) -> Position<'a> {
    Position {
      file: self.file,
      line: self.line,
      column: self.column + i,
    }
  }
}
pub struct SourceFile<'a> {
  pub path: &'a Path,
  pub lines: Vec<String>,
}
pub struct Files<'a> {
  table: HashMap<&'a Path, SourceFile<'a>>,
}
impl<'a> Files<'a> {
  pub fn new() -> Files<'a> {
    Files {
      table: HashMap::new(),
    }
  }
  pub fn load(&mut self, path: &'a Path) -> std::io::Result<()> {
    let file = SourceFile::load(path)?;
    self.table.insert(path, file);
    Ok(())
  }
  pub fn get(&self, path: &'a Path) -> Option<&SourceFile<'a>> {
    self.table.get(path)
  }
}

impl<'a> SourceFile<'a> {
  pub fn load(path: &'a Path) -> std::io::Result<SourceFile<'a>> {
    let lines: std::io::Result<Vec<String>> =
      BufReader::new(std::fs::File::open(path)?).lines().collect();
    Ok(SourceFile {
      path,
      lines: lines?,
    })
  }
  pub fn dummy_file(lines: Vec<&'a str>) -> SourceFile<'a> {
    SourceFile {
      path: Path::new("dummy.strom"),
      lines: lines.into_iter().map(|x| String::from(x)).collect(),
    }
  }
  pub fn chars<'b>(&'b self) -> SourceFileChars<'a, 'b> {
    let pos = Position::in_file(self.path);
    let iter = self.lines[0].chars().peekable();
    SourceFileChars {
      file: self,
      position: pos,
      iterator: iter,
    }
  }
}

pub struct SourceFileChars<'a, 'b> {
  pub file: &'b SourceFile<'a>,
  pub position: Position<'a>,
  iterator: Peekable<Chars<'b>>,
}

impl<'a, 'b> Iterator for SourceFileChars<'a, 'b> {
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
impl<'a, 'b> SourceFileChars<'a, 'b> {
  pub fn peek(&mut self) -> Option<char> {
    match self.iterator.peek() {
      Some(ch) => Some(ch.clone()),
      None => {
        let mut position = self.position.next_line();
        while position.line < self.file.lines.len() {
          if let Some(ch) = self.file.lines[position.line].chars().next() {
            return Some(ch);
          } else {
            position = position.next_line()
          }
        }
        return None;
      }
    }
  }
  pub fn until_eol(&mut self) -> String {
    let str: String = self.iterator.clone().collect();
    self.position = self.position.next_line();
    if self.position.line < self.file.lines.len() {
      self.iterator = self.file.lines[self.position.line].chars().peekable();
    }
    str
  }
  pub fn next_while<P: FnMut(&char) -> bool>(&mut self, pred: P) -> String {
    let str: String = self.iterator.by_ref().peeking_take_while(pred).collect();
    self.position = self.position.after_columns(str.len());
    str
  }
  pub fn next_while_escaped<P: FnMut(&char) -> bool>(&mut self, mut pred: P) -> String {
    let mut str: String = self
      .iterator
      .by_ref()
      .peeking_take_while(|&x| pred(&x) && x != '\\')
      .collect();
    if self.iterator.peek() == Some(&'\\') {
      str.push('\\');
      self.iterator.next();
      if let Some(c) = self.iterator.next() {
        str.push(c);
        str += &self.next_while_escaped(pred);
      }
    }
    self.position = self.position.after_columns(str.len());
    str
  }
}

#[cfg(test)]
mod tests {
  use crate::files::SourceFile;
  use proptest::prelude::*;

  proptest! {

    // Checks that repeatedly calling `.next()` gives you back the full source file.
    // Ignores newline characters.
    #[test]
    fn returns_all_with_next(input in "(\\PC{0,8}\\n)*") {
      let file = SourceFile::dummy_file(input.split("\n").collect());
      let iterator = file.chars();
      let output: String = iterator.collect();
      assert_eq!(output, input.replace("\n", ""));
    }

    // Checks that calling `.peek()` at each position, stepping forward using `.next()`,
    // gives you back the full source file. Ignores newline characters.
    #[test]
    fn returns_all_with_peek(input in "(\\PC{0,8}\\n)*") {
      let file = SourceFile::dummy_file(input.split("\n").collect());
      let mut iterator = file.chars();
      let mut peeks: Vec<char> = vec![];
      loop {
        match iterator.peek() {
          Some(v) =>        {
            peeks.push(v);
            iterator.next();
          }
          None => break
        }
      }
      let output: String = peeks.into_iter().collect();
      assert_eq!(output, input.replace("\n", ""));
    }

    #[test]
    fn returns_all_correct_positions(input in "(\\PC{0,8}\\n)*") {
      let lines: Vec<&str> = input.split("\n").collect();
      let file = SourceFile::dummy_file(lines.clone());
      let mut iterator = file.chars();
      let mut positions: Vec<(usize, usize)> = vec![];
      loop {
        positions.push((iterator.position.line, iterator.position.column));
        match iterator.next() {
          Some(_) => continue,
          None => break,
        }
      }
      let mut found: Vec<char> = vec![];
      for (l, c) in positions {
        if !(l == 0 && c == 0) {
          let line = lines[l];
          let ch = line.chars().nth(c-1).unwrap();
          found.push(ch);
        }
      }
      let found_str: String = found.into_iter().collect();
      assert_eq!(found_str, input.replace("\n", ""));
    }
  }
}
