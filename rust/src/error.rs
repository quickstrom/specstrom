use std::path::Path;
impl<'a> Position<'a> {
  pub fn new(file: &'a Path, line: usize, column: usize) -> Position<'a> {
    Position {file: Some(file), line, column}
  }
  pub fn in_file(file: &'a Path) -> Position<'a> {
    Position {file: Some(file), line: 0, column: 0}
  }
  pub fn dummy() -> Position<'a> {
    Position {file: None, line: 0, column: 0}
  }
  pub fn next_line(&self) -> Position<'a> {
    Position {file:self.file, line: self.line + 1, column: 1}
  }
  pub fn next_column(&self) -> Position<'a> {
    Position {file:self.file, line: self.line, column: self.column + 1}
  }
  pub fn after_columns(&self, i : usize) -> Position<'a> {
    Position {file:self.file, line: self.line, column: self.column + i}
  }
  pub fn advance_char(&self, c: char) -> Position<'a> {
    if c == '\n' { self.next_line() } else { self.next_column() }
  }
}
#[derive(Clone, Debug)]
pub struct Position<'a> {
  pub file: Option<&'a Path>,
  pub line: usize,
  pub column: usize,
}

#[derive(Clone, Debug)]
pub struct SourceError<'a, E> { 
  pub position: Position<'a>,
  pub content: E
}
