use crate::files::{Files,Position};

pub trait Error {
  fn display(&self) -> String;
}

#[derive(Clone, Debug, Hash, Eq,PartialEq)]
pub struct SourceError<'a, E : Error> {
  pub position: Position<'a>,
  pub length: usize,
  pub content: E,
}
impl <'a, E : Error> SourceError<'a,E> {
  pub fn display(&self,files: &Files<'a>) {
    self.position.display_short();
    println!(": {}",self.content.display());
    self.position.display_context(files, self.length);
  }
}