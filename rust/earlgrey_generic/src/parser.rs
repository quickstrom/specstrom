#![deny(warnings)]

use crate::grammar::{Rule, Grammar};
use crate::items::Item;
use std::collections::HashSet;
use std::rc::Rc;
use std::fmt::Debug;

pub struct EarleyParser<T> where T:PartialEq + std::hash::Hash + 'static {
    pub grammar: Grammar<T>,
}

#[derive(Debug)]
pub struct ParseTrees<T:PartialEq+std::hash::Hash+Clone>(pub Vec<Rc<Item<T>>>);

///////////////////////////////////////////////////////////////////////////////

impl <T:PartialEq+Debug+std::hash::Hash+Eq+Clone> EarleyParser<T> {
    pub fn new(grammar: Grammar<T>) -> EarleyParser<T> {
        EarleyParser{grammar}
    }

    /// Build new `Prediction` items from `next_terminal` of some Symbol:
    fn predictions<'r>(
        rules: impl Iterator<Item=&'r Rc<Rule<T>>> + 'r,
        next_terminal: &'r str,
        start_pos: usize,
    ) -> Box<dyn Iterator<Item=Item<T>> + 'r>
    {
        Box::new(rules.filter(move |rule| rule.head == next_terminal)
            .map(move |rule| Item::predict_new(&rule, start_pos)))
    }

    /// Build new `Completion` items based on `trigger` item having completed.
    /// When an item is completed it advances all items in the same starting
    /// StateSet whose next symbol matches its rule name.
    fn completions<'r>(
        starting_stateset: impl Iterator<Item=&'r Rc<Item<T>>> + 'r,
        trigger: &'r Rc<Item<T>>,
        complete_pos: usize,
    ) -> Box<dyn Iterator<Item=Item<T>> + 'r>
    {
        assert!(trigger.complete(), "Incomplete `trigger` used for completions");
        Box::new(starting_stateset.filter(move |item| {
            item.next_symbol().and_then(
                |s| s.nonterm()) == Some(trigger.rule.head.as_ref())
        }).map(move |item| Item::complete_new(item, trigger, complete_pos)))
    }

    /// Build new `Scan` items for items in the current stateset whose next
    /// symbol is a Terminal that matches the input lexeme ahead in the stream.
    fn scans<'r>(
        current_stateset: impl Iterator<Item=&'r Rc<Item<T>>> + 'r,
        lexeme: &'r T,
        end: usize,
    ) -> impl Iterator<Item=Rc<Item<T>>> + 'r
    {
        current_stateset.filter(move |item| {
            // check item's next symbol is a temrinal that scans lexeme
            let next_sym_term = item.next_symbol().and_then(|s| s.terminal());
            next_sym_term.map(|(_, matcher)| matcher(lexeme)) == Some(true)
        }).map(move |item| Rc::new(Item::scan_new(item, end, lexeme)))
    }

    pub fn parse<Tok>(&self, greedy : bool, mut tokenizer: Tok) -> Result<ParseTrees<T>, (String, Option<T>, Vec<String>)>
            where Tok: Iterator, Tok::Item: Debug + Into<T>  {

        // Populate S0, add items for each rule matching the start symbol
        let s0: HashSet<_> = self.grammar.rules.iter()
            .filter(|rule| rule.head == self.grammar.start)
            .map(|rule| Rc::new(Item::predict_new(&rule, 0)))
            .collect();

        let mut statesets = vec![s0];
        let mut token : Option<T> = None;
        // New statesets are generated from input stream (Scans)
        for idx in 0.. {
            // Predict/Complete until no new Items are added to the StateSet
            // Instead of looping we could pre-populate completions of nullable symbols
            loop {
                let new_items: Vec<_> = statesets[idx].iter().flat_map(|trigger| {
                    let next_sym = trigger.next_symbol();
                    if let Some(next_terminal) = next_sym.and_then(|s| s.nonterm()) {
                        EarleyParser::predictions(self.grammar.rules.iter(), next_terminal, idx)
                    } else if trigger.complete() {
                        assert!(next_sym.is_none(), "Expected next symbol to be None");
                        EarleyParser::completions(statesets[trigger.start].iter(), trigger, idx)
                    } else {
                        // Scan items populate next stateset only when done with current state
                        assert!(next_sym.and_then(|s| s.terminal()).is_some());
                        Box::new(std::iter::empty())
                    }
                }).collect();
                let stateset = statesets.get_mut(idx).unwrap();
                let prev_len = stateset.len();
                // Add new items to the current stateset merging existing ones
                for new_item in new_items {
                    if let Some(existent) = stateset.get(&new_item) {
                        existent.merge_sources(new_item);
                    } else {
                        stateset.insert(Rc::new(new_item));
                    }
                }
                // do precitions/completions until expansions are exhausted
                if prev_len == stateset.len() {
                    break;
                }
            }
            if !greedy && statesets[idx].iter().any(|item| item.complete() && item.start == 0 && item.rule.head == self.grammar.start) {
                break;
            } else if statesets[idx].is_empty() {
                break;
            }
            // Build Si+1 with items in the current state that accept the next token
            if let Some(lexeme) = tokenizer.next() {
                let t : T = lexeme.into();
                token = Some(t.clone());
                let new_ss : HashSet<_>  = EarleyParser::scans(statesets[idx].iter(), &t, idx + 1).collect();
                statesets.push(new_ss);
            } else {
                break;
            }
        }

        // debug StateSets
       // if cfg!(feature="debug") {
            for (idx, stateset) in statesets.iter().enumerate() {
                eprintln!("=== StateSet {} ===", idx);
                stateset.iter().inspect(|item| {
                    let src = item.sources().iter()
                        .map(|bp| format!("{:?}", bp))
                        .collect::<Vec<_>>().join(", ");
                    eprintln!("{:?} -- SRC: {}", item, src);
                }).count();
            }
       // }

        // Check that at least one item is a. complete, b. starts at the idx 0,
        // and c. the name of the rule matches the starting symbol.
        // It spans the whole input because we search at the last stateset
        let parse_trees: Vec<_> = statesets.pop()
            .expect("No Statesets (even s0)")
            .iter()
            .filter(|item| item.start == 0 && item.complete() &&
                           item.rule.head == self.grammar.start)
            .cloned()
            .collect();
        if parse_trees.is_empty() {
            let thing : Vec<String> = statesets.pop().map(|ss| {
                let mut possibilities =  ss.iter().cloned().collect::<Vec<_>>();
                possibilities.sort_by_key(|item| item.end - item.start);
                possibilities}).unwrap_or(Vec::new()).into_iter().flat_map(|item| item.next_symbol().and_then(|symb| Some(symb.name().to_string()))).collect();
            return Err(("Parse Error: No Rule completes".to_string(),token,thing));
        }
        Ok(ParseTrees(parse_trees))
    }
}
