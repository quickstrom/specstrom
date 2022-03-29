use crate::error::{Error, SourceError};
use crate::files::*;
use crate::lexer::*;
use ordered_float::OrderedFloat;
use earlgrey_generic::{Grammar,GrammarBuilder,EarleyForest};
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    None
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Hole {
    Lower, 
    Same,
    Top
}


#[derive(Clone, Debug, PartialEq)]
pub enum MixfixPiece {
    Terminal(String),
    NonTerminal(Hole),
}

impl MixfixPiece {
    fn choose(&self,lower:&str,same:&str,top:&str) -> String {
        match self {
            Self::Terminal(str) => str.clone(),
            Self::NonTerminal(Hole::Lower) => String::from(lower),
            Self::NonTerminal(Hole::Same) => String::from(same),
            Self::NonTerminal(Hole::Top) => String::from(top),
        }
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum ParseTree<'a> {
  Ident(String,Position<'a>),
  StringLit(String,Position<'a>),
  CharLit(char,Position<'a>),
  IntLit(i64,Position<'a>),
  FloatLit(OrderedFloat<f64>,Position<'a>),
  SelectorLit(String,Position<'a>),
  App(Box<ParseTree<'a>>,Vec<ParseTree<'a>>),
  TempList(Vec<ParseTree<'a>>),
  Irrelevant,
}
impl <'a> ParseTree<'a> {
    fn get_position(&self) -> Position<'a> {
        match self {
            ParseTree::Ident(_,p) => *p,
            ParseTree::IntLit(_,p) => *p,
            ParseTree::FloatLit(_,p) => *p,
            ParseTree::StringLit(_,p) => *p,
            ParseTree::SelectorLit(_,p) => *p,
            ParseTree::CharLit(_,p) => *p,
            ParseTree::App(x,_) => x.get_position(),
            _ => Position::dummy()
        }
    }
}
pub fn basic_forest<'a,'b>() -> EarleyForest<'b,ParseTree<'a>,Token<'a>> {
    let mut forest = EarleyForest::new(
        |symbol,token : &Token<'a>| match (symbol,&token.symbol) {
            ("IntLit",Symbol::IntLit(i)) => ParseTree::IntLit(*i,token.position),
            ("StringLit",Symbol::StringLit(i)) => ParseTree::StringLit(i.clone(),token.position),
            ("CharLit",Symbol::CharLit(i)) => ParseTree::CharLit(*i,token.position),
            ("SelectorLit",Symbol::SelectorLit(i)) => ParseTree::SelectorLit(i.clone(),token.position),
            ("FloatLit",Symbol::FloatLit(i)) => ParseTree::FloatLit(*i,token.position),
            ("Ident",Symbol::Ident(i)) => ParseTree::Ident(i.clone(),token.position),
            _ => ParseTree::Irrelevant
        }
    );
    forest.action("Atom -> IntLit", |n| n[0].clone());
    forest.action("Atom -> StringLit", |n| n[0].clone());
    forest.action("Atom -> FloatLit", |n| n[0].clone());
    forest.action("Atom -> SelectorLit", |n| n[0].clone());
    forest.action("Atom -> CharLit", |n| n[0].clone());
    forest.action("Atom -> Ident", |n| n[0].clone());
    forest.action("Atom -> LParen Expr RParen", |n| n[1].clone());
    forest.action("ExprList -> ", |_| ParseTree::TempList(Vec::new()));
    forest.action("ExprList -> Expr", |n| ParseTree::TempList(n.clone()));
    forest.action("ExprList -> Expr Comma ExprList", |n| match n[2].clone() {
        ParseTree::TempList(vec) => {
            let mut ret = vec.clone();
            ret.insert(0,n[0].clone());
            ParseTree::TempList(ret)
        }
        _ => ParseTree::Irrelevant
    });
    forest.action("BaseExpr -> Atom", |n| n[0].clone());
    forest.action("BaseExpr -> BaseExpr LParen ExprList RParen", |n| match n[2].clone() {
        ParseTree::TempList(vec) => ParseTree::App(Box::new(n[0].clone()),vec),
        _ => ParseTree::Irrelevant
    });
    forest
}
pub fn basic_grammar<'a>() -> GrammarBuilder<Token<'a>> {
    let mut builder = GrammarBuilder::default();
    builder.quiet_nonterm("Expr");
    builder.quiet_terminal("IntLit", |tok : &Token| match tok.symbol { Symbol::IntLit(_) => true, _ => false} );
    builder.quiet_terminal("StringLit", |tok : &Token| match tok.symbol { Symbol::StringLit(_) => true, _ => false} );
    builder.quiet_terminal("FloatLit", |tok : &Token| match tok.symbol { Symbol::FloatLit(_) => true, _ => false} );
    builder.quiet_terminal("SelectorLit", |tok : &Token| match tok.symbol { Symbol::SelectorLit(_) => true, _ => false} );
    builder.quiet_terminal("CharLit", |tok : &Token| match tok.symbol { Symbol::CharLit(_) => true, _ => false} );
    builder.quiet_terminal("LParen", |tok : &Token| tok.symbol == Symbol::LParen );
    builder.quiet_terminal("RParen", |tok : &Token| tok.symbol == Symbol::RParen );
    builder.quiet_terminal("Comma", |tok : &Token| tok.symbol == Symbol::Ident(",".to_string()) );
    builder.quiet_terminal("Ident", |tok : &Token| match &tok.symbol { Symbol::Ident(s) => s != ",", _ => false} );
    builder.quiet_nonterm("Atom");
    builder.quiet_rule("Atom",&["Ident"]);
    builder.quiet_rule("Atom",&["IntLit"]);
    builder.quiet_rule("Atom",&["StringLit"]);
    builder.quiet_rule("Atom",&["FloatLit"]);
    builder.quiet_rule("Atom",&["SelectorLit"]);
    builder.quiet_rule("Atom",&["CharLit"]);
    builder.quiet_rule("Atom",&["LParen", "Expr", "RParen"]);
    builder.quiet_nonterm("ExprList");
    let empty : &[String] = &[];
    builder.quiet_rule("ExprList",empty);
    builder.quiet_rule("ExprList",&["Expr"]);
    builder.quiet_rule("ExprList",&["Expr","Comma","ExprList"]);
    builder.quiet_nonterm("BaseExpr");
    builder.quiet_rule("BaseExpr",&["BaseExpr","LParen","ExprList","RParen"]);
    builder.quiet_rule("BaseExpr",&["Atom"]);
    builder
}
pub fn generate_grammar<'a,'b>(table: Vec<Vec<Vec<MixfixPiece>>>) -> (Grammar<Token<'a>>,EarleyForest<'b,ParseTree<'a>,Token<'a>>)  {
    let mut builder = basic_grammar();
    let mut env = basic_forest();
    let mut name = String::from("BaseExpr");
    
    for level in table {
        let old_name = name;
        name = builder.unique_symbol_name();
        builder.quiet_nonterm(&name);
        builder.quiet_rule(&name,&[&old_name]);
        env.action(&(name.clone() + " -> " + &old_name),|n| n[0].clone() );
        for spec in level {
            let mut rule_name = name.clone();
            let mut app_root = "".to_string();
            let mut rule_spec : Vec<String> = Vec::new();
            let mut indices = vec![];
            let mut i : usize = 0;
            let mut position = None;
            rule_name.push_str(" ->");
            for item in spec {
                let it = item.choose(&old_name,&name,"Expr");
                rule_name.push_str(&" ");
                rule_name.push_str(&it);
                app_root.push_str(&item.choose("_","_","_"));
                rule_spec.push(it);
                if let MixfixPiece::Terminal(s) = item {
                    let st = s.clone();
                    if position.is_none() { 
                        position = Some(i)
                    }
                    builder.quiet_terminal(s, move |tok : &Token | tok.symbol == Symbol::Ident(st.clone()));
                } else {
                    indices.push(i);
                }
                i += 1;
            }
            builder.quiet_rule(&name,&rule_spec);
            env.action(&rule_name,move |n| {
                let args = indices.iter().map(|i| n[*i].clone()).collect();
                ParseTree::App(Box::new(ParseTree::Ident(app_root.clone(),n[position.unwrap()].get_position())),args)
            });

        }
    }
    builder.quiet_rule("Expr",&[&name]);
    env.action(&("Expr -> ".to_string() + &name),|n| n[0].clone() );
    (builder.into_grammar("Expr").unwrap(),env)
}