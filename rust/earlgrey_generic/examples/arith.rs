use std::cell::RefCell;

fn build_grammar() -> earlgrey::Grammar<(String,usize)> {
    let foo : earlgrey::GrammarBuilder<(String,usize)> = earlgrey::GrammarBuilder::default();

    foo.nonterm("expr")
      .nonterm("term")
      .nonterm("factor")
      .nonterm("power")
      .nonterm("ufact")
      .nonterm("group")
      .nonterm("func")
      .nonterm("args")
      .terminal("[n]", |(n,_)| n.parse::<f32>().is_ok())
      .terminal("+", |(n,_)| n == "+")
      .terminal("-", |(n,_)| n == "-")
      .terminal("*", |(n,_)| n == "*")
      .terminal("/", |(n,_)| n == "/")
      .terminal("%", |(n,_)| n == "%")
      .terminal("^", |(n,_)| n == "^")
      .terminal("!", |(n,_)| n == "!")
      .terminal("(", |(n,_)| n == "(")
      .terminal(")", |(n,_)| n == ")")
      .rule("expr",   &["term"])
      .rule("expr",   &["expr", "+", "term"])
      .rule("expr",   &["expr", "-", "term"])
      .rule("term",   &["factor"])
      .rule("term",   &["term", "*", "factor"])
      .rule("term",   &["term", "/", "factor"])
      .rule("term",   &["term", "%", "factor"])
      .rule("factor", &["power"])
      .rule("factor", &["-", "factor"])
      .rule("power",  &["ufact"])
      .rule("power",  &["ufact", "^", "factor"])
      .rule("ufact",  &["group"])
      .rule("ufact",  &["ufact", "!"])
      .rule("group",  &["[n]"])
      .rule("group",  &["(", "expr", ")"])
      .into_grammar("expr")
      .expect("Bad Gramar")
}

struct Tokenizer<I: Iterator<Item=char>>(lexers::Scanner<I>);

impl<I: Iterator<Item=char>> Iterator for Tokenizer<I> {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.scan_whitespace();
        self.0.scan_math_op()
            .or_else(|| self.0.scan_number())
            .or_else(|| self.0.scan_identifier())
    }
}

fn tokenizer<I: Iterator<Item=char>>(input: I) -> Tokenizer<I> {
    Tokenizer(lexers::Scanner::new(input))
}

fn gamma(x: f64) -> f64 {
    #[link(name="m")]
    extern { fn tgamma(x: f64) -> f64; }
    unsafe { tgamma(x) }
}

fn semanter<'a>() -> earlgrey::EarleyForest<'a, f64,(String,usize)> {
    use std::str::FromStr;
    let mut ev : earlgrey::EarleyForest<'a,f64,(String,usize)> = earlgrey::EarleyForest::new(|symbol, token : &(String,usize)| {
        match symbol {"[n]" => token.0.parse().unwrap(), _ => 0.0}
    });
    ev.action("expr -> term", |n| n[0]);
    ev.action("expr -> expr + term", |n| n[0] + n[2]);
    ev.action("expr -> expr - term", |n| n[0] - n[2]);
    ev.action("term -> factor", |n| n[0]);
    ev.action("term -> term * factor", |n| n[0] * n[2]);
    ev.action("term -> term / factor", |n| n[0] / n[2]);
    ev.action("term -> term % factor", |n| n[0] % n[2]);
    ev.action("factor -> power", |n| n[0]);
    ev.action("factor -> - factor", |n| -n[1]);
    ev.action("power -> ufact", |n| n[0]);
    ev.action("power -> ufact ^ factor", |n| n[0].powf(n[2]));
    ev.action("ufact -> group", |n| n[0]);
    ev.action("ufact -> ufact !", |n| gamma(n[0]+1.0));
    ev.action("group -> [n]", |n| n[0]);
    ev.action("group -> ( expr )", |n| n[1]);
    ev
}

fn main() {
    let rl = RefCell::new(rustyline::Editor::<()>::new());
    let input: Box<dyn Iterator<Item=_>> = if std::env::args().len() > 1 {
        Box::new((0..1).map(|_| std::env::args()
                            .skip(1).collect::<Vec<String>>().join(" ")))
    } else {
        Box::new((0..).map(|_| rl.borrow_mut().readline("~> "))
                 .take_while(|i| i.is_ok()).map(|i| i.unwrap()))
    };
    let parser = earlgrey::EarleyParser::new(build_grammar());
    let evaler = semanter();
    for expr in input {
        match parser.parse(&mut tokenizer(expr.chars()).enumerate().map(|(a,b)| (b,a))) {
            Err(e) => println!("Parse err: {:?}", e),
            Ok(state) => {
                rl.borrow_mut().add_history_entry(&expr);
                let val = evaler.eval(&state);
                println!("{:?}", val);
            }
        }
    }
}
