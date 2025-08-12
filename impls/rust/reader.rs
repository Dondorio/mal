use lazy_static::lazy_static;
use regex::Regex;

use crate::{mal_err, types::*};

pub struct Reader {
    tokens: Vec<String>,
    index: usize,
}

pub fn read_str(input: &str) -> Result<MalType, MalErr> {
    let tokens = tokenize(input);
    Reader::new(tokens).read_form()
}

fn tokenize(input: &str) -> Vec<String> {
    let re =
        Regex::new(r##"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"##)
            .unwrap();
    re.captures_iter(input)
        .map(|s| (&s)[1].to_string())
        .filter(|s| !s.starts_with(';'))
        .collect()
}

#[allow(dead_code)]
impl Reader {
    fn new(tokens: Vec<String>) -> Reader {
        Reader { index: 0, tokens }
    }

    fn next(&mut self) -> Option<&str> {
        if self.index >= self.tokens.len() {
            return None;
        }

        let out = &self.tokens[self.index];
        self.index += 1;
        Some(out)
    }

    fn peek(&mut self) -> Option<&str> {
        if self.index >= self.tokens.len() {
            return None;
        }

        self.tokens.get(self.index).map(|t| t.as_str())
    }

    fn read_form(&mut self) -> MalRet {
        let token = match self.peek() {
            Some(t) => t,
            None => return mal_err!("expected token, found EOF"),
        };

        fn symbol(r: &mut Reader, n: &str) -> Result<MalType, MalErr> {
            Ok(MalType::List(vec![
                MalType::Symbol(n.to_string()),
                r.read_form()?,
            ]))
        }

        match token {
            "'" => {
                self.next();
                symbol(self, "quote")
            }
            "`" => {
                self.next();
                symbol(self, "quasiquote")
            }
            "~" => {
                self.next();
                symbol(self, "unquote")
            }
            "~@" => {
                self.next();
                symbol(self, "splice-unquote")
            }
            "^" => {
                self.next();
                let meta = self.read_form()?;
                Ok(MalType::List(vec![
                    MalType::Symbol("with-meta".to_string()),
                    self.read_form()?,
                    meta,
                ]))
            }
            "@" => {
                self.next();
                symbol(self, "deref")
            }
            "(" => self.read_seq(")"),
            "[" => self.read_seq("]"),
            "{" => self.read_seq("}"),
            _ => self.read_atom(),
        }
    }

    fn read_seq(&mut self, closing: &str) -> MalRet {
        let mut seq = Vec::new();
        self.next();
        loop {
            match self.peek() {
                Some(t) => {
                    if t == closing {
                        self.next();
                        break;
                    }
                }
                None => {
                    return mal_err!("expected {closing} but found EOF");
                }
            };

            seq.push(self.read_form()?);
        }
        match closing {
            ")" => Ok(MalType::List(seq)),
            "]" => Ok(MalType::Vec(seq)),
            "}" => make_hashmap(seq),
            _ => mal_err!("unknown symbol: {closing}"),
        }
    }

    // return mal type based off regex probably
    fn read_atom(&mut self) -> MalRet {
        lazy_static! {
            static ref INT_RE: Regex = Regex::new(r"^-?[0-9]+$").unwrap();
            static ref STR_RE: Regex = Regex::new(r#""(?:\\.|[^\\"])*""#).unwrap();
        }

        let token = self.next().unwrap();

        if INT_RE.is_match(token) {
            return Ok(MalType::Int(token.parse().unwrap()));
        } else if STR_RE.is_match(token) {
            // TODO trim " only once
            return Ok(MalType::Str(token.trim_matches('"').to_string()));
        } else if token.starts_with("\"") {
            return mal_err!("unbalanced \"");
        } else if let Some(k) = token.strip_prefix(":") {
            return Ok(MalType::Keyword(k.to_string()));
        }

        Ok(match token {
            "true" => MalType::Bool(true),
            "false" => MalType::Bool(false),

            _ => MalType::Symbol(token.to_string()),
        })
    }
}
