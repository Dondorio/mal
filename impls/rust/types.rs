use crate::env::*;
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

#[macro_export]
macro_rules! mal_err {
    ($($arg:tt)*) => {
        Err(MalErr::String(format!($($arg)*)))
    };
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum MalType {
    Nil,
    Int(i64),
    Symbol(String),
    Str(String),
    Bool(bool),
    List(Vec<MalType>),
    Vec(Vec<MalType>),
    HashMap(HashMap<String, MalType>),
    Keyword(String),
    Func(fn(Vec<MalType>) -> MalRet),
}

#[allow(dead_code)]
impl MalType {
    pub fn eval(&self, env: &Rc<RefCell<MalEnv>>) -> MalRet {
        if env.borrow().get("DEBUG-EVAL").is_some() {
            println!("EVAL: {self}");
            println!("{}", env.borrow())
        }

        match self {
            Self::List(l) => {
                if l.is_empty() {
                    return Ok(Self::List(Vec::new()));
                }

                let set_env_from_vec =
                    |l: (&MalType, &MalType), env: &Rc<RefCell<MalEnv>>| -> MalRet {
                        match l.0 {
                            Self::Symbol(key) => {
                                let val = l.1.eval(env)?;
                                env.borrow_mut().set(key.to_string(), val.clone());
                                Ok(val)
                            }
                            _ => mal_err!("expected symbol, found {}", l.0),
                        }
                    };

                match &l[0] {
                    Self::Symbol(sym) => {
                        match sym.as_str() {
                            "def!" => return set_env_from_vec((&l[1], &l[2]), env),
                            "let*" => {
                                if l.len() != 3 {
                                    return mal_err!(
                                        "let* failed: expected 2 elements, found no1 {}",
                                        l.len() - 1
                                    );
                                }

                                return match &l[1] {
                                    Self::List(l1) | Self::Vec(l1) => {
                                        let new_env =
                                            &Rc::new(RefCell::new(env.borrow().new_into_outer()));

                                        if l1.len() % 2 != 0 {
                                            return mal_err!(
                                                "let* failed: expected symbol type pairs"
                                            );
                                        }

                                        for i in l1.iter().tuples::<(_, _)>() {
                                            set_env_from_vec(i, new_env).map_err(|e| {
                                                MalErr::String(format!("let* failed: {e}"))
                                            })?;
                                        }

                                        l[2].eval(new_env)
                                    }
                                    _ => {
                                        mal_err!(
                                            "expected first argument to be list, found {} ",
                                            l[1]
                                        )
                                    }
                                };
                            }
                            _ => {}
                        }
                        match env.borrow().get(sym) {
                            Some(env_match) => {
                                let args = l[1..].iter().map(|i| i.eval(env)).collect::<Result<
                                    Vec<MalType>,
                                    MalErr,
                                >>(
                                )?;

                                match env_match {
                                    Self::Func(f) => f(args),
                                    _ => env_match.eval(env),
                                }
                            }
                            None => mal_err!("'{sym}' not found"),
                        }
                    }
                    _ => mal_err!("expected symbol, found: {}", l[0]),
                }
            }
            Self::Vec(v) => {
                Ok(Self::Vec(v.iter().map(|e| e.eval(env)).collect::<Result<
                    Vec<Self>,
                    MalErr,
                >>(
                )?))
            }
            Self::HashMap(h) => Ok(Self::HashMap(
                h.iter()
                    .map(|(k, v)| Ok((k.clone(), v.eval(env)?)))
                    .collect::<Result<HashMap<String, MalType>, MalErr>>()?,
            )),
            Self::Symbol(s) => match env.borrow().get(s.as_str()) {
                Some(Self::Func(..)) => Ok(self.clone()),
                Some(e) => Ok(e.clone()),
                None => Ok(self.clone()),
            },
            _ => Ok(self.clone()),
        }
    }
}

impl Display for MalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::Nil => write!(f, "Nil"),
            MalType::Int(i) => write!(f, "{i}",),
            MalType::Symbol(s) => write!(f, "{s}"),
            MalType::Str(s) => {
                if let Some(p) = s.strip_prefix('\u{29e}') {
                    write!(f, ":{p}")
                } else {
                    write!(f, "\"{s}\"")
                }
            }
            MalType::Bool(b) => write!(f, "{b}"),
            MalType::List(l) => {
                write!(f, "({})", join_list(l, " "))
            }
            MalType::Vec(v) => {
                write!(f, "[{}]", join_list(v, " "))
            }
            MalType::HashMap(h) => {
                let s = h.iter().map(|(key, val)| format!("{key} {val}")).join(" ");
                write!(f, "{{{s}}}")
            }
            MalType::Keyword(k) => write!(f, ":{k}"),
            MalType::Func(..) => todo!(),
        }
    }
}

fn join_list(v: &[MalType], join: &str) -> String {
    v.iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(join)
}

pub fn make_hashmap(seq: Vec<MalType>) -> Result<MalType, MalErr> {
    if seq.len() % 2 != 0 {
        return mal_err!("missing hashmap value or key");
    }

    let h = seq
        .iter()
        .tuples()
        .map(|(key, val)| match key {
            MalType::Str(_) => Ok((key.to_string(), val.clone())),
            MalType::Keyword(_) => Ok((key.to_string(), val.clone())),
            _ => Err(MalErr::String(
                "hashmap key isn't a string or keyword".to_string(),
            )),
        })
        .collect::<Result<HashMap<_, _>, MalErr>>()?;

    Ok(MalType::HashMap(h))
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum MalErr {
    String(String),
    MalType(MalType),
}

impl Display for MalErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalErr::String(s) => write!(f, "{s}"),
            MalErr::MalType(t) => write!(f, "{t}"),
        }
    }
}

pub type MalRet = Result<MalType, MalErr>;
pub type MalArgs = Vec<MalType>;
