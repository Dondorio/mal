use crate::env::*;
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

#[macro_export]
macro_rules! mal_err {
    ($($arg:tt)*) => {
        Err(MalErr::String(format!($($arg)*)))
    };
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
    Builtin(fn(Vec<MalType>) -> MalRet),
    MalFunc {
        // needed for custom eval functions for things like macros
        eval: fn(&MalType, &Rc<RefCell<MalEnv>>) -> MalRet,
        ast: Rc<Self>,
        args: MalArgs,
        env: Rc<RefCell<MalEnv>>,
    },
}

// ----- eval -----

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

                match &l[0].eval(env)? {
                    Self::Symbol(sym) => eval_list_symbol(sym, &l[1..], env),
                    // it's a list for some reason
                    // so it isn't a malfunc
                    // with
                    Self::MalFunc {
                        eval,
                        ast,
                        args,
                        env: fn_env,
                    } => {
                        let mut args_mut = args.clone();

                        let evaluated_args = if args.len() >= 2
                            && let MalType::Symbol(sym) = &args[args.len() - 2]
                            && sym == "&"
                        {
                            let expected_len = args.len() - 1;

                            if l.len() < expected_len {
                                return mal_err!(
                                    "{ast} expected at least {} arguments, found {}",
                                    expected_len,
                                    l.len() - 1
                                );
                            }

                            let mut ar = l[1..expected_len]
                                .iter()
                                .clone()
                                .map(|e| e.eval(env))
                                .collect::<Result<Vec<MalType>, MalErr>>()?;

                            // Janky piece of shit code
                            // It's here so the hew & args return a list
                            // TODO move the condidionals for & syntax to bind_env
                            let mut p = vec![MalType::Symbol("list".to_string())];
                            p.extend_from_slice(&l[expected_len..]);

                            ar.push(MalType::List(p));

                            args_mut.remove(expected_len - 1);

                            println!("{ar:?}");
                            ar
                        } else {
                            l[1..]
                                .iter()
                                .map(|e| e.eval(env))
                                .collect::<Result<Vec<MalType>, MalErr>>()?
                        };

                        let mut new_env = fn_env.borrow().new_into_outer();
                        new_env.bind_env(&args_mut, &evaluated_args)?;

                        eval(ast, &Rc::new(RefCell::new(new_env)))
                    }
                    _ => mal_err!("expected symbol or func, found: {}", l[0]),
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
                Some(Self::Builtin(..)) => Ok(self.clone()),
                Some(e) => e.eval(env),
                None => Ok(self.clone()),
            },
            _ => Ok(self.clone()),
        }
    }
}

fn eval_list_symbol(sym: &str, l: &[MalType], env: &Rc<RefCell<MalEnv>>) -> MalRet {
    match sym {
        "def!" => set_env_from_vec((&l[0], &l[1]), env),
        "let*" => {
            if l.len() != 2 {
                return mal_err!("let* failed: expected 2 elements, found {}", l.len() - 1);
            }

            match &l[0] {
                MalType::List(l1) | MalType::Vec(l1) => {
                    let new_env = &Rc::new(RefCell::new(env.borrow().new_into_outer()));

                    if l1.len() % 2 != 0 {
                        return mal_err!("let* failed: expected symbol type pairs");
                    }

                    for i in l1.iter().tuples::<(_, _)>() {
                        set_env_from_vec(i, new_env)
                            .map_err(|e| MalErr::String(format!("let* failed: {e}")))?;
                    }

                    l[1].eval(new_env)
                }
                _ => {
                    mal_err!("expected first argument to be list, found {} ", l[0])
                }
            }
        }
        "do" => {
            if l.is_empty() {
                return mal_err!("expected at least one argument, found {}", l.len());
            }

            Ok(l[0..]
                .iter()
                .map(|e| e.eval(env))
                .collect::<Result<Vec<MalType>, MalErr>>()?[l.len() - 1]
                .clone())
        }
        "if" => {
            if l.len() < 2 || l.len() > 3 {
                return mal_err!("expected three or four arguments, found {}", l.len());
            }

            // If false
            if let MalType::Nil | MalType::Bool(false) = l[0].eval(env)? {
                if l.len() == 3 {
                    return l[2].eval(env);
                }

                return Ok(MalType::Nil);
            }

            l[1].eval(env)
        }
        "fn*" => {
            if l.len() != 2 {
                return mal_err!("expected 2 arguments, found {}", l.len());
            }

            let eval = MalType::eval;
            let ast = Rc::new(l[1].clone());
            let args = match &l[0] {
                MalType::List(l) | MalType::Vec(l) => l,
                _ => return mal_err!("expected first argument to be a list or vec of args"),
            }
            .clone();

            Ok(MalType::MalFunc {
                eval,
                ast,
                args,
                env: env.clone(),
            })
        }
        _ => match env.borrow().get(sym) {
            Some(env_match) => {
                let args = l
                    .iter()
                    .map(|i| i.eval(env))
                    .collect::<Result<Vec<MalType>, MalErr>>()?;

                match env_match {
                    MalType::Builtin(f) => f(args),
                    _ => env_match.eval(env),
                }
            }
            None => mal_err!("'{sym}' not found"),
        },
    }
}

fn set_env_from_vec(l: (&MalType, &MalType), env: &Rc<RefCell<MalEnv>>) -> Result<MalType, MalErr> {
    match l.0 {
        MalType::Symbol(key) => {
            let val = l.1.eval(env)?;
            env.borrow_mut().set(key.to_string(), val.clone());
            Ok(val)
        }
        _ => mal_err!("expected symbol, found {}", l.0),
    }
}

// ---- eval

impl Display for MalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::Nil => write!(f, "nil"),
            MalType::Int(i) => write!(f, "{i}",),
            MalType::Symbol(s) => write!(f, "{s}"),
            MalType::Str(s) => write!(f, "\"{s}\""),
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
            MalType::Builtin(..) => write!(f, "builtin"),
            MalType::MalFunc { args, ast, .. } => {
                write!(f, "#<function>({}) {ast}", join_list(args, " "))
            }
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
