use crate::env::*;
use colored::Colorize;
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, ops::Deref, rc::Rc};

pub type MalRet = Result<MalType, MalErr>;
pub type MalArgs = Vec<MalType>;

#[derive(Debug)]
#[allow(dead_code)]
pub enum MalErr {
    ErrStr(String),
    Throw(MalType),
}

impl Display for MalErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MalErr::ErrStr(s) => write!(f, "{s}"),
            MalErr::Throw(t) => write!(f, "ThrownException: {t:#}"),
        }
    }
}

#[macro_export]
macro_rules! mal_err {
    ($($arg:tt)*) => {
        Err(MalErr::ErrStr(format!($($arg)*)))
    };
}

#[macro_export]
macro_rules! mal_list {
    ($arg:expr) => {
        MalType::List($arg, None)
    };
}

#[macro_export]
macro_rules! mal_vec {
    ($arg:expr) => {
        MalType::Vec($arg, None)
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
    // second field is for meta
    List(Vec<MalType>, Option<Rc<MalType>>),
    Vec(Vec<MalType>, Option<Rc<MalType>>),
    HashMap(HashMap<String, MalType>, Option<Rc<MalType>>),
    Keyword(String),
    Builtin(fn(Vec<MalType>) -> MalRet, Option<Rc<MalType>>),
    MalFunc {
        ast: Rc<Self>,
        args: MalArgs,
        env: Rc<MalEnv>,
        is_macro: bool,
        meta: Option<Rc<MalType>>,
    },
    Atom(Rc<RefCell<Self>>),
}

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Str(a), Self::Str(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            // ----- list/vec -----
            (Self::List(a, ..), Self::List(b, ..))
            | (Self::Vec(a, ..), Self::Vec(b, ..))
            | (Self::List(a, ..), Self::Vec(b, ..))
            | (Self::Vec(a, ..), Self::List(b, ..)) => a == b,
            // ----- list/vec
            (Self::HashMap(a, ..), Self::HashMap(b, ..)) => a == b,
            (Self::Keyword(a), Self::Keyword(b)) => a == b,
            (Self::Builtin(a, ..), Self::Builtin(b, ..)) => std::ptr::fn_addr_eq(*a, *b),
            (Self::MalFunc { .. }, Self::MalFunc { .. }) => false,
            (Self::Atom(a), Self::Atom(b)) => a == b,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// ----- eval -----

#[allow(dead_code)]
impl MalType {
    pub fn apply(&self, args: &[MalType]) -> MalRet {
        match self {
            Self::Builtin(f, ..) => f(args.to_vec()),
            Self::MalFunc {
                ast,
                args: fn_args,
                env,
                ..
            } => {
                let new_env = env.clone().bind_env(fn_args, args)?;
                ast.eval(&Rc::new(new_env))
            }
            _ => mal_err!("attempted to apply non function"),
        }
    }

    pub fn eval(&self, env_original: &Rc<MalEnv>) -> MalRet {
        let mut ast = self;
        let mut env = env_original;

        let mut live_ast;
        let mut live_env;

        'tco: loop {
            if env.get("DEBUG-EVAL").is_some() {
                println!(
                    "{}: {} {}\t{}",
                    "EVAL".on_bright_black(),
                    ast.to_string().blue(),
                    "with env:".yellow(),
                    env.to_string().green()
                );
            }

            match ast {
                Self::List(list_inner, ..) => {
                    if list_inner.is_empty() {
                        return Ok(mal_list!(Vec::new()));
                    }

                    let operation = list_inner[0].clone();

                    let args: &[MalType] = if list_inner.len() > 1 {
                        &list_inner[1..]
                    } else {
                        &[]
                    };

                    return match operation {
                        Self::Symbol(sym) if sym == "def!" => {
                            set_env_from_vec((&args[0], &args[1]), env)
                        }
                        Self::Symbol(sym) if sym == "let*" => {
                            if args.len() != 2 {
                                return mal_err!(
                                    "{sym} err: expected 2 elements, found {}",
                                    args.len() - 1
                                );
                            }

                            match &args[0] {
                                MalType::List(l1, ..) | MalType::Vec(l1, ..) => {
                                    live_env = Rc::new(MalEnv::new(Some(env.clone())));
                                    env = &live_env;

                                    if l1.len() % 2 != 0 {
                                        return mal_err!("{sym} err: expected symbol type pairs");
                                    }

                                    for i in l1.iter().tuples::<(_, _)>() {
                                        set_env_from_vec(i, env)?;
                                    }

                                    ast = &args[1];
                                    continue 'tco;
                                }
                                _ => {
                                    mal_err!(
                                        "{sym} err: expected first argument to be list, found {} ",
                                        args[0]
                                    )
                                }
                            }
                        }
                        Self::Symbol(sym) if sym == "do" => {
                            if args.is_empty() {
                                return mal_err!(
                                    "{sym} err: expected at least one argument, found {}",
                                    args.len()
                                );
                            }

                            args[0..args.len() - 1]
                                .iter()
                                .map(|e| e.eval(env))
                                .collect::<Result<Vec<MalType>, MalErr>>()?;

                            ast = &args[args.len() - 1];
                            continue 'tco;
                        }
                        Self::Symbol(sym) if sym == "if" => {
                            if args.len() < 2 || args.len() > 3 {
                                return mal_err!(
                                    "{sym} err: expected three or four arguments, found {}",
                                    args.len()
                                );
                            }

                            // If false
                            if let MalType::Nil | MalType::Bool(false) = args[0].eval(env)? {
                                if args.len() == 3 {
                                    ast = &args[2];
                                    continue 'tco;
                                }

                                return Ok(MalType::Nil);
                            }

                            ast = &args[1];
                            continue 'tco;
                        }
                        Self::Symbol(sym) if sym == "fn*" => {
                            if args.len() != 2 {
                                return mal_err!(
                                    "{sym} err: expected 2 arguments, found {}",
                                    args.len()
                                );
                            }

                            let ast = Rc::new(args[1].clone());
                            let args = match &args[0] {
                                        MalType::List(l, ..) | MalType::Vec(l,..) => l,
                                        _ => return mal_err!("{sym} err: expected first argument to be a list or vec of args"),
                                    }
                                        .clone();

                            Ok(MalType::MalFunc {
                                ast,
                                args,
                                // TODO make env in MalFunc optional
                                env: env.clone(),
                                is_macro: false,
                                meta: None,
                            })
                        }
                        Self::Symbol(sym) if sym == "quote" => {
                            if args.len() != 1 {
                                return mal_err!(
                                    "{sym} err: expected 1 arguments, found {}",
                                    args.len()
                                );
                            }
                            return Ok(args[0].clone());
                        }
                        Self::Symbol(sym) if sym == "quasiquote" => {
                            if args.len() != 1 {
                                return mal_err!(
                                    "{sym} err: expected 1 argument, found {}",
                                    args.len()
                                );
                            }

                            quasiquote(&args[0])?.eval(env)
                        }
                        Self::Symbol(sym) if sym == "defmacro!" => {
                            return match &args[0] {
                                MalType::Symbol(key) => {
                                    let ev = args[1].eval(env)?;

                                    let val = match ev {
                                        MalType::MalFunc { ast, args, env, .. } => {
                                            MalType::MalFunc {
                                                ast,
                                                args,
                                                env,
                                                is_macro: true,
                                                meta: None,
                                            }
                                        }
                                        MalType::Builtin(..) => ev,
                                        _ => {
                                            return mal_err!(
                                                "{sym} err: expected func, found {}",
                                                ev
                                            );
                                        }
                                    };

                                    env.set(key.to_string(), val.clone());
                                    Ok(val)
                                }
                                _ => mal_err!("expected symbol, found {}", args[1]),
                            };
                        }
                        Self::Symbol(sym) if sym == "try*" => {
                            if args.len() == 1 {
                                ast = &args[0];

                                continue 'tco;
                            }

                            if args.len() != 2 {
                                return mal_err!(
                                    "{sym} err: expected 1 or 2 args, found {}",
                                    args.len()
                                );
                            }

                            if let Self::List(ref l0, ..) = args[1]
                                && l0.len() == 3
                                && let Self::Symbol(ref catch_sym) = l0[0]
                                && catch_sym == "catch*"
                            {
                                match args[0].eval(env) {
                                    ok @ Ok(_) => return ok,
                                    Err(err) => {
                                        let e = match err {
                                            MalErr::Throw(mal_type) => mal_type,
                                            MalErr::ErrStr(str) => MalType::Str(str),
                                        };

                                        let key = match l0[1] {
                                            Self::Symbol(ref key_sym) => key_sym,
                                            _ => {
                                                return mal_err!(
                                                    "{sym} err: expected 1st arg to be of type symbol"
                                                );
                                            }
                                        };

                                        let new_env = MalEnv::new(Some(env.clone()));
                                        new_env.set(key.to_string(), e);

                                        live_env = Rc::new(new_env);
                                        env = &live_env;

                                        ast = &l0[2];

                                        continue 'tco;
                                    }
                                }
                            }

                            mal_err!("{sym} err: expected catch block, found '{:#}'", args[0])
                        }
                        _ => {
                            let eval_op = operation.eval(env)?;
                            match eval_op {
                                Self::MalFunc {
                                    ast: ref fn_ast,
                                    args: ref fn_args,
                                    env: ref fn_env,
                                    is_macro: false,
                                    meta: None,
                                } => {
                                    let evaluated_args = args
                                        .iter()
                                        .map(|e| e.eval(env))
                                        .collect::<Result<Vec<MalType>, MalErr>>()?;

                                    let new_env = fn_env.bind_env(fn_args, &evaluated_args)?;

                                    live_env = Rc::new(new_env);
                                    env = &live_env;

                                    live_ast = fn_ast.deref().clone();
                                    ast = &live_ast;

                                    continue 'tco;
                                }
                                // Macro
                                Self::MalFunc {
                                    args: ref fn_args,
                                    // env: ref fn_env,
                                    is_macro: true,
                                    ..
                                } => {
                                    let new_env = env.bind_env(fn_args, args)?;

                                    live_env = Rc::new(new_env);
                                    env = &live_env;

                                    live_ast = eval_op.apply(args)?;
                                    ast = &live_ast;

                                    continue 'tco;
                                }
                                Self::Builtin(f, ..) => {
                                    let args = list_inner[1..]
                                        .iter()
                                        .map(|i| i.eval(env))
                                        .collect::<Result<Vec<MalType>, MalErr>>()?;

                                    return f(args);
                                }
                                _ => {
                                    return mal_err!(
                                        "expected symbol or func, found: {}",
                                        list_inner[0]
                                    );
                                }
                            }
                        }
                    };
                }
                Self::Vec(v, ..) => {
                    return Ok(mal_vec!(v.iter().map(|e| e.eval(env)).collect::<Result<
                        Vec<Self>,
                        MalErr,
                    >>(
                    )?));
                }
                Self::HashMap(h, ..) => {
                    return Ok(Self::HashMap(
                        h.iter()
                            .map(|(k, v)| Ok((k.clone(), v.eval(env)?)))
                            .collect::<Result<HashMap<String, MalType>, MalErr>>()?,
                        None,
                    ));
                }
                Self::Symbol(s) => {
                    return match env.clone().get(s.as_str()) {
                        Some(e) => Ok(e),
                        None => mal_err!("'{ast}' not found"),
                    };
                }
                _ => return Ok(ast.clone()),
            }
        }
    }
}

fn splice_unquote(l: &[MalType]) -> MalRet {
    let mut result = mal_list!(Vec::new());

    for elt in l.iter().rev() {
        if let MalType::List(elt_list, ..) = elt
            && !elt_list.is_empty()
            && let MalType::Symbol(x) = &elt_list[0]
            && x == "splice-unquote"
        {
            if elt_list.len() != 2 {
                return mal_err!(
                    "splice-unquote err: expected 1 arg, found {}",
                    elt_list.len() - 1
                );
            }

            result = mal_list!(vec![
                MalType::Symbol("concat".to_string()),
                elt_list[1].clone(),
                result,
            ])
        } else {
            result = mal_list!(vec![
                MalType::Symbol("cons".to_string()),
                quasiquote(elt)?,
                result,
            ])
        }
    }

    Ok(result)
}

fn quasiquote(ast: &MalType) -> MalRet {
    match ast {
        MalType::List(l, ..) => {
            if !l.is_empty()
                && let MalType::Symbol(sym) = &l[0]
                && sym == "unquote"
            {
                if l.len() != 2 {
                    return mal_err!("unquote err: expected 1 argument, found {}", l.len() - 1);
                }

                Ok(l[1].clone())
            } else {
                splice_unquote(l)
            }
        }

        MalType::Vec(v, ..) => Ok(mal_list!(vec![
            MalType::Symbol("vec".to_string()),
            splice_unquote(v)?,
        ])),
        MalType::Symbol(..)
        | MalType::HashMap(..)
        | MalType::Builtin(..)
        | MalType::MalFunc { .. } => Ok(mal_list!(vec![
            MalType::Symbol("quote".to_string()),
            ast.clone(),
        ])),
        _ => Ok(ast.clone()),
    }
}

fn set_env_from_vec(l: (&MalType, &MalType), env: &Rc<MalEnv>) -> MalRet {
    match l.0 {
        MalType::Symbol(key) => {
            let val = l.1.eval(env)?;
            env.set(key.to_string(), val.clone());
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
            MalType::Str(s) => {
                // print readability
                if f.alternate() {
                    let x = s
                        .chars()
                        .map(|c| match c {
                            '"' => "\\\"".to_string(),
                            '\n' => "\\n".to_string(),
                            '\\' => "\\\\".to_string(),
                            _ => c.to_string(),
                        })
                        .collect::<Vec<String>>()
                        .join("");

                    write!(f, "\"{x}\"")
                } else {
                    write!(f, "{s}")
                }
            }
            MalType::Bool(b) => write!(f, "{b}"),
            MalType::List(l, ..) => {
                if f.alternate() {
                    write!(f, "({:#})", l.iter().map(|e| format!("{e:#}")).join(" "))
                } else {
                    write!(f, "({})", l.iter().join(" "))
                }
            }
            MalType::Vec(v, ..) => {
                if f.alternate() {
                    write!(f, "[{:#}]", v.iter().map(|e| format!("{e:#}")).join(" "))
                } else {
                    write!(f, "[{}]", v.iter().join(" "))
                }
            }
            MalType::HashMap(h, ..) => {
                let s = h
                    .iter()
                    .map(|(key, val)| {
                        if f.alternate() {
                            format!("{:#} {val:#}", from_hashmap_key(key))
                        } else {
                            format!("{:#} {val}", from_hashmap_key(key))
                        }
                    })
                    .join(" ");

                write!(f, "{{{s:#}}}")
            }
            MalType::Keyword(k) => write!(f, ":{k}"),
            MalType::Builtin(..) => write!(f, "builtin"),
            MalType::MalFunc { args, ast, .. } => {
                if f.alternate() {
                    write!(
                        f,
                        "#<function>({}) {ast}",
                        args.iter().map(|e| format!("{e:#}")).join(" ")
                    )
                } else {
                    write!(f, "#<function>({}) {ast}", args.iter().join(" "))
                }
            }
            MalType::Atom(a) => {
                let inner = a.borrow();

                if f.alternate() {
                    write!(f, "(atom {inner:#})")
                } else {
                    write!(f, "(atom {inner})")
                }
            }
        }
    }
}

pub fn to_hashmap_key(from: &MalType) -> Result<String, MalErr> {
    match from {
        MalType::Keyword(k) => Ok(format!("\u{29e}{k}")),
        MalType::Str(str) => Ok(str.to_string()),
        _ => {
            mal_err!("failed to construct hashmap key: expected keyword or string, found {from:#}")
        }
    }
}

pub fn from_hashmap_key(key: &str) -> MalType {
    match key.chars().next() {
        // the unicode symbol is 2 bytes long, hence the [2..]
        Some('\u{29e}') => MalType::Keyword(key[2..].to_string()),
        _ => MalType::Str(key.to_string()),
    }
}

pub fn make_hashmap(seq: Vec<MalType>) -> Result<MalType, MalErr> {
    if seq.len() % 2 != 0 {
        return mal_err!("missing hashmap value or key");
    }

    let h = seq
        .iter()
        .tuples()
        .map(|(key, val)| Ok((to_hashmap_key(key)?, val.clone())))
        .collect::<Result<HashMap<_, _>, MalErr>>()?;

    Ok(MalType::HashMap(h, None))
}
