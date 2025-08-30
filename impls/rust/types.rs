use crate::env::*;
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, fmt::Display, ops::Deref, rc::Rc};

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
    Builtin(fn(Vec<MalType>) -> MalRet),
    MalFunc {
        ast: Rc<Self>,
        args: MalArgs,
        env: Rc<RefCell<MalEnv>>,
    },
    Atom(Rc<RefCell<Self>>),
}

unsafe impl Sync for MalType {}

impl PartialEq for MalType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::Str(a), Self::Str(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            // ----- list/vec -----
            (Self::List(a), Self::List(b))
            | (Self::Vec(a), Self::Vec(b))
            | (Self::List(a), Self::Vec(b))
            | (Self::Vec(a), Self::List(b)) => a == b,
            // ----- list/vec
            (Self::HashMap(a), Self::HashMap(b)) => a == b,
            (Self::Keyword(a), Self::Keyword(b)) => a == b,
            (Self::Builtin(a), Self::Builtin(b)) => std::ptr::fn_addr_eq(*a, *b),
            (Self::MalFunc { .. }, Self::MalFunc { .. }) => false,
            (Self::Atom(a), Self::Atom(b)) => a == b,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// ----- eval -----

#[allow(dead_code)]
impl MalType {
    pub fn eval(&self, env_original: &Rc<RefCell<MalEnv>>) -> MalRet {
        let mut ast = self;
        let mut env = env_original;

        let mut live_ast;
        let mut live_env;

        'tco: loop {
            if env.borrow().get("DEBUG-EVAL").is_some() {
                println!("EVAL: {ast}");
                println!("{}", env.borrow())
            }

            match ast {
                Self::List(l) => {
                    if l.is_empty() {
                        return Ok(Self::List(Vec::new()));
                    }

                    match l[0].eval(env)? {
                        Self::Symbol(sym) => {
                            let args: &[MalType] = &l[1..];

                            return match sym.as_str() {
                                "def!" => set_env_from_vec((&args[0], &args[1]), env),
                                "let*" => {
                                    if args.len() != 2 {
                                        return mal_err!(
                                            "let* failed: expected 2 elements, found {}",
                                            args.len() - 1
                                        );
                                    }

                                    match &args[0] {
                                        MalType::List(l1) | MalType::Vec(l1) => {
                                            live_env = Rc::new(RefCell::new(
                                                env.clone().borrow().new_into_outer(),
                                            ));
                                            env = &live_env;

                                            if l1.len() % 2 != 0 {
                                                return mal_err!(
                                                    "let* failed: expected symbol type pairs"
                                                );
                                            }

                                            for i in l1.iter().tuples::<(_, _)>() {
                                                set_env_from_vec(i, env).map_err(|e| {
                                                    MalErr::String(format!("let* failed: {e}"))
                                                })?;
                                            }

                                            ast = &args[1];
                                            continue 'tco;
                                        }
                                        _ => {
                                            mal_err!(
                                                "expected first argument to be list, found {} ",
                                                args[0]
                                            )
                                        }
                                    }
                                }
                                "do" => {
                                    if args.is_empty() {
                                        return mal_err!(
                                            "expected at least one argument, found {}",
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
                                "if" => {
                                    if args.len() < 2 || args.len() > 3 {
                                        return mal_err!(
                                            "expected three or four arguments, found {}",
                                            args.len()
                                        );
                                    }

                                    // If false
                                    if let MalType::Nil | MalType::Bool(false) =
                                        args[0].eval(env)?
                                    {
                                        if args.len() == 3 {
                                            ast = &args[2];
                                            continue 'tco;
                                        }

                                        return Ok(MalType::Nil);
                                    }

                                    ast = &args[1];
                                    continue 'tco;
                                }
                                "fn*" => {
                                    if args.len() != 2 {
                                        return mal_err!(
                                            "expected 2 arguments, found {}",
                                            args.len()
                                        );
                                    }

                                    let ast = Rc::new(args[1].clone());
                                    let args = match &args[0] {
                                        MalType::List(l) | MalType::Vec(l) => l,
                                        _ => return mal_err!("expected first argument to be a list or vec of args"),
                                    }
                                    .clone();

                                    Ok(MalType::MalFunc {
                                        ast,
                                        args,
                                        env: env.clone(),
                                    })
                                }
                                _ => match env.borrow().get(&sym) {
                                    Some(env_match) => {
                                        let args = args
                                            .iter()
                                            .map(|i| i.eval(env))
                                            .collect::<Result<Vec<MalType>, MalErr>>()?;

                                        match env_match {
                                            MalType::Builtin(f) => f(args),
                                            _ => {
                                                live_ast = env_match.clone();
                                                ast = &live_ast;
                                                continue 'tco;
                                            }
                                        }
                                    }
                                    None => mal_err!("'{sym}' not found"),
                                },
                            };
                        }
                        Self::MalFunc {
                            ast: fn_ast,
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
                                        "{fn_ast} expected at least {} arguments, found {}",
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

                                ar
                            } else {
                                l[1..]
                                    .iter()
                                    .map(|e| e.eval(env))
                                    .collect::<Result<Vec<MalType>, MalErr>>()?
                            };

                            let new_env =
                                fn_env.borrow_mut().bind_env(&args_mut, &evaluated_args)?;

                            live_env = Rc::new(RefCell::new(new_env));
                            env = &live_env;

                            live_ast = fn_ast.deref().clone();
                            ast = &live_ast;

                            continue 'tco;
                        }
                        _ => return mal_err!("expected symbol or func, found: {}", l[0]),
                    }
                }
                Self::Vec(v) => {
                    return Ok(Self::Vec(v.iter().map(|e| e.eval(env)).collect::<Result<
                        Vec<Self>,
                        MalErr,
                    >>(
                    )?));
                }
                Self::HashMap(h) => {
                    return Ok(Self::HashMap(
                        h.iter()
                            .map(|(k, v)| Ok((k.clone(), v.eval(env)?)))
                            .collect::<Result<HashMap<String, MalType>, MalErr>>()?,
                    ));
                }
                Self::Symbol(s) => {
                    return match env.clone().borrow().get(s.as_str()) {
                        Some(Self::Builtin(..)) => Ok(ast.clone()),
                        Some(e) => {
                            live_ast = e.clone();
                            ast = &live_ast;
                            continue 'tco;
                        }
                        None => Ok(ast.clone()),
                    };
                }
                _ => return Ok(ast.clone()),
            }
        }
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

// TODO write_with_alt!() macro
// ":#" for print_readability
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
            MalType::List(l) => {
                if f.alternate() {
                    write!(f, "({:#})", l.iter().map(|e| format!("{e:#}")).join(" "))
                } else {
                    write!(f, "({})", l.iter().join(" "))
                }
            }
            MalType::Vec(v) => {
                if f.alternate() {
                    write!(f, "[{:#}]", v.iter().map(|e| format!("{e:#}")).join(" "))
                } else {
                    write!(f, "[{}]", v.iter().join(" "))
                }
            }
            MalType::HashMap(h) => {
                let s = h.iter().map(|(key, val)| format!("{key} {val}")).join(" ");

                if f.alternate() {
                    write!(f, "{{{s:#}}}")
                } else {
                    write!(f, "{{{s}}}")
                }
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
