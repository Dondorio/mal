use std::{cell::RefCell, fs, rc::Rc};

use crate::{mal_err, reader::read_str, types::*};
use itertools::Itertools;

macro_rules! lisp_fn {
    ( | $($arg_name:ident: MalType::$type:ident), * | $body:expr ) => {
        MalType::Builtin(|args: $crate::types::MalArgs| {
            const EXPECTED_LEN: usize = [$(stringify!($arg_name)),*].len();

            if args.len() != EXPECTED_LEN {
                return $crate::mal_err!("expected {} arguments, found {}", EXPECTED_LEN, args.len());
            }

            let mut arg_iter = args.iter();

            $(
                let next = arg_iter.next().unwrap();
                let $arg_name = match next {
                     MalType::$type(i) => i,
                     _ => return $crate::mal_err!("expected {}, found {}", stringify!($type), next),
                };
            )*

            $body
        })
    };
}
macro_rules! lisp_fn_len {
    (| $ar:ident where len $type:tt $expected_len:literal| $body:expr ) => {
        MalType::Builtin(|args: $crate::types::MalArgs| {
            let type_str = stringify!($type);

            if !(args.len() $type $expected_len) {
                return $crate::mal_err!(
                    "expected arguments len {} {}, found len = {}",
                    type_str,
                    $expected_len,
                    args.len()
                );
            }

            let $ar = args;

            $body
        })
    };
}

pub fn ns() -> Vec<(&'static str, MalType)> {
    vec![
        // Arithmetic
        (
            "+",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Int(a + b))),
        ),
        (
            "-",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Int(a - b))),
        ),
        (
            "/",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Int(a / b))),
        ),
        (
            "*",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Int(a * b))),
        ),
        // Compare
        (
            "=",
            lisp_fn_len!(|args where len == 2| Ok(MalType::Bool(args[0] == args[1]))),
        ),
        (
            ">",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Bool(a > b))),
        ),
        (
            ">=",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Bool(a >= b))),
        ),
        (
            "<",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Bool(a < b))),
        ),
        (
            "<=",
            lisp_fn!(|a: MalType::Int, b: MalType::Int| Ok(MalType::Bool(a <= b))),
        ),
        // Bool
        (
            "list?",
            MalType::Builtin(|args| Ok(MalType::Bool(matches!(args[0], MalType::List(..))))),
        ),
        (
            "empty?",
            MalType::Builtin(|args| {
                Ok(MalType::Bool(
                    if let MalType::List(a) | MalType::Vec(a) = &args[0] {
                        a.is_empty()
                    } else {
                        return mal_err!("expected lisp or vec, found {}", args[0]);
                    },
                ))
            }),
        ),
        (
            "atom?",
            lisp_fn_len!(|args where len == 1| {
                Ok(MalType::Bool(
                    matches!(args[0], MalType::Atom(..))
                ))
            }),
        ),
        (
            "macro?",
            lisp_fn_len!(|args where len == 1| {
                Ok(MalType::Bool(
                    matches!(&args[0], MalType::MalFunc { is_macro: true, .. })
                ))
            }),
        ),
        (
            "nil?",
            lisp_fn_len!(|args where len == 1| {
                Ok(MalType::Bool(
                    matches!(&args[0], MalType::Nil)
                ))
            }),
        ),
        (
            "true?",
            lisp_fn_len!(|args where len == 1| {
                Ok(MalType::Bool(
                    matches!(&args[0], MalType::Bool(true))
                ))
            }),
        ),
        (
            "false?",
            lisp_fn_len!(|args where len == 1| {
                Ok(MalType::Bool(
                    matches!(&args[0], MalType::Bool(false))
                ))
            }),
        ),
        (
            "symbol?",
            lisp_fn_len!(|args where len == 1| {
                Ok(MalType::Bool(
                    matches!(&args[0], MalType::Symbol(_))
                ))
            }),
        ),
        // Io
        (
            "prn",
            MalType::Builtin(|args| {
                println!("{}", args.iter().map(|e| format!("{e:#}")).join(" "));
                Ok(MalType::Nil)
            }),
        ),
        (
            "println",
            MalType::Builtin(|args| {
                println!("{}", args.iter().join(" "));
                Ok(MalType::Nil)
            }),
        ),
        (
            "slurp",
            lisp_fn!(|s: MalType::Str| if let Ok(str) = fs::read_to_string(s) {
                Ok(MalType::Str(str))
            } else {
                mal_err!("failed to read file")
            }),
        ),
        // Declare
        ("list", MalType::Builtin(|args| Ok(MalType::List(args)))),
        (
            "str",
            MalType::Builtin(|args| Ok(MalType::Str(args.iter().join("")))),
        ),
        (
            "pr-str",
            MalType::Builtin(|args| {
                Ok(MalType::Str(
                    args.iter().map(|e| format!("{e:#}")).join(" "),
                ))
            }),
        ),
        (
            "atom",
            lisp_fn_len!(|a where len == 1| {
                Ok(MalType::Atom(Rc::new(RefCell::new(a[0].clone()))))
            }),
        ),
        (
            "vec",
            lisp_fn_len!(|args where len == 1| {
                if let MalType::List(v) | MalType::Vec(v) = &args[0]{
                    return Ok(MalType::Vec(v.clone()))
                }

                mal_err!("expected list or vec, found {}", args[0])
            }),
        ),
        // Index
        (
            "nth",
            lisp_fn_len!(|args where len == 2| {
                if let MalType::List(v) | MalType::Vec(v) = &args[0]
                && let MalType::Int(i) = &args[1]
                {
                    return match v.get(*i as usize) {
                        Some(e) => Ok(e.clone()),
                        None => mal_err!("nth err: out of bounds")
                    }
                }

                mal_err!("expected list or vec + int, found {} + {}", args[0], args[1])
            }),
        ),
        (
            "first",
            lisp_fn_len!(|args where len == 1| {
                return match &args[0] {
                    MalType::List(v) | MalType::Vec(v) => {
                        match v.first() {
                            Some(e) => Ok(e.clone()),
                            None => Ok(MalType::Nil),
                        }
                    },
                    MalType::Nil => Ok(MalType::Nil),
                    _ => mal_err!("expected list or vec, found {}", args[0]),
                }
            }),
        ),
        (
            "rest",
            lisp_fn_len!(|args where len == 1| {
                match &args[0] {
                    MalType::List(v) | MalType::Vec(v) =>
                    {
                        if v.is_empty() {
                            return Ok(MalType::List(vec![]));
                        }

                        return Ok(MalType::List(v[1..].to_vec()))
                    },
                    MalType::Nil => Ok(MalType::List(vec![])),
                    _ => mal_err!("expected list or vec, found {}", args[0]),
                }
            }),
        ),
        // Do
        ("read-string", lisp_fn!(|s: MalType::Str| read_str(s))),
        (
            "count",
            lisp_fn_len!(|args where len == 1| {
                Ok(MalType::Int(
                    match &args[0] {
                        MalType::List(a) | MalType::Vec(a) => a.len() as i64,
                        MalType::Nil => 0,
                        _ => {
                            return mal_err!("expected list or vec, found {}", args[0]);
                        }
                    }))
            }),
        ),
        (
            "deref",
            lisp_fn!(|atom: MalType::Atom| Ok(atom.borrow().clone())),
        ),
        (
            "reset!",
            lisp_fn_len!(|args where len == 2| {
                if let MalType::Atom(inner) = &args[0] {
                    *inner.borrow_mut() = args[1].clone();
                    return Ok(inner.borrow().clone());
                }
                mal_err!("expected atom, found {}", args[0])
            }),
        ),
        (
            "swap!",
            lisp_fn_len!(|args where len >= 2| {
                if let MalType::Atom(inner) = args[0].clone() {
                    let mut a = args[1..].to_vec();
                    a.insert(1, inner.borrow().clone());

                    let result = args[1].apply(&a[1..])?;

                    *inner.borrow_mut() = result.clone();

                    return Ok(result);
                }
                mal_err!("expected atom, func and other args, found {}", MalType::List(args))
            }),
        ),
        (
            "cons",
            lisp_fn_len!(|args where len == 2| {
                if let MalType::List(v) | MalType::Vec(v) = &args[1] {
                    let mut new_list = v.clone();
                    new_list.insert(0, args[0].clone());

                    return Ok(MalType::List(new_list));
                }
                mal_err!("expected second argument to be a list, found {}", args[1])
            }),
        ),
        (
            "concat",
            MalType::Builtin(|args| {
                let mut new_list = vec![];

                for i in args {
                    if let MalType::List(v) | MalType::Vec(v) = i {
                        new_list.extend(v.clone());
                    } else {
                        return mal_err!("expected all arguments to be of type list or vec");
                    }
                }

                Ok(MalType::List(new_list))
            }),
        ),
        (
            "throw",
            lisp_fn_len!(|args where len == 1| {
                Err(MalErr::Throw(args[0].clone()))
            }),
        ),
        (
            "apply",
            lisp_fn_len!(|args where len >= 2| {
                if let MalType::List(v) | MalType::Vec(v) = args.last().unwrap() {
                    let a = [&args[1..args.len() - 1], v].concat();

                    args[0].apply(&a)
                }
                else {
                    mal_err!("expected last argument to be of type list or vec")
                }
            }),
        ),
        (
            "map",
            lisp_fn_len!(|args where len == 2| {
                if let MalType::List(v) | MalType::Vec(v) = &args[1] {
                    let res = v.iter().map(|i| args[0].apply(&[i.clone()])).collect::<Result<Vec<MalType>, MalErr>>()?;
                    return Ok(MalType::List(res));
                }
                mal_err!("expected second argument to be of type list or vec")
            }),
        ),
    ]
}
