use std::mem::discriminant;

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
        ("not", read_str("(fn* (a) (if a false true))").unwrap()),
        (
            "list?",
            MalType::Builtin(|args| {
                Ok(MalType::Bool(
                    discriminant(&MalType::List(vec![])) == discriminant(&args[0]),
                ))
            }),
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
        // Index
        // Do
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
    ]
}
