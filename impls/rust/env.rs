use itertools::Itertools;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{mal_err, types::*};

#[derive(Debug, Clone)]
pub struct MalEnv {
    outer: Option<Rc<MalEnv>>,
    data: HashMap<String, MalType>,
}

pub fn int_op(args: MalArgs, f: fn(f1: i64, f2: i64) -> i64) -> MalRet {
    if args.len() != 2 {
        return mal_err!("expected 2 arguments, found '{}'", args.iter().join(" "));
    }

    let mut args_iter = args.iter();

    let args_tuple = (args_iter.next(), args_iter.next());

    match args_tuple {
        (Some(MalType::Int(i1)), Some(MalType::Int(i2))) => Ok(MalType::Int(f(*i1, *i2))),
        _ => mal_err!(
            "expected int int, found '{}, {}'",
            args_tuple.0.unwrap_or(&MalType::Nil),
            args_tuple.1.unwrap_or(&MalType::Nil)
        ),
    }
}

#[allow(dead_code)]
impl MalEnv {
    pub fn new() -> MalEnv {
        let mut e = MalEnv {
            outer: None,
            data: HashMap::<String, MalType>::new(),
        };

        e.set(
            "+".to_string(),
            MalType::Func(|args| int_op(args, |a, b| a + b)),
        );
        e.set(
            "-".to_string(),
            MalType::Func(|args| int_op(args, |a, b| a - b)),
        );
        e.set(
            "*".to_string(),
            MalType::Func(|args| int_op(args, |a, b| a * b)),
        );
        e.set(
            "/".to_string(),
            MalType::Func(|args| int_op(args, |a, b| a / b)),
        );
        e
    }

    pub fn new_into_outer(&self) -> Self {
        Self {
            outer: Some(Rc::new(self.clone())),
            data: HashMap::<String, MalType>::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&MalType> {
        let mut tmp_env = self;

        loop {
            if let Some(val) = tmp_env.data.get(key) {
                return Some(val);
            } else if let Some(new_env) = &tmp_env.outer {
                tmp_env = new_env;
            } else {
                return None;
            }
        }
    }

    pub fn set(&mut self, key: String, val: MalType) -> Option<MalType> {
        self.data.insert(key, val)
    }
}

impl Display for MalEnv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let outer_str = match self.outer.is_some() {
            false => "none".to_string(),
            true => self.outer.clone().unwrap().to_string(),
        };

        let out: String = format!("outer: {},\ndata: {:?}", outer_str, self.data);

        writeln!(f, "{{ {out}}}")
    }
}
