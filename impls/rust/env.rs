use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{mal_err, types::*};

#[derive(Debug, Clone, PartialEq)]
pub struct MalEnv {
    outer: Option<Rc<MalEnv>>,
    data: RefCell<HashMap<String, MalType>>,
}

#[allow(dead_code)]
impl MalEnv {
    pub fn new(outer: Option<Rc<Self>>) -> MalEnv {
        MalEnv {
            outer,
            data: HashMap::<String, MalType>::new().into(),
        }
    }

    pub fn get(&self, key: &str) -> Option<MalType> {
        let mut tmp_env = self;

        loop {
            if let Some(val) = tmp_env.data.borrow().get(key) {
                return Some(val.clone());
            } else if let Some(new_env) = &tmp_env.outer {
                tmp_env = new_env;
            } else {
                return None;
            }
        }
    }

    pub fn set(&self, key: String, val: MalType) -> Option<MalType> {
        self.data.borrow_mut().insert(key, val)
    }

    pub fn bind_env(&self, from: &[MalType], to: &[MalType]) -> Result<Self, MalErr> {
        let env = Self::new(Some(Rc::new(self.clone())));

        let mut params = from.to_vec();
        let mut to_mut = to.to_vec();

        let mut is_variadic = false;
        if from.len() >= 2
            && let MalType::Symbol(sym) = &from[from.len() - 2]
            && sym == "&"
        {
            is_variadic = true;
        }

        match is_variadic {
            false if from.len() != to.len() => {
                return mal_err!(
                    "bind failed: both sides must have the same size, {} != {}",
                    from.len(),
                    to.len()
                );
            }
            true if from.len() - 2 > to.len() => {
                return mal_err!(
                    "bind failed: expected at least {} arguments, found {}",
                    from.len(),
                    to.len()
                );
            }
            true => {
                params.remove(from.len() - 2);
                to_mut = to[..from.len() - 2].to_vec();
                to_mut.push(MalType::List(to[from.len() - 2..].to_vec()));
            }
            _ => {}
        }

        for (i, j) in params.iter().enumerate() {
            env.set(j.to_string(), to_mut[i].clone());
        }

        Ok(env)
    }
}

impl Display for MalEnv {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out: String = format!("data: {:?}", self.data);

        writeln!(f, "{{ {out}}}")
    }
}
