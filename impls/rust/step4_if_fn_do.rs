use std::cell::RefCell;
use std::rc::Rc;

use rustyline::Editor;
use rustyline::error::ReadlineError;

mod core;
mod env;
mod reader;
mod types;

use env::*;

fn main() {
    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new().unwrap();
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    let repl_env = &Rc::new(RefCell::new(MalEnv::new()));

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                rl.save_history(".mal-history").unwrap();
                if !line.is_empty() {
                    let form = reader::read_str(&line);
                    match form {
                        Ok(f) => println!(
                            "{}",
                            match f.eval(repl_env) {
                                Ok(o) => o.to_string(),
                                Err(e) => format!("{e}"),
                            }
                        ),
                        Err(e) => eprintln!("Err: {e}"),
                    }
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }
}
