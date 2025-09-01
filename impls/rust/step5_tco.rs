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

    let repl_env = &Rc::new(MalEnv::new(None));

    for i in core::ns() {
        repl_env.set(i.0.to_string(), i.1);
    }

    reader::read_str("(def! not (fn* (a) (if a false true)))")
        .unwrap()
        .eval(repl_env)
        .unwrap();

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
                                Ok(o) => format!("{o:#}"),
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
