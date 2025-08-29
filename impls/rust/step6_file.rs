use std::cell::RefCell;
use std::rc::Rc;

use rustyline::Editor;
use rustyline::error::ReadlineError;

mod core;
mod env;
mod reader;
mod types;

use env::*;
use types::*;

// stolen from the existing impl
// I have no clue how I would make (eval ...) use the top level REPL_ENV otherwire
thread_local! {
    static REPL_ENV: Rc<RefCell<MalEnv>> = Rc::new(RefCell::new(MalEnv::new()));
}

fn main() {
    REPL_ENV.with(|repl_env| {
        let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new().unwrap();

        if rl.load_history(".mal-history").is_err() {
            eprintln!("No previous history.");
        }

        for i in core::ns() {
            REPL_ENV.with(|e| e.borrow_mut().set(i.0.to_string(), i.1));
        }

        reader::read_str(
            "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))",
        )
        .unwrap()
        .eval(repl_env)
        .unwrap();

        repl_env.borrow_mut().set(
            "eval".to_string(),
            MalType::Builtin(|a| REPL_ENV.with(|e| a[0].eval(e))),
        );

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
    })
}
