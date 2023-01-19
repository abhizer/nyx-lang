use nyxc_parser::Parser;
use rustyline::{error::ReadlineError, Editor, Result};

fn main() -> Result<()> {
    let mut rl = Editor::<()>::new()?;
    if rl.load_history("history.txt").is_err() {
        println!("No previous REPL history");
    }

    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let mut parser = Parser::new(&line);
                match parser.parse() {
                    Ok(ast) => {
                        println!("{ast:?}");
                    }
                    Err(e) => {
                        println!("Got error while parsing: ");
                        e.into_iter().for_each(|e| {
                            println!("{e:?}");
                        });
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
