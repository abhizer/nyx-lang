use std::{time::Instant, io::{BufReader, Read}, fs::File};

use nyxc_evaluator::Evaluator;
use nyxc_parser::Parser;
use nyxc_typechecker::TypeChecker;

use anyhow::{Result, anyhow}; 

fn main() -> Result<()> {
    let Some(file) = std::env::args().nth(1) else {
        eprintln!("err: source file not provided"); 
        eprintln!("usage: nyxc main.nyx"); 
        std::process::exit(1); 
    };

    println!("Processing Source Code");

    let mut bf = BufReader::new(File::open(file)?); 

    let mut source = String::new(); 

    bf.read_to_string(&mut source)?; 

    let mut parser = Parser::new(&source); 
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            for diag in e {
                println!("parsing err: {diag:?}");
            }
            std::process::exit(1); 
        },
    }; 

    let mut type_checker = TypeChecker::default(); 
    type_checker.check_program(&ast).map_err(|e| anyhow!(e))?; 

    println!("Done");

    println!("stdout:");
    println!("-----------------------------");

    let time = Instant::now(); 
    let mut evaluator = Evaluator::default(); 
    evaluator.eval(&ast); 

    println!("-----------------------------");
    println!("Execution took: {:?} milli seconds", time.elapsed().as_millis()); 

    Ok(())
}
