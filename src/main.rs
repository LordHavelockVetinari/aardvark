mod compile;
mod parse;
mod vm;

#[cfg(test)]
mod tests;

use std::error::Error;
use std::io::{stdout, Write};
use std::process::ExitCode;

fn run_str(s: &str, error_handler: Box<dyn Write>) -> Result<(), Box<dyn Error>> {
    let prog = parse::parse(s.chars())?;
    vm::VirtualMachine::with(Box::new(error_handler), |vm| {
        let main = compile::compile(prog, vm)?;
        vm.run_main(main);
        Ok(())
    })
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    let filename = match args.len() {
        0..=1 => {
            eprintln!("Error: No input file.");
            return ExitCode::FAILURE;
        }
        2 => &args[1],
        _ => {
            eprintln!("Error: Too many command line arguments.");
            return ExitCode::FAILURE;
        }
    };
    let prog = match std::fs::read_to_string(filename) {
        Ok(prog) => prog,
        Err(_) => {
            eprintln!("Error: File '{filename}' not found.");
            return ExitCode::FAILURE;
        }
    };
    match run_str(&prog, Box::new(stdout())) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}
