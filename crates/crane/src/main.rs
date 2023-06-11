mod ast;
mod lexer;
mod parser;

use clap::{Parser, Subcommand};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Compiles the current project.
    Build,
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Build => {
            let source = std::fs::read_to_string("examples/hello_world.crane").unwrap();

            let parser = crate::parser::Parser::new(&source);

            match parser.parse() {
                Ok(statements) => {
                    println!("Compiled!")
                }
                Err(err) => {
                    eprintln!("{}", err)
                }
            }
        }
    }
}
