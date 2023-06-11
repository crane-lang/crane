mod ast;
mod backend;
mod lexer;
mod parser;

use clap::{Parser, Subcommand};

use crate::backend::javascript::JsBackend;

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
                    use std::fs::{self, File};
                    use std::io::Write;

                    let backend = JsBackend::new();

                    let output = backend.compile(statements);

                    fs::create_dir_all("build").unwrap();

                    let mut file = File::create("build/main.js").unwrap();

                    file.write_all(output.as_bytes()).unwrap();

                    println!("Compiled!")
                }
                Err(err) => {
                    eprintln!("{}", err)
                }
            }
        }
    }
}
