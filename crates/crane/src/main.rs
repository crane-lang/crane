mod ast;
mod backend;
mod compiler;
mod interned;
mod lexer;
mod parser;
mod typer;

use std::io::Write;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

use crate::compiler::{CompileParams, Compiler, Input};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Creates a new Crane project.
    New {
        /// The path at which to create the project.
        path: PathBuf,
    },

    /// Compiles the current project.
    Build {
        /// Builds the given example.
        #[arg(long)]
        example: Option<String>,
    },

    /// Runs the current project.
    Run {
        /// Runs the given example.
        #[arg(long)]
        example: Option<String>,
    },
}

fn main() {
    let args = Args::parse();

    let subscriber = FmtSubscriber::builder()
        .with_max_level(Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("failed to set default tracing subscriber");

    match args.command {
        Command::New { path } => {
            use std::fs::{self, File};

            fs::create_dir(&path).expect("Failed to create directory");

            let mut src_path = path.clone();
            src_path.push("src");

            fs::create_dir_all(&src_path).unwrap();

            let mut main_path = src_path;
            main_path.push("main.crane");

            let mut main = File::create(&main_path).unwrap();

            let hello_world_program = r#"
fn main() {
    println("你好，世界。")
}
            "#
            .trim();

            main.write_all(hello_world_program.as_bytes()).unwrap();
        }
        Command::Build { example } => {
            let _ = compile(example);
        }
        Command::Run { example } => {
            if compile(example).is_ok() {
                run();
            }
        }
    }
}

fn compile(example: Option<String>) -> Result<(), ()> {
    // TODO: Don't force the usage of an example.
    let example = example.unwrap_or("scratch".to_string());

    let examples_path = PathBuf::from("examples");

    let mut example_file = examples_path;
    example_file.push(format!("{example}.crane"));

    let mut compiler = Compiler::new();

    let params = CompileParams {
        input: Input::File(example_file),
    };

    compiler.compile(&mut std::io::stderr(), params)
}

fn run() {
    use std::process::Command;

    let exit_status = Command::new("./build/main")
        .status()
        .expect("Failed to run");

    println!("Exited with {}", exit_status);
}
