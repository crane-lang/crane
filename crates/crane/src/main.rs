mod ast;
mod backend;
mod lexer;
mod parser;
mod typer;

use std::io::Write;
use std::path::PathBuf;

use ariadne::{Color, Label, Report, ReportKind, Source};
use ast::SourceSpan;
use clap::{Parser, Subcommand};
use lexer::Lexer;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;
use typer::TypeErrorKind;

use crate::ast::Module;
use crate::backend::native::NativeBackend;
use crate::parser::ParseErrorKind;
use crate::typer::Typer;

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
    Build,

    /// Runs the current project.
    Run,
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
        Command::Build => {
            let _ = compile();
        }
        Command::Run => {
            if compile().is_ok() {
                run();
            }
        }
    }
}

fn compile() -> Result<(), ()> {
    let source = std::fs::read_to_string("examples/scratch.crane").unwrap();

    let lexer = Lexer::new(&source);

    let parser = crate::parser::Parser::new(lexer);

    match parser.parse() {
        Ok(items) => {
            let mut typer = Typer::new();

            let module = Module { items };

            match typer.type_check_module(module) {
                Ok(typed_module) => {
                    std::fs::create_dir_all("build").unwrap();

                    let backend = NativeBackend::new();

                    backend.compile(typed_module.items.into());

                    println!("Compiled!");

                    Ok(())
                }
                Err(type_error) => {
                    let span = type_error.span;

                    let error_report = match type_error.kind {
                        TypeErrorKind::UnknownFunction { name } => {
                            Report::build(ReportKind::Error, "scratch.crane", 1)
                                .with_message("A type error occurred.")
                                .with_label(
                                    Label::new(SourceSpan::from(("scratch.crane", span)))
                                        .with_message(format!("Function `{name}` does not exist."))
                                        .with_color(Color::Red),
                                )
                                .finish()
                        }
                        TypeErrorKind::Error(message) => {
                            Report::build(ReportKind::Error, "scratch.crane", 1)
                                .with_message("A type error occurred.")
                                .with_label(
                                    Label::new(SourceSpan::from(("scratch.crane", span)))
                                        .with_message(message)
                                        .with_color(Color::Red),
                                )
                                .finish()
                        }
                    };

                    error_report
                        .eprint(("scratch.crane".into(), Source::from(source)))
                        .unwrap();

                    Err(())
                }
            }
        }
        Err(err) => {
            let span = err.span;

            let error_report = match err.kind {
                ParseErrorKind::LexError(lex_error) => {
                    Report::build(ReportKind::Error, "scratch.crane", 1)
                        .with_message("An error occurred during lexing.")
                        .with_label(
                            Label::new(SourceSpan::from(("scratch.crane", span)))
                                .with_message(lex_error)
                                .with_color(Color::Red),
                        )
                        .finish()
                }
                ParseErrorKind::Error(message) => {
                    Report::build(ReportKind::Error, "scratch.crane", 1)
                        .with_message("An error occurred during parsing.")
                        .with_label(
                            Label::new(SourceSpan::from(("scratch.crane", span)))
                                .with_message(message)
                                .with_color(Color::Red),
                        )
                        .finish()
                }
            };

            error_report
                .eprint(("scratch.crane".into(), Source::from(source)))
                .unwrap();

            Err(())
        }
    }
}

fn run() {
    use std::process::Command;

    let exit_status = Command::new("./build/main")
        .status()
        .expect("Failed to run");

    println!("Exited with {}", exit_status);
}
