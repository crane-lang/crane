mod ast;
mod backend;
mod lexer;
mod parser;
mod typer;

use ariadne::{Color, Label, Report, ReportKind, Source};
use ast::SourceSpan;
use clap::{Parser, Subcommand};
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
        Command::Build => {
            let _ = compile();
        }
        Command::Run => {
            if let Ok(_) = compile() {
                run();
            }
        }
    }
}

fn compile() -> Result<(), ()> {
    let source = std::fs::read_to_string("examples/scratch.crane").unwrap();

    let parser = crate::parser::Parser::new(&source);

    match parser.parse() {
        Ok(items) => {
            let mut typer = Typer::new();

            let module = Module {
                items: items.clone().into(),
            };

            match typer.type_check_module(module) {
                Ok(()) => {
                    std::fs::create_dir_all("build").unwrap();

                    let backend = NativeBackend::new();

                    backend.compile(items);

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
                ParseErrorKind::AdvancedPastEndOfInput => {
                    Report::build(ReportKind::Error, "scratch.crane", 1)
                        .with_message("An error occurred during parsing.")
                        .with_label(
                            Label::new(SourceSpan::from(("scratch.crane", span)))
                                .with_message(err.kind)
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
