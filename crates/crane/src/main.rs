mod ast;
mod backend;
mod lexer;
mod parser;

use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::{Parser, Subcommand};
use tracing::Level;
use tracing_subscriber::FmtSubscriber;

use crate::backend::javascript::JsBackend;
use crate::parser::ParseErrorKind;

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
        Ok(statements) => {
            use std::fs::{self, File};
            use std::io::Write;

            let backend = JsBackend::new();

            let output = backend.compile(statements);

            fs::create_dir_all("build").unwrap();

            let mut file = File::create("build/main.js").unwrap();

            file.write_all(output.as_bytes()).unwrap();

            println!("Compiled!");

            Ok(())
        }
        Err(err) => {
            let span = err.span;

            let error_report = match err.kind {
                ParseErrorKind::LexError(lex_error) => {
                    Report::build(ReportKind::Error, "scratch.crane", 1)
                        .with_message("An error occurred during lexing.")
                        .with_label(
                            Label::new(("scratch.crane", span))
                                .with_message(lex_error)
                                .with_color(Color::Red),
                        )
                        .finish()
                }
                ParseErrorKind::AdvancedPastEndOfInput => {
                    Report::build(ReportKind::Error, "scratch.crane", 1)
                        .with_message("An error occurred during parsing.")
                        .with_label(
                            Label::new(("scratch.crane", span))
                                .with_message(err.kind)
                                .with_color(Color::Red),
                        )
                        .finish()
                }
                ParseErrorKind::Unknown => Report::build(ReportKind::Error, "scratch.crane", 1)
                    .with_message("An error occurred during parsing.")
                    .with_label(
                        Label::new(("scratch.crane", span))
                            .with_message(err.kind)
                            .with_color(Color::Red),
                    )
                    .finish(),
            };

            error_report
                .eprint(("scratch.crane", Source::from(source)))
                .unwrap();

            Err(())
        }
    }
}

fn run() {
    use std::process::Command;

    let exit_status = Command::new("node")
        .arg("build/main.js")
        .status()
        .expect("Failed to run");

    println!("Exited with {}", exit_status);
}
