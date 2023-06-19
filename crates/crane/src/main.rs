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
use itertools::Itertools;
use lexer::Lexer;
use thin_vec::thin_vec;
use tracing::Level;
use tracing_subscriber::FmtSubscriber;
use typer::TypeErrorKind;

use crate::ast::{Module, Package};
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

    let source = std::fs::read_to_string(&example_file).unwrap();

    let lexer = Lexer::new(&source);

    let parser = crate::parser::Parser::new(lexer);

    match parser.parse() {
        Ok(items) => {
            let mut typer = Typer::new();

            let module = Module { items };

            let package = Package {
                modules: thin_vec![module],
            };

            match typer.type_check_package(package) {
                Ok(typed_package) => {
                    std::fs::create_dir_all("build").unwrap();

                    let backend = NativeBackend::new();

                    backend.compile(typed_package);

                    println!("Compiled!");

                    Ok(())
                }
                Err(type_error) => {
                    let span = type_error.span;

                    let example_file = example_file.display().to_string();

                    let error_report = match type_error.kind {
                        TypeErrorKind::UnknownFunction { path, options } => {
                            let report = Report::build(ReportKind::Error, &example_file, 1)
                                .with_message("A type error occurred.")
                                .with_label(
                                    Label::new(SourceSpan::from((&example_file, span)))
                                        .with_message(format!("Function `{path}` does not exist.",))
                                        .with_color(Color::Red),
                                );

                            let suggestion = options
                                .iter()
                                .sorted_by_key(|option| option.to_string())
                                .min_by_key(|option| {
                                    strsim::levenshtein(&option.to_string(), &path.to_string())
                                });

                            let report = if let Some(suggestion) = suggestion {
                                report.with_label(
                                    Label::new(SourceSpan::from((&example_file, suggestion.span)))
                                        .with_message(format!(
                                            "There is a function with a similar name: `{}`.",
                                            suggestion.clone()
                                        ))
                                        .with_color(Color::Cyan),
                                )
                            } else {
                                report
                            };

                            report.finish()
                        }
                        TypeErrorKind::Error(message) => {
                            Report::build(ReportKind::Error, &example_file, 1)
                                .with_message("A type error occurred.")
                                .with_label(
                                    Label::new(SourceSpan::from((&example_file, span)))
                                        .with_message(message)
                                        .with_color(Color::Red),
                                )
                                .finish()
                        }
                    };

                    error_report
                        .eprint((example_file.into(), Source::from(source)))
                        .unwrap();

                    Err(())
                }
            }
        }
        Err(err) => {
            let span = err.span;

            let example_file = example_file.display().to_string();

            let error_report = match err.kind {
                ParseErrorKind::LexError(lex_error) => {
                    Report::build(ReportKind::Error, &example_file, 1)
                        .with_message("An error occurred during lexing.")
                        .with_label(
                            Label::new(SourceSpan::from((&example_file, span)))
                                .with_message(lex_error)
                                .with_color(Color::Red),
                        )
                        .finish()
                }
                ParseErrorKind::Error(message) => {
                    Report::build(ReportKind::Error, &example_file, 1)
                        .with_message("An error occurred during parsing.")
                        .with_label(
                            Label::new(SourceSpan::from((&example_file, span)))
                                .with_message(message)
                                .with_color(Color::Red),
                        )
                        .finish()
                }
            };

            error_report
                .eprint((example_file.into(), Source::from(source)))
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
