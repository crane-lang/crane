use std::path::PathBuf;

use ariadne::{Color, Label, Report, ReportKind, Source};
use itertools::Itertools;
use thin_vec::thin_vec;

use crate::ast::{Module, Package, SourceSpan};
use crate::backend::native::NativeBackend;
use crate::lexer::Lexer;
use crate::parser::{ParseErrorKind, Parser};
use crate::typer::{TypeErrorKind, Typer};

/// The input to the compiler.
pub enum Input {
    /// Load the source code from a file.
    File(PathBuf),

    /// Load source code from a string.
    String {
        /// The "filename" where the source code is coming from.
        filename: String,

        /// The source code.
        input: String,
    },
}

pub struct CompileParams {
    pub input: Input,
}

/// The interface to the Crane compiler.
pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&mut self, params: CompileParams) -> Result<(), ()> {
        let (filepath, source) = match params.input {
            Input::File(path) => (
                path.display().to_string(),
                std::fs::read_to_string(&path).unwrap(),
            ),
            Input::String { filename, input } => (filename, input),
        };

        let lexer = Lexer::new(&source);
        let parser = Parser::new(lexer);

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

                        let error_report = match type_error.kind {
                            TypeErrorKind::InvalidFunctionName { reason, suggestion } => {
                                Report::build(ReportKind::Error, &filepath, 1)
                                    .with_message("A type error occurred.")
                                    .with_label(
                                        Label::new(SourceSpan::from((&filepath, span)))
                                            .with_message(reason)
                                            .with_color(Color::Red),
                                    )
                                    .with_label(
                                        Label::new(SourceSpan::from((&filepath, span)))
                                            .with_message(format!(
                                                "Try writing it as `{suggestion}` instead."
                                            ))
                                            .with_color(Color::Cyan),
                                    )
                                    .finish()
                            }
                            TypeErrorKind::UnknownModule { path, options } => {
                                let report = Report::build(ReportKind::Error, &filepath, 1)
                                    .with_message("A type error occurred.")
                                    .with_label(
                                        Label::new(SourceSpan::from((&filepath, span)))
                                            .with_message(format!(
                                                "Module `{path}` does not exist.",
                                            ))
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
                                        Label::new(SourceSpan::from((&filepath, suggestion.span)))
                                            .with_message(format!(
                                                "There is a module with a similar name: `{}`.",
                                                suggestion.clone()
                                            ))
                                            .with_color(Color::Cyan),
                                    )
                                } else {
                                    report
                                };

                                report.finish()
                            }
                            TypeErrorKind::UnknownFunction { path, options } => {
                                let report = Report::build(ReportKind::Error, &filepath, 1)
                                    .with_message("A type error occurred.")
                                    .with_label(
                                        Label::new(SourceSpan::from((&filepath, span)))
                                            .with_message(format!(
                                                "Function `{path}` does not exist.",
                                            ))
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
                                        Label::new(SourceSpan::from((&filepath, suggestion.span)))
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
                                Report::build(ReportKind::Error, &filepath, 1)
                                    .with_message("A type error occurred.")
                                    .with_label(
                                        Label::new(SourceSpan::from((&filepath, span)))
                                            .with_message(message)
                                            .with_color(Color::Red),
                                    )
                                    .finish()
                            }
                        };

                        error_report
                            .eprint((filepath.into(), Source::from(source)))
                            .unwrap();

                        Err(())
                    }
                }
            }
            Err(err) => {
                let span = err.span;

                let error_report = match err.kind {
                    ParseErrorKind::LexError(lex_error) => {
                        Report::build(ReportKind::Error, &filepath, 1)
                            .with_message("An error occurred during lexing.")
                            .with_label(
                                Label::new(SourceSpan::from((&filepath, span)))
                                    .with_message(lex_error)
                                    .with_color(Color::Red),
                            )
                            .finish()
                    }
                    ParseErrorKind::Error(message) => {
                        Report::build(ReportKind::Error, &filepath, 1)
                            .with_message("An error occurred during parsing.")
                            .with_label(
                                Label::new(SourceSpan::from((&filepath, span)))
                                    .with_message(message)
                                    .with_color(Color::Red),
                            )
                            .finish()
                    }
                };

                error_report
                    .eprint((filepath.into(), Source::from(source)))
                    .unwrap();

                Err(())
            }
        }
    }
}
