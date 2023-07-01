use std::io::Write;
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

    pub fn compile<'io>(
        &mut self,
        stderr: &'io mut impl Write,
        params: CompileParams,
    ) -> Result<(), ()> {
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

                        let context = inkwell::context::Context::create();

                        let backend = NativeBackend::new(&context);

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
                            TypeErrorKind::InvalidTypeName { reason, suggestion } => {
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
                            TypeErrorKind::UnknownType { path, options } => {
                                let report = Report::build(ReportKind::Error, &filepath, 1)
                                    .with_message("A type error occurred.")
                                    .with_label(
                                        Label::new(SourceSpan::from((&filepath, span)))
                                            .with_message(format!("Type `{path}` does not exist.",))
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
                                                "There is a type with a similar name: `{}`.",
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
                            .write((filepath.into(), Source::from(source)), stderr)
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
                    .write((filepath.into(), Source::from(source)), stderr)
                    .unwrap();

                Err(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_camel_case_function_name() {
        let mut compiler = Compiler::new();

        let params = CompileParams {
            input: Input::String {
                filename: "camel_case.crane".into(),
                input: r#"
fn main() {}

fn inCamelCase() {}
                "#
                .trim()
                .to_string(),
            },
        };

        let mut stderr = Vec::new();

        let _ = compiler.compile(&mut stderr, params);

        let stderr = strip_ansi_escapes::strip(stderr).unwrap();
        let stderr = std::str::from_utf8(&stderr).unwrap();

        insta::assert_snapshot!(&stderr);
    }

    #[test]
    pub fn test_mixed_case_function_name() {
        let mut compiler = Compiler::new();

        let params = CompileParams {
            input: Input::String {
                filename: "mixed_case.crane".into(),
                input: r#"
fn main() {}

fn XMLHttpRequest() {}
                "#
                .trim()
                .to_string(),
            },
        };

        let mut stderr = Vec::new();

        let _ = compiler.compile(&mut stderr, params);

        let stderr = strip_ansi_escapes::strip(stderr).unwrap();
        let stderr = std::str::from_utf8(&stderr).unwrap();

        insta::assert_snapshot!(&stderr);
    }

    #[test]
    pub fn test_snake_case_struct_name() {
        let mut compiler = Compiler::new();

        let params = CompileParams {
            input: Input::String {
                filename: "snake_case.crane".into(),
                input: r#"
struct snake_cased_struct {
    foo: Uint64,
    bar: Uint64,
}
                "#
                .trim()
                .to_string(),
            },
        };

        let mut stderr = Vec::new();

        let _ = compiler.compile(&mut stderr, params);

        let stderr = strip_ansi_escapes::strip(stderr).unwrap();
        let stderr = std::str::from_utf8(&stderr).unwrap();

        insta::assert_snapshot!(&stderr);
    }

    #[test]
    pub fn test_mixed_case_struct_name() {
        let mut compiler = Compiler::new();

        let params = CompileParams {
            input: Input::String {
                filename: "mixed_case.crane".into(),
                input: r#"
struct XMLHttpRequest {
    foo: Uint64,
    bar: Uint64,
}
                "#
                .trim()
                .to_string(),
            },
        };

        let mut stderr = Vec::new();

        let _ = compiler.compile(&mut stderr, params);

        let stderr = strip_ansi_escapes::strip(stderr).unwrap();
        let stderr = std::str::from_utf8(&stderr).unwrap();

        insta::assert_snapshot!(&stderr);
    }

    #[test]
    pub fn test_snake_case_union_name() {
        let mut compiler = Compiler::new();

        let params = CompileParams {
            input: Input::String {
                filename: "snake_case.crane".into(),
                input: r#"
union snake_cased_union {
    Foo,
    Bar,
}
                "#
                .trim()
                .to_string(),
            },
        };

        let mut stderr = Vec::new();

        let _ = compiler.compile(&mut stderr, params);

        let stderr = strip_ansi_escapes::strip(stderr).unwrap();
        let stderr = std::str::from_utf8(&stderr).unwrap();

        insta::assert_snapshot!(&stderr);
    }

    #[test]
    pub fn test_mixed_case_union_name() {
        let mut compiler = Compiler::new();

        let params = CompileParams {
            input: Input::String {
                filename: "mixed_case.crane".into(),
                input: r#"
union XMLHttpRequest {
    Foo,
    Bar,
}
                "#
                .trim()
                .to_string(),
            },
        };

        let mut stderr = Vec::new();

        let _ = compiler.compile(&mut stderr, params);

        let stderr = strip_ansi_escapes::strip(stderr).unwrap();
        let stderr = std::str::from_utf8(&stderr).unwrap();

        insta::assert_snapshot!(&stderr);
    }

    #[test]
    pub fn test_struct_expression() {
        let mut compiler = Compiler::new();

        let params = CompileParams {
            input: Input::String {
                filename: "mixed_case.crane".into(),
                input: r#"
struct User {
    first_name: String,
    last_name: String,
    age: Uint64,
}

fn main() {
    let user = User {
        first_name: "Elaine",
        last_name: "Benes",
        age: 27,
    }
}
                "#
                .trim()
                .to_string(),
            },
        };

        let mut stderr = Vec::new();

        let _ = compiler.compile(&mut stderr, params);

        let stderr = strip_ansi_escapes::strip(stderr).unwrap();
        let stderr = std::str::from_utf8(&stderr).unwrap();

        insta::assert_snapshot!(&stderr);
    }
}
