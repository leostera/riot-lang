use std::path::PathBuf;

use tinyml::{
    checker::Checker,
    lambda::lower::Lowerer,
    parser::{lexer::Lexer, parse::Parser},
};

fn source(path: &str) -> &'static str {
    Box::leak(std::fs::read_to_string(path).expect(path).into_boxed_str())
}

fn lex_snapshot(path: &str) -> String {
    let src = source(path);
    let lexer = Lexer::new(src).expect("lex ok");
    format!("{:#?}", lexer.tokens())
}

fn parse_module(path: &str) -> tinyml::parser::ast::Module {
    let src = source(path);
    let lexer = Lexer::new(src).expect("lex ok");
    Parser::new(lexer).parse_module().expect("parse ok")
}

fn parse_snapshot(path: &str) -> String {
    let module = parse_module(path);
    format!("{module:#?}")
}

fn check_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let mut checker = Checker::new();
    let summary = checker.check_module(&module);
    format!("{summary:#?}")
}

fn lower_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let mut checker = Checker::new();
    let summary = checker.check_module(&module);
    let lambda = Lowerer.lower_module(&summary.tst);
    format!("{lambda:#?}")
}

fn emit_js_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let mut checker = Checker::new();
    let summary = checker.check_module(&module);
    let lambda = Lowerer.lower_module(&summary.tst);
    format!("emit pending\n{lambda:#?}")
}

fn emit_native_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let mut checker = Checker::new();
    let summary = checker.check_module(&module);
    let lambda = Lowerer.lower_module(&summary.tst);
    format!("emit pending\n{lambda:#?}")
}

fn emit_wasm_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let mut checker = Checker::new();
    let summary = checker.check_module(&module);
    let lambda = Lowerer.lower_module(&summary.tst);
    format!("emit pending\n{lambda:#?}")
}

macro_rules! assert_corpus_snapshot {
    ($value:expr) => {{
        let mut settings = insta::Settings::clone_current();
        settings.set_snapshot_path(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/snapshots"));
        settings.bind(|| insta::assert_snapshot!($value));
    }};
}

macro_rules! corpus_tests {
    ($($name:ident => $path:literal),+ $(,)?) => {
        $(
            mod $name {
                use super::*;

                #[test]
                fn lex() {
                    assert_corpus_snapshot!(lex_snapshot($path));
                }

                #[test]
                fn parse() {
                    assert_corpus_snapshot!(parse_snapshot($path));
                }

                #[test]
                fn lower() {
                    assert_corpus_snapshot!(lower_snapshot($path));
                }

                #[test]
                fn check() {
                    assert_corpus_snapshot!(check_snapshot($path));
                }

                #[test]
                fn emit_js() {
                    assert_corpus_snapshot!(emit_js_snapshot($path));
                }

                #[test]
                fn emit_native() {
                    assert_corpus_snapshot!(emit_native_snapshot($path));
                }

                #[test]
                fn emit_wasm() {
                    assert_corpus_snapshot!(emit_wasm_snapshot($path));
                }
            }
        )+
    };
}

include!(concat!(env!("OUT_DIR"), "/corpus_tests_generated.rs"));
