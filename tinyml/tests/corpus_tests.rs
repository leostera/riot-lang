use std::path::PathBuf;

use tinyml::{
    backend::js,
    backend::native,
    checker, lambda,
    parser::{self, lexer::Lexer},
};

fn source(path: &str) -> &'static str {
    Box::leak(std::fs::read_to_string(path).expect(path).into_boxed_str())
}

fn lex_snapshot(path: &str) -> String {
    let src = source(path);
    let lexer = Lexer::new(src).expect("lex ok");
    format!("{:#?}", lexer.tokens())
}

fn parse_module(path: &str) -> parser::Ast {
    parser::parse(source(path)).expect("parse ok")
}

fn parse_snapshot(path: &str) -> String {
    let module = parse_module(path);
    format!("{module:#?}")
}

fn check_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let summary = checker::check_module(&module);
    format!("{summary:#?}")
}

fn lower_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let summary = checker::check_module(&module);
    let lambda = lambda::translate(&summary.tir);
    format!("{lambda:#?}")
}

fn emit_js_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let summary = checker::check_module(&module);
    let lambda = lambda::translate(&summary.tir);
    let js = js::lower(&summary.interface, &lambda);
    js::emit_ts_string(&js)
}

fn nir_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let summary = checker::check_module(&module);
    let lambda = lambda::translate(&summary.tir);
    let nir = native::lower(&lambda).optimize();
    nir.to_string()
}

fn assert_no_native_unsupported(text: &str) {
    assert!(
        !text.to_ascii_lowercase().contains("unsupported"),
        "native output should not contain unsupported placeholders:\n{text}"
    );
}

fn emit_aarch64_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let summary = checker::check_module(&module);
    let lambda = lambda::translate(&summary.tir);
    let nir = native::lower(&lambda).optimize();
    let aarch64 = native::aarch64::select(&nir);
    aarch64.to_asm_string()
}

fn emit_wasm_snapshot(path: &str) -> String {
    let module = parse_module(path);
    let summary = checker::check_module(&module);
    let lambda = lambda::translate(&summary.tir);
    format!("emit pending\n{lambda:#?}")
}

macro_rules! assert_corpus_snapshot {
    ($value:expr) => {{
        let mut settings = insta::Settings::clone_current();
        settings
            .set_snapshot_path(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/snapshots"));
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
                fn nir() {
                    let snapshot = nir_snapshot($path);
                    assert_no_native_unsupported(&snapshot);
                    assert_corpus_snapshot!(snapshot);
                }

                #[test]
                fn emit_aarch64() {
                    let snapshot = emit_aarch64_snapshot($path);
                    assert_no_native_unsupported(&snapshot);
                    assert_corpus_snapshot!(snapshot);
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
