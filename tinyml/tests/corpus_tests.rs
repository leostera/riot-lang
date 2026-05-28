use tinyml::parser::{lexer::Lexer, parse};

fn source(path: &str) -> &'static str {
    Box::leak(std::fs::read_to_string(path).expect(path).into_boxed_str())
}

fn lex_snapshot(path: &str) -> String {
    let src = source(path);
    let lexer = Lexer::from_str(src).expect("lex ok");
    format!("{:#?}", lexer.tokens())
}

fn parse_snapshot(path: &str) -> String {
    let src = source(path);
    let lexer = Lexer::from_str(src).expect("lex ok");
    let module = parse::parse_module(src, lexer).expect("parse ok");
    format!("{module:#?}")
}

fn check_snapshot(path: &str) -> String {
    let src = source(path);
    let lexer = Lexer::from_str(src).expect("lex ok");
    let module = parse::parse_module(src, lexer).expect("parse ok");
    format!("check pending\n{module:#?}")
}

fn emit_snapshot(path: &str) -> String {
    let src = source(path);
    let lexer = Lexer::from_str(src).expect("lex ok");
    let module = parse::parse_module(src, lexer).expect("parse ok");
    format!("emit pending\n{module:#?}")
}

fn build_snapshot(path: &str) -> String {
    let src = source(path);
    let lexer = Lexer::from_str(src).expect("lex ok");
    let module = parse::parse_module(src, lexer).expect("parse ok");
    format!("build pending\n{module:#?}")
}

macro_rules! corpus_tests {
    ($($name:ident => $path:literal),+ $(,)?) => {
        $(
            mod $name {
                use super::*;

                #[test]
                fn lex() {
                    insta::assert_snapshot!(lex_snapshot($path));
                }

                #[test]
                fn parse() {
                    insta::assert_snapshot!(parse_snapshot($path));
                }

                #[test]
                fn check() {
                    insta::assert_snapshot!(check_snapshot($path));
                }

                #[test]
                fn emit() {
                    insta::assert_snapshot!(emit_snapshot($path));
                }

                #[test]
                fn build() {
                    insta::assert_snapshot!(build_snapshot($path));
                }
            }
        )+
    };
}

include!(concat!(env!("OUT_DIR"), "/corpus_tests_generated.rs"));
