use tinyml::parser::{lexer::Lexer, parse::Parser};

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
    format!("check pending\n{module:#?}")
}

fn emit_snapshot(path: &str) -> String {
    let module = parse_module(path);
    format!("emit pending\n{module:#?}")
}

fn build_snapshot(path: &str) -> String {
    let module = parse_module(path);
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
