use crate::parser::ast;

use super::{state::State, typer::Typer};

pub use super::typer::ModuleSummary;

pub struct Checker {
    state: State,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            state: State::new(),
        }
    }

    pub fn check_module(&mut self, module: &ast::Module) -> ModuleSummary {
        Typer::new(&mut self.state).type_module(module)
    }
}

impl Default for Checker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{lexer::Lexer, parse::Parser};

    fn parse_module(src: &str) -> ast::Module {
        let lexer = Lexer::new(src).expect("lex ok");
        Parser::new(lexer).parse_module().expect("parse ok")
    }

    #[test]
    fn checks_successful_top_level_constants() {
        let module = parse_module(
            r#"
let answer = 42
let pi = 3.14
let enabled = true
let name = "Tiny"
let nothing = ()
"#,
        );
        let summary = Checker::new().check_module(&module);
        assert!(summary.diagnostics.is_empty());
        assert_eq!(summary.tst.items.len(), 5);
        assert_eq!(summary.interface.values.len(), 5);
    }

    #[test]
    fn reports_duplicate_top_level_constants() {
        let module = parse_module("let value = 1\nlet value = 2");
        let summary = Checker::new().check_module(&module);
        assert_eq!(summary.tst.items.len(), 1);
        assert_eq!(summary.interface.values.len(), 1);
        assert_eq!(
            summary.diagnostics[0],
            crate::checker::CheckDiagnostic::DuplicateTopLevelValue {
                name: "value".into()
            }
        );
    }

    #[test]
    fn rejects_top_level_functions() {
        let module = parse_module("let f x = x");
        let summary = Checker::new().check_module(&module);
        assert!(summary.tst.items.is_empty());
        assert!(summary.interface.values.is_empty());
        assert_eq!(
            summary.diagnostics[0],
            crate::checker::CheckDiagnostic::UnsupportedTopLevelFunction
        );
    }

    #[test]
    fn gives_unsupported_expressions_type_holes() {
        let module = parse_module("let value = answer");
        let summary = Checker::new().check_module(&module);
        assert_eq!(summary.tst.items.len(), 1);
        assert_eq!(summary.interface.values.len(), 1);
        assert_eq!(
            summary.diagnostics[0],
            crate::checker::CheckDiagnostic::UnsupportedExpression
        );
    }
}
