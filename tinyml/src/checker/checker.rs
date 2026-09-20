//! Public checker orchestration API.
//!
//! `Checker` owns mutable type-checking state and delegates typing work to the
//! typer, exposing `Checker::new().check_module(&module)` as the entrypoint.

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
        self.check_module_with_imported_values(module, &[])
    }

    /// Check a module with values supplied by previously compiled interfaces.
    ///
    /// Interface loading deliberately starts with opaque monomorphic holes. The
    /// source module can constrain imported functions at call sites while the
    /// artifact format is still kept tiny and textual.
    pub fn check_module_with_imported_values(
        &mut self,
        module: &ast::Module,
        imported_values: &[String],
    ) -> ModuleSummary {
        for name in imported_values {
            if !self.state.has_value(name) {
                let ty = self.state.fresh_var();
                let _ = self
                    .state
                    .add_value(name.clone(), super::scheme::Scheme::monomorphic(ty));
            }
        }
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
        assert_eq!(summary.tir.items.len(), 5);
        assert_eq!(summary.interface.values.len(), 5);
    }

    #[test]
    fn reports_duplicate_top_level_constants() {
        let module = parse_module("let value = 1\nlet value = 2");
        let summary = Checker::new().check_module(&module);
        assert_eq!(summary.tir.items.len(), 1);
        assert_eq!(summary.interface.values.len(), 1);
        assert!(matches!(
            &summary.diagnostics[0],
            crate::checker::CheckDiagnostic::DuplicateTopLevelValue { name, .. }
                if name == "value"
        ));
    }

    #[test]
    fn checks_top_level_functions() {
        let module = parse_module("let f x = x");
        let summary = Checker::new().check_module(&module);
        assert_eq!(summary.tir.items.len(), 1);
        assert_eq!(summary.interface.values.len(), 1);
        assert!(summary.diagnostics.is_empty());
    }

    #[test]
    fn infers_principal_identity_scheme() {
        let module = parse_module("let id x = x");
        let summary = Checker::new().check_module(&module);
        assert!(summary.diagnostics.is_empty());
        assert_eq!(summary.interface.values[0].scheme.quantifier.len(), 1);
    }

    #[test]
    fn infers_principal_const_scheme() {
        let module = parse_module("let const x = fn y -> x");
        let summary = Checker::new().check_module(&module);
        assert!(summary.diagnostics.is_empty());
        assert_eq!(summary.interface.values[0].scheme.quantifier.len(), 2);
    }

    #[test]
    fn constructor_scheme_instantiates_independently() {
        let module = parse_module(
            r#"
type Option<'a> =
  | None
  | Some('a)

let a = Some(1)
let b = Some("x")
"#,
        );
        let summary = Checker::new().check_module(&module);
        assert!(summary.diagnostics.is_empty(), "{:#?}", summary.diagnostics);
        assert_eq!(summary.interface.constructors.len(), 2);
    }

    #[test]
    fn polymorphic_local_let_can_be_reused_at_different_types() {
        let module = parse_module("let pair = { let id = fn x -> x; (id 1, id true) }");
        let summary = Checker::new().check_module(&module);
        assert!(summary.diagnostics.is_empty(), "{:#?}", summary.diagnostics);
    }

    #[test]
    fn gives_unknown_value_type_holes() {
        let module = parse_module("let value = answer");
        let summary = Checker::new().check_module(&module);
        assert_eq!(summary.tir.items.len(), 1);
        assert_eq!(summary.interface.values.len(), 1);
        assert!(matches!(
            summary.diagnostics[0],
            crate::checker::CheckDiagnostic::UnknownValue { .. }
        ));
    }
}
