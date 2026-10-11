//! Public checker orchestration API.
//!
//! `Checker` owns mutable type-checking state and delegates typing work to the
//! typer, exposing `Checker::new().check_module(&module)` as the entrypoint.

use std::collections::HashSet;

use crate::parser::ast;

use super::{
    diagnostic::CheckDiagnostic,
    env::{ConstructorBinding, RecordFieldBinding, TypeBinding},
    interface::ModuleInterface,
    state::State,
    typer::Typer,
};

pub use super::typer::ModuleSummary;

pub struct Checker {
    state: State,
}

#[derive(Default)]
struct ImportedNames {
    values: HashSet<String>,
    types: HashSet<String>,
    constructors: HashSet<String>,
    fields: HashSet<String>,
}

fn selected_members(
    exports: &ModuleInterface,
    selected: Option<&[crate::parser::ident::Ident]>,
) -> Vec<crate::parser::ident::Ident> {
    if let Some(selected) = selected {
        return selected.to_vec();
    }

    let mut members = Vec::new();
    let mut seen = HashSet::new();
    for name in exports
        .values
        .iter()
        .map(|item| &item.name)
        .chain(exports.types.iter().map(|item| &item.name))
        .chain(exports.constructors.iter().map(|item| &item.name))
        .chain(exports.record_fields.iter().map(|item| &item.name))
    {
        if let Some(name_text) = name.as_name()
            && seen.insert(name_text.to_string())
        {
            members.push(name.clone());
        }
    }
    members
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

    /// Check a module against declarations loaded from compiled interfaces.
    pub fn check_module_with_interface(
        &mut self,
        module: &ast::Module,
        imported: &ModuleInterface,
    ) -> ModuleSummary {
        if imported.modules.is_empty() {
            self.import_flat_interface(imported);
        } else {
            self.import_selected_modules(module, imported);
        }
        Typer::new(&mut self.state).type_module(module)
    }

    fn import_flat_interface(&mut self, imported: &ModuleInterface) {
        for value in &imported.values {
            if let Some(name) = value.name.as_name() {
                let _ = self.state.add_value(name, value.scheme.clone());
            }
        }
        for ty in &imported.types {
            if let Some(name) = ty.name.as_name() {
                self.state.env_mut().add_type(
                    name.to_string(),
                    TypeBinding {
                        arity: ty.params.len(),
                    },
                );
            }
        }
        for constructor in &imported.constructors {
            if let Some(name) = constructor.name.as_name() {
                let result_type = type_name(&constructor.result)
                    .or_else(|| constructor.owner.as_name())
                    .unwrap_or("")
                    .to_string();
                self.state.env_mut().add_constructor(
                    name.to_string(),
                    ConstructorBinding {
                        scheme: constructor.scheme.clone(),
                        field_names: constructor.field_names.clone(),
                        result_type,
                    },
                );
            }
        }
        for field in &imported.record_fields {
            if let Some(name) = field.name.as_name() {
                self.state.env_mut().add_record_field(
                    name.to_string(),
                    RecordFieldBinding {
                        description: field.clone(),
                    },
                );
            }
        }
    }

    fn import_selected_modules(&mut self, module: &ast::Module, imported: &ModuleInterface) {
        let mut imported_names = ImportedNames::default();
        for item in &module.items {
            let ast::ModuleItem::UseDecl(use_decl) = item else {
                continue;
            };
            self.import_use_decl(use_decl, imported, &mut imported_names);
        }
    }

    fn import_use_decl(
        &mut self,
        use_decl: &ast::UseDecl,
        imported: &ModuleInterface,
        imported_names: &mut ImportedNames,
    ) {
        let (module_name, selected, span) = match use_decl {
            ast::UseDecl::Concrete {
                module,
                members,
                span,
            } => (module, Some(members.as_slice()), *span),
            ast::UseDecl::Wildcard { module, span } => (module, None, *span),
        };
        let Some(source) = imported
            .modules
            .iter()
            .find(|source| source.name.same_path(module_name))
        else {
            self.state.add_diagnostic(CheckDiagnostic::UnknownModule {
                name: ident_path(module_name),
                span,
            });
            return;
        };

        for member in selected_members(&source.interface, selected) {
            self.import_member(
                module_name,
                &source.interface,
                &member,
                span,
                imported_names,
            );
        }
    }

    fn import_member(
        &mut self,
        module_name: &crate::parser::ident::Ident,
        exports: &ModuleInterface,
        member: &crate::parser::ident::Ident,
        import_span: crate::parser::lexer::Span,
        imported_names: &mut ImportedNames,
    ) {
        let Some(name) = member.as_name() else {
            self.unknown_import_member(module_name, member);
            return;
        };

        let mut found = false;
        found |= self.import_values(exports, name, import_span, &mut imported_names.values);
        found |= self.import_types(exports, name, import_span, &mut imported_names.types);
        found |=
            self.import_constructors(exports, name, import_span, &mut imported_names.constructors);
        found |= self.import_record_fields(exports, name, import_span, &mut imported_names.fields);
        if !found {
            self.unknown_import_member(module_name, member);
        }
    }

    fn import_values(
        &mut self,
        exports: &ModuleInterface,
        name: &str,
        span: crate::parser::lexer::Span,
        imported_names: &mut HashSet<String>,
    ) -> bool {
        let values = exports
            .values
            .iter()
            .filter(|value| value.name.as_name() == Some(name))
            .map(|value| value.scheme.clone())
            .collect::<Vec<_>>();
        for scheme in &values {
            if !imported_names.insert(name.to_string()) {
                self.conflicting_import("value", name, span);
                continue;
            }
            if self.state.add_value(name, scheme.clone()).is_err() {
                self.conflicting_import("value", name, span);
            }
        }
        !values.is_empty()
    }

    fn import_types(
        &mut self,
        exports: &ModuleInterface,
        name: &str,
        span: crate::parser::lexer::Span,
        imported_names: &mut HashSet<String>,
    ) -> bool {
        let types = exports
            .types
            .iter()
            .filter(|ty| ty.name.as_name() == Some(name))
            .cloned()
            .collect::<Vec<_>>();
        for ty in &types {
            if !imported_names.insert(name.to_string()) {
                self.conflicting_import("type", name, span);
                continue;
            }
            if self
                .state
                .env_mut()
                .add_type(
                    name,
                    TypeBinding {
                        arity: ty.params.len(),
                    },
                )
                .is_some()
            {
                self.conflicting_import("type", name, span);
            }
            for constructor in exports
                .constructors
                .iter()
                .filter(|constructor| constructor.owner.same_path(&ty.name))
            {
                self.add_constructor_coverage(constructor);
            }
        }
        !types.is_empty()
    }

    fn import_constructors(
        &mut self,
        exports: &ModuleInterface,
        name: &str,
        span: crate::parser::lexer::Span,
        imported_names: &mut HashSet<String>,
    ) -> bool {
        let constructors = exports
            .constructors
            .iter()
            .filter(|constructor| constructor.name.as_name() == Some(name))
            .cloned()
            .collect::<Vec<_>>();
        for constructor in &constructors {
            if !imported_names.insert(name.to_string()) {
                self.conflicting_import("constructor", name, span);
                continue;
            }
            let result_type = type_name(&constructor.result)
                .or_else(|| constructor.owner.as_name())
                .unwrap_or("")
                .to_string();
            if self
                .state
                .env_mut()
                .add_constructor(
                    name,
                    ConstructorBinding {
                        scheme: constructor.scheme.clone(),
                        field_names: constructor.field_names.clone(),
                        result_type,
                    },
                )
                .is_some()
            {
                self.conflicting_import("constructor", name, span);
            }
            for sibling in exports
                .constructors
                .iter()
                .filter(|sibling| sibling.owner.same_path(&constructor.owner))
            {
                self.add_constructor_coverage(sibling);
            }
        }
        !constructors.is_empty()
    }

    fn import_record_fields(
        &mut self,
        exports: &ModuleInterface,
        name: &str,
        span: crate::parser::lexer::Span,
        imported_names: &mut HashSet<String>,
    ) -> bool {
        let fields = exports
            .record_fields
            .iter()
            .filter(|field| field.name.as_name() == Some(name))
            .cloned()
            .collect::<Vec<_>>();
        for field in &fields {
            if !imported_names.insert(name.to_string()) {
                self.conflicting_import("record field", name, span);
                continue;
            }
            if self
                .state
                .env_mut()
                .add_record_field(
                    name,
                    RecordFieldBinding {
                        description: field.clone(),
                    },
                )
                .is_some()
            {
                self.conflicting_import("record field", name, span);
            }
        }
        !fields.is_empty()
    }

    fn unknown_import_member(
        &mut self,
        module_name: &crate::parser::ident::Ident,
        member: &crate::parser::ident::Ident,
    ) {
        self.state
            .add_diagnostic(CheckDiagnostic::UnknownImportMember {
                module: ident_path(module_name),
                member: ident_path(member),
                span: member.span(),
            });
    }

    fn add_constructor_coverage(&mut self, constructor: &super::interface::ConstructorDescription) {
        let result_type = type_name(&constructor.result)
            .or_else(|| constructor.owner.as_name())
            .unwrap_or("");
        if let Some(name) = constructor.name.as_name() {
            self.state
                .env_mut()
                .add_constructor_coverage(name, result_type);
        }
    }

    fn conflicting_import(
        &mut self,
        namespace: &str,
        name: &str,
        span: crate::parser::lexer::Span,
    ) {
        self.state
            .add_diagnostic(CheckDiagnostic::ConflictingImport {
                namespace: namespace.to_string(),
                name: name.to_string(),
                span,
            });
    }

    pub fn check_module_with_imported_values(
        &mut self,
        module: &ast::Module,
        imported_values: &[(String, super::scheme::Scheme)],
    ) -> ModuleSummary {
        let interface = ModuleInterface {
            values: imported_values
                .iter()
                .map(|(name, scheme)| super::interface::ValueDescription {
                    name: crate::parser::ident::Ident::from_string(name),
                    scheme: scheme.clone(),
                })
                .collect(),
            ..ModuleInterface::empty()
        };
        self.check_module_with_interface(module, &interface)
    }
}

fn type_name(ty: &super::tir::Type) -> Option<&str> {
    fn ident_name(ident: &crate::parser::ident::Ident) -> Option<&str> {
        match ident {
            crate::parser::ident::Ident::Name { name, .. } => Some(name),
            crate::parser::ident::Ident::Qualified { rest, .. } => ident_name(rest),
        }
    }
    match ty {
        super::tir::Type::Apply { ident, .. } => ident_name(ident),
        _ => None,
    }
}

fn ident_path(ident: &crate::parser::ident::Ident) -> String {
    match ident {
        crate::parser::ident::Ident::Name { name, .. } => name.clone(),
        crate::parser::ident::Ident::Qualified { name, rest, .. } => {
            format!("{name}.{}", ident_path(rest))
        }
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
let enabled = true
let name = "Tiny"
let nothing = ()
"#,
        );
        let summary = Checker::new().check_module(&module);
        assert!(summary.diagnostics.is_empty());
        assert_eq!(summary.tir.items.len(), 4);
        assert_eq!(summary.interface.values.len(), 4);
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
