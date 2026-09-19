//! Module and expression typing.
//!
//! `Typer` is the main semantic checker for TinyML.  It translates parsed source
//! AST into typed IR (TIR), constructs the public module interface, and records
//! diagnostics for semantic errors that should be shown to users.
//!
//! The checker is split into a few explicit phases:
//!
//! 1. **Top-level pre-registration.**  All top-level values are registered with
//!    placeholder schemes before bodies are checked.  This gives same-module
//!    references stable entities and permits simple recursive/corecursive lookup
//!    shapes.
//! 2. **Declaration lowering.**  Use declarations become no-op TIR imports for
//!    now.  Type declarations become TIR type items plus interface metadata for
//!    constructors and record fields.  Let declarations infer expression bodies
//!    and update their placeholder schemes with generalized types.
//! 3. **Expression inference.**  Expressions produce TIR expressions annotated
//!    with checker `Type`s.  Typing constraints are sent to `Unifier`; errors are
//!    kept as diagnostics so the checker can continue producing best-effort TIR.
//! 4. **Pattern inference/binding.**  Patterns produce typed pattern trees and
//!    add any bound variables to the current value scope.

use std::collections::{HashMap, HashSet};

use crate::parser::{ast, ident::Ident};

use super::{
    builtin,
    diagnostic::CheckDiagnostic,
    env::{ConstructorBinding, EnvError, RecordFieldBinding, TypeBinding},
    interface::{
        ConstructorDescription, ModuleInterface, RecordFieldDescription, TypeDescription,
        ValueDescription,
    },
    scheme::Scheme,
    state::State,
    tir::{
        self, BindingId, EntityId, Expr, ExprKind, LetDecl, MatchArm, ModuleItem, Pattern,
        PatternKind, Type, TypeVarId,
    },
    unifier::Unifier,
};

/// Result of checking one source module.
///
/// The summary intentionally contains both the internal typed module and the
/// public interface.  Tests and backends can inspect the TIR for implementation
/// details, while tools that only need exported declarations can use the
/// interface.
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleSummary {
    pub tir: tir::Module,
    pub interface: ModuleInterface,
    pub diagnostics: Vec<CheckDiagnostic>,
}

/// Main module typer.
///
/// A `Typer` borrows mutable `State` for the duration of checking and applies
/// the source-language typing rules.  The environment inside `State` owns the
/// same-module symbol tables for values, types, constructors, and record fields.
pub struct Typer<'state> {
    state: &'state mut State,
}

impl<'state> Typer<'state> {
    /// Create a typer over an existing checker state.
    pub fn new(state: &'state mut State) -> Self {
        for &(name, arity) in builtin::TYPE_ARITIES {
            state.env_mut().add_type(name, TypeBinding { arity });
        }
        Self { state }
    }

    /// Typecheck a parsed module into TIR and a public interface.
    ///
    /// This method is intentionally tolerant: unsupported or mismatched pieces
    /// add diagnostics, but checking continues when possible so CLI commands can
    /// show multiple errors and `emit` can still produce early-stage artifacts.
    pub fn type_module(&mut self, module: &ast::Module) -> ModuleSummary {
        self.register_top_level_values(module);

        let mut items = Vec::new();
        let mut interface = ModuleInterface::empty();
        let mut checked_values = HashSet::new();
        self.register_type_arities(module);

        for item in &module.items {
            match item {
                ast::ModuleItem::LetDecl(let_decl) => {
                    let Some((item, value)) = self.check_let_decl(let_decl, &mut checked_values)
                    else {
                        continue;
                    };
                    items.push(ModuleItem::LetDecl(item));
                    interface.values.push(value);
                }
                ast::ModuleItem::TypeDecl(type_decl) => {
                    let (tir_decl, type_description, constructors, record_fields) =
                        self.lower_type_decl(type_decl);
                    items.push(ModuleItem::TypeDecl(tir_decl));
                    interface.types.push(type_description);
                    interface.constructors.extend(constructors);
                    interface.record_fields.extend(record_fields);
                }
                ast::ModuleItem::UseDecl(use_decl) => {
                    items.push(ModuleItem::UseDecl(match use_decl {
                        ast::UseDecl::Concrete {
                            module,
                            members,
                            span,
                        } => tir::UseDecl::Concrete {
                            module: module.clone(),
                            members: members.clone(),
                            span: *span,
                        },
                        ast::UseDecl::Wildcard { module, span } => tir::UseDecl::Wildcard {
                            module: module.clone(),
                            span: *span,
                        },
                    }));
                }
            }
        }

        ModuleSummary {
            tir: tir::Module { items },
            interface,
            diagnostics: self.state.diagnostics().to_vec(),
        }
    }

    fn register_type_arities(&mut self, module: &ast::Module) {
        for item in &module.items {
            if let ast::ModuleItem::TypeDecl(type_decl) = item {
                let Some(name) = type_decl.name.as_name() else {
                    continue;
                };
                if self
                    .state
                    .env_mut()
                    .add_type(
                        name.to_string(),
                        TypeBinding {
                            arity: type_decl.params.len(),
                        },
                    )
                    .is_some()
                {
                    self.state
                        .add_diagnostic(CheckDiagnostic::DuplicateTypeDeclaration {
                            name: name.to_string(),
                            span: type_decl.span,
                        });
                }
            }
        }
    }

    fn lower_type_expr_checked(&mut self, annotation: &ast::TypeExpr) -> Type {
        self.validate_type_expr(annotation);
        Type::from_type_expr(self.state, annotation)
    }

    fn validate_type_expr(&mut self, annotation: &ast::TypeExpr) {
        match annotation {
            ast::TypeExpr::App { name, args, span } => {
                if let Some(name_text) = name.as_name() {
                    match self.state.env().type_arity(name_text) {
                        Some(expected) if expected != args.len() => {
                            self.state
                                .add_diagnostic(CheckDiagnostic::TypeArityMismatch {
                                    name: name_text.to_string(),
                                    expected,
                                    actual: args.len(),
                                    span: *span,
                                });
                        }
                        Some(_) => {}
                        None => self.state.add_diagnostic(CheckDiagnostic::UnknownType {
                            name: name_text.to_string(),
                            span: *span,
                        }),
                    }
                }
                for arg in args {
                    self.validate_type_expr(arg);
                }
            }
            ast::TypeExpr::Fn { params, result, .. } => {
                for param in params {
                    self.validate_type_expr(param);
                }
                self.validate_type_expr(result);
            }
            ast::TypeExpr::Tuple { items, .. } => {
                for item in items {
                    self.validate_type_expr(item);
                }
            }
            ast::TypeExpr::Record { fields, .. } => {
                for (_, field) in fields {
                    self.validate_type_expr(field);
                }
            }
            ast::TypeExpr::Variant { variants, .. } => {
                for variant in variants {
                    match variant {
                        ast::Variant::TupleConstructor { args, .. } => {
                            for arg in args {
                                self.validate_type_expr(arg);
                            }
                        }
                        ast::Variant::RecordConstructor { fields, .. } => {
                            for (_, field) in fields {
                                self.validate_type_expr(field);
                            }
                        }
                    }
                }
            }
            ast::TypeExpr::Var { .. } => {}
        }
    }

    /// Lower one parsed type declaration into every checker-level view of that type.
    ///
    /// A source type declaration participates in the checker in three related forms:
    ///
    /// 1. a TIR item, so later pipeline snapshots/stages can still see the declared type;
    /// 2. public `ModuleInterface` metadata, so backends can describe exported types,
    ///    constructors, and record fields without re-reading source syntax; and
    /// 3. environment metadata in `State`, so same-file constructor expressions
    ///    and patterns can be typed against the declared constructor payloads.
    ///
    /// This function builds all three at once because they must agree on constructor
    /// tags, field order, and the declared result type. Keeping that work in one place
    /// prevents the common bug where the checker and backend interfaces disagree about
    /// whether, for example, the second field of `User { name, age }` is index `1` or
    /// has a different payload type.
    fn lower_type_decl(
        &mut self,
        type_decl: &ast::TypeDecl,
    ) -> (
        tir::TypeDecl,
        TypeDescription,
        Vec<ConstructorDescription>,
        Vec<RecordFieldDescription>,
    ) {
        // Type parameters are scoped only over this declaration.  We install them in
        // `State` before lowering the body so every occurrence of the same source
        // parameter (`'a`) resolves to the same checker type variable.  This mirrors
        // how expression inference uses scoped value environments: enter the scope,
        // lower everything that can mention the names, then restore the previous map.
        let mut type_params = HashMap::new();
        let mut seen_params = HashSet::new();
        let mut quantifier = Vec::new();
        for (index, param) in type_decl.params.iter().enumerate() {
            if !seen_params.insert(param.name.clone()) {
                self.state
                    .add_diagnostic(CheckDiagnostic::DuplicateTypeParameter {
                        name: param.name.clone(),
                        span: param.span,
                    });
            }
            let id = TypeVarId::new(index);
            quantifier.push(id);
            type_params.insert(param.name.clone(), Type::Generic(id));
        }

        self.validate_type_expr(&type_decl.body);
        let (body, constructors, record_fields) =
            self.state.with_type_params(type_params, |state| {
                // The declared result is the nominal type being introduced, e.g.
                // `Option<'a>` or `User`.  Constructors all produce this result; their
                // payload fields become the parameters of the constructor scheme.
                let result = type_result_type(&type_decl.name, &type_decl.params);

                // Preserve the type body in TIR/interface form.  For variants this is
                // currently a nominal `variant` marker from `Type::from_type_expr`, while
                // the detailed constructor payload metadata is recorded below.
                let body = Type::from_type_expr(state, &type_decl.body);
                let mut constructors = Vec::new();
                let mut record_fields = Vec::new();

                // TinyML type declarations currently lower interesting runtime data from
                // variant forms.  Each constructor receives a stable tag according to
                // declaration order; this is the same tag later used by native lowering
                // when variants become runtime blocks/immediates.
                if let ast::TypeExpr::Variant { variants, .. } = &type_decl.body {
                    for (tag, variant) in variants.iter().enumerate() {
                        match variant {
                            ast::Variant::TupleConstructor { ident, args, .. } => {
                                // Tuple constructors are represented as positional payloads.
                                // `Some('a)` becomes a constructor whose argument list contains
                                // exactly the lowered `'a` type.  A nullary constructor such as
                                // `None` therefore has an empty argument list and its scheme body
                                // is just the result type.
                                let arguments = args
                                    .iter()
                                    .map(|arg| Type::from_type_expr(state, arg))
                                    .collect::<Vec<_>>();
                                let scheme = Scheme {
                                    quantifier: quantifier.clone(),
                                    body: arrow_type(arguments.clone(), result.clone()),
                                };
                                constructors.push(ConstructorDescription {
                                    name: ident.clone(),
                                    scheme,
                                    owner: type_decl.name.clone(),
                                    tag: tag as u32,
                                    result: result.clone(),
                                    arguments,
                                });
                            }
                            ast::Variant::RecordConstructor { ident, fields, .. } => {
                                // Record constructors carry the same payload information as tuple
                                // constructors, plus stable field names/indices.  The positional
                                // `arguments` vector is still important because native lowering uses
                                // field order, while `record_fields` lets source-level field access and
                                // JS/TypeScript emission preserve names.
                                let mut arguments = Vec::new();
                                let mut field_names = Vec::new();
                                let mut seen_fields = HashSet::new();
                                for (index, (field_name, field_ty)) in fields.iter().enumerate() {
                                    if let Some(field_text) = field_name.as_name()
                                        && !seen_fields.insert(field_text.to_string())
                                    {
                                        state.add_diagnostic(
                                            CheckDiagnostic::DuplicateRecordField {
                                                name: field_text.to_string(),
                                                span: field_name.span(),
                                            },
                                        );
                                    }
                                    let field = Type::from_type_expr(state, field_ty);
                                    arguments.push(field.clone());
                                    field_names.push(field_name.clone());
                                    record_fields.push(RecordFieldDescription {
                                        name: field_name.clone(),
                                        owner: result.clone(),
                                        field,
                                        index,
                                    });
                                }
                                let scheme = Scheme {
                                    quantifier: quantifier.clone(),
                                    body: arrow_type(arguments.clone(), result.clone()),
                                };
                                constructors.push(ConstructorDescription {
                                    name: ident.clone(),
                                    scheme,
                                    owner: type_decl.name.clone(),
                                    tag: tag as u32,
                                    result: result.clone(),
                                    arguments,
                                });
                                if let Some(name) = ident.as_name() {
                                    // Keep record constructor field order for record expressions/patterns.
                                    // Constructor metadata is inserted after this closure returns.
                                    let _ = (name, field_names);
                                }
                            }
                        }
                    }
                }
                (body, constructors, record_fields)
            });

        // Publish same-file constructor lookup metadata for expression/pattern typing.
        // This is intentionally separate from `ModuleInterface`: the interface is the
        // durable public summary, while these maps are a local checker acceleration used
        // to type constructor occurrences encountered later in the same module.
        for constructor in &constructors {
            let field_names = match &type_decl.body {
                ast::TypeExpr::Variant { variants, .. } => variants
                    .iter()
                    .find_map(|variant| match variant {
                        ast::Variant::RecordConstructor { ident, fields, .. }
                            if ident == &constructor.name =>
                        {
                            Some(fields.iter().map(|(name, _)| name.clone()).collect())
                        }
                        _ => None,
                    })
                    .unwrap_or_default(),
                _ => Vec::new(),
            };
            if let Some(name) = constructor.name.as_name() {
                if self.state.env().constructor(name).is_some() {
                    self.state
                        .add_diagnostic(CheckDiagnostic::DuplicateConstructor {
                            name: name.to_string(),
                            span: constructor.name.span(),
                        });
                }
                self.state.env_mut().add_constructor(
                    name.to_string(),
                    ConstructorBinding {
                        scheme: constructor.scheme.clone(),
                        field_names,
                        result_type: type_name_key(&constructor.result).unwrap_or_else(|| {
                            constructor.owner.as_name().unwrap_or("").to_string()
                        }),
                    },
                );
            }
        }
        // Record fields are also made available by source name.  When a later record
        // pattern/expression mentions a declared field, the checker can recover both
        // its expected type and its positional index in the runtime block layout.
        for field in &record_fields {
            if let Some(name) = field.name.as_name() {
                self.state.env_mut().add_record_field(
                    name.to_string(),
                    RecordFieldBinding {
                        description: field.clone(),
                    },
                );
            }
        }

        // Finally build the TIR/interface type declaration.  These use the same lowered
        // body and source parameters, so snapshots and downstream consumers observe the
        // same type declaration the checker used to populate constructor metadata.
        let tir_decl = tir::TypeDecl {
            name: type_decl.name.clone(),
            params: type_decl.params.clone(),
            body: body.clone(),
            span: type_decl.span,
        };
        let type_description = TypeDescription {
            name: type_decl.name.clone(),
            params: type_decl.params.clone(),
            body,
        };
        (tir_decl, type_description, constructors, record_fields)
    }

    /// Register all top-level value names before checking bodies.
    ///
    /// This pre-pass allocates stable value entities and placeholder schemes for
    /// every top-level `let`.  Body inference later updates each placeholder with
    /// the generalized inferred type.  Doing this before checking bodies lets
    /// references resolve by entity instead of by ad-hoc source-name strings.
    ///
    /// TinyML module items are corecursive. This first pass gives every
    /// top-level `let` a binding, entity, and placeholder type before the
    /// second pass checks right-hand sides and unifies them with placeholders.
    fn register_top_level_values(&mut self, module: &ast::Module) {
        for item in &module.items {
            if let ast::ModuleItem::LetDecl(let_decl) = item {
                let name = match let_decl {
                    ast::LetDecl::Const { name, .. } | ast::LetDecl::Fn { name, .. } => name,
                };
                let Some(name_text) = name.as_name() else {
                    self.state.add_diagnostic(
                        CheckDiagnostic::UnsupportedQualifiedTopLevelConstant {
                            span: let_decl_span(let_decl),
                        },
                    );
                    continue;
                };
                let placeholder = Scheme::monomorphic(self.state.fresh_var());
                if let Err(err) = self.state.add_value(name_text, placeholder) {
                    match err {
                        EnvError::DuplicateValue { name } => {
                            self.state
                                .add_diagnostic(CheckDiagnostic::DuplicateTopLevelValue {
                                    name,
                                    span: let_decl_span(let_decl),
                                })
                        }
                        EnvError::CannotPopRootScope => unreachable!("registration does not pop"),
                    }
                }
            }
        }
    }

    fn instantiate_constructor(&mut self, info: &ConstructorBinding) -> (Vec<Type>, Type) {
        let instantiated = self.state.instantiate(&info.scheme);
        split_arrow_type(instantiated)
    }

    /// Check one top-level `let` declaration.
    ///
    /// Constants and function declarations share the same high-level flow:
    /// resolve the pre-registered placeholder symbol, infer the declaration's
    /// body, unify the inferred type with the placeholder, then generalize and
    /// publish the resulting scheme.  Function declarations additionally create
    /// a parameter scope and synthesize a curried arrow type from parameter
    /// patterns to the body type.
    fn check_let_decl(
        &mut self,
        let_decl: &ast::LetDecl,
        checked_values: &mut HashSet<String>,
    ) -> Option<(LetDecl, ValueDescription)> {
        match let_decl {
            ast::LetDecl::Const { name, body, span } => {
                let Some(name_text) = name.as_name() else {
                    self.state.add_diagnostic(
                        CheckDiagnostic::UnsupportedQualifiedTopLevelConstant { span: *span },
                    );
                    return None;
                };
                if !checked_values.insert(name_text.to_string()) {
                    return None;
                }
                let Some(symbol) = self.state.get_value(name_text).cloned() else {
                    return None;
                };
                let base_level = self.state.current_level();
                let value = self.infer_expression(body);
                let resolved = {
                    let mut unifier = Unifier::new(self.state);
                    unifier
                        .resolve(&value.ty)
                        .unwrap_or_else(|_| value.ty.clone())
                };
                let scheme = self.state.generalize(resolved, base_level);
                self.state
                    .update_value_scheme(symbol.entity.clone(), scheme.clone());
                Some((
                    LetDecl::Const {
                        binding: symbol.binding.clone(),
                        entity: symbol.entity.clone(),
                        name: name.clone(),
                        hint: None,
                        body: value,
                        scheme: scheme.clone(),
                        span: *span,
                    },
                    ValueDescription {
                        name: name.clone(),
                        scheme,
                    },
                ))
            }
            ast::LetDecl::Fn {
                name,
                args,
                body,
                span,
            } => {
                let Some(name_text) = name.as_name() else {
                    self.state.add_diagnostic(
                        CheckDiagnostic::UnsupportedQualifiedTopLevelConstant { span: *span },
                    );
                    return None;
                };
                if !checked_values.insert(name_text.to_string()) {
                    return None;
                }
                let Some(symbol) = self.state.get_value(name_text).cloned() else {
                    return None;
                };
                let base_level = self.state.current_level();
                self.state.push_scope();
                let params = args
                    .iter()
                    .map(|param| self.infer_pattern_and_bind(param))
                    .collect::<Vec<_>>();
                let body = self.infer_expression(body);
                self.state.pop_scope().expect("function scope pop");
                let fn_ty = arrow_type(
                    params.iter().map(|param| param.ty.clone()).collect(),
                    body.ty.clone(),
                );
                let resolved = {
                    let mut unifier = Unifier::new(self.state);
                    unifier.resolve(&fn_ty).unwrap_or(fn_ty)
                };
                let scheme = self.state.generalize(resolved, base_level);
                self.state
                    .update_value_scheme(symbol.entity.clone(), scheme.clone());
                Some((
                    LetDecl::Fn {
                        binding: symbol.binding.clone(),
                        entity: symbol.entity.clone(),
                        name: name.clone(),
                        args: params,
                        body,
                        scheme: scheme.clone(),
                        span: *span,
                    },
                    ValueDescription {
                        name: name.clone(),
                        scheme,
                    },
                ))
            }
        }
    }

    /// Infer a source expression and produce a typed TIR expression.
    ///
    /// This is syntax-directed Hindley-Milner inference.  It recursively infers
    /// children, emits unification constraints for constructs that require type
    /// equality, and returns a TIR node carrying the resulting type.  The method
    /// records diagnostics but usually still returns a best-effort expression so
    /// later compiler stages and snapshots can continue to run.
    pub fn infer_expression(&mut self, expr: &ast::Expr) -> Expr {
        match expr {
            ast::Expr::Literal { value, span } => Expr {
                kind: ExprKind::Literal {
                    value: value.clone(),
                },
                ty: literal_type(value),
                span: *span,
            },
            ast::Expr::Constructor { name, args, span } if is_unit_constructor(name, args) => {
                Expr {
                    kind: ExprKind::Constructor {
                        name: name.clone(),
                        entity: EntityId::value(BindingId::new(name.clone(), 0)),
                        args: Vec::new(),
                    },
                    ty: builtin::unit(),
                    span: *span,
                }
            }
            ast::Expr::Constructor { name, args, span } => {
                let args = args
                    .iter()
                    .map(|arg| self.infer_expression(arg))
                    .collect::<Vec<_>>();
                let info = name
                    .as_name()
                    .and_then(|name| self.state.env().constructor(name).cloned());
                let ty = if let Some(info) = &info {
                    let (expected_args, result) = self.instantiate_constructor(info);
                    for (arg, expected) in args.iter().zip(&expected_args) {
                        let unify_result = {
                            let mut unifier = Unifier::new(self.state);
                            unifier.unify(&arg.ty, expected)
                        };
                        if let Err(error) = unify_result {
                            self.state.add_diagnostic(CheckDiagnostic::TypeMismatch {
                                error,
                                span: arg.span,
                            });
                        }
                    }
                    result
                } else {
                    if let Some(name) = name.as_name() {
                        self.state
                            .add_diagnostic(CheckDiagnostic::UnknownConstructor {
                                name: name.to_string(),
                                span: *span,
                            });
                    }
                    self.state.fresh_var()
                };
                Expr {
                    ty,
                    kind: ExprKind::Constructor {
                        name: name.clone(),
                        entity: EntityId::value(BindingId::new(name.clone(), 0)),
                        args,
                    },
                    span: *span,
                }
            }
            ast::Expr::Var { name, span } => {
                let Some(name_text) = name.as_name() else {
                    self.state.add_diagnostic(CheckDiagnostic::UnknownValue {
                        name: format!("{name:?}"),
                        span: *span,
                    });
                    return Expr {
                        ty: self.state.fresh_var(),
                        kind: ExprKind::Var {
                            name: name.clone(),
                            entity: EntityId::value(BindingId::new(name.clone(), 0)),
                        },
                        span: *span,
                    };
                };
                let Some(symbol) = self.state.get_value(name_text).cloned() else {
                    self.state.add_diagnostic(CheckDiagnostic::UnknownValue {
                        name: name_text.to_string(),
                        span: *span,
                    });
                    return Expr {
                        ty: self.state.fresh_var(),
                        kind: ExprKind::Var {
                            name: name.clone(),
                            entity: EntityId::value(BindingId::new(name.clone(), 0)),
                        },
                        span: *span,
                    };
                };
                Expr {
                    ty: self.state.instantiate(&symbol.scheme),
                    kind: ExprKind::Var {
                        name: name.clone(),
                        entity: symbol.entity,
                    },
                    span: *span,
                }
            }
            ast::Expr::Fn { params, body, span } => {
                self.state.push_scope();
                let params = params
                    .iter()
                    .map(|param| self.infer_pattern_and_bind(param))
                    .collect::<Vec<_>>();
                let body = Box::new(self.infer_expression(body));
                self.state
                    .pop_scope()
                    .expect("function expression scope pop");
                let ty = arrow_type(
                    params.iter().map(|param| param.ty.clone()).collect(),
                    body.ty.clone(),
                );
                Expr {
                    ty,
                    kind: ExprKind::Fn { params, body },
                    span: *span,
                }
            }
            ast::Expr::Let {
                bind,
                hint,
                body,
                span,
            } => {
                let base_level = self.state.current_level();
                let value = self.infer_expression(body);
                let mut bind = self.infer_pattern(bind);
                let hint_ty = hint
                    .as_ref()
                    .map(|annotation| self.lower_type_expr_checked(annotation));
                if let Some(hint) = &hint_ty {
                    bind.ty = hint.clone();
                    let unify_result = {
                        let mut unifier = Unifier::new(self.state);
                        unifier.unify(&bind.ty, &value.ty)
                    };
                    if let Err(error) = unify_result {
                        self.state
                            .add_diagnostic(CheckDiagnostic::TypeMismatch { error, span: *span });
                    }
                } else if matches!(bind.kind, PatternKind::Var { .. }) {
                    bind.ty = value.ty.clone();
                } else {
                    let unify_result = {
                        let mut unifier = Unifier::new(self.state);
                        unifier.unify(&bind.ty, &value.ty)
                    };
                    if let Err(error) = unify_result {
                        self.state
                            .add_diagnostic(CheckDiagnostic::TypeMismatch { error, span: *span });
                    }
                }
                if let PatternKind::Var { name, binding } = &mut bind.kind {
                    if let Some(name_text) = name.as_name() {
                        let scheme = self.state.generalize(bind.ty.clone(), base_level);
                        if let Ok(symbol) = self.state.add_value(name_text, scheme) {
                            *binding = symbol.binding;
                        }
                    }
                } else {
                    self.bind_tir_pattern(&mut bind);
                }
                Expr {
                    ty: value.ty.clone(),
                    kind: ExprKind::Let {
                        bind,
                        hint: hint_ty,
                        body: Box::new(value),
                    },
                    span: *span,
                }
            }
            ast::Expr::Apply { callee, args, span } => {
                let callee = Box::new(self.infer_expression(callee));
                let args = args
                    .iter()
                    .map(|arg| self.infer_expression(arg))
                    .collect::<Vec<_>>();
                let mut callee_ty = callee.ty.clone();
                let mut result_ty = self.state.fresh_var();
                for arg in &args {
                    result_ty = self.state.fresh_var();
                    let expected = Type::Arrow {
                        parameter: Box::new(arg.ty.clone()),
                        result: Box::new(result_ty.clone()),
                    };
                    let unify_result = {
                        let mut unifier = Unifier::new(self.state);
                        unifier.unify(&callee_ty, &expected)
                    };
                    if let Err(error) = unify_result {
                        self.state
                            .add_diagnostic(CheckDiagnostic::TypeMismatch { error, span: *span });
                    }
                    callee_ty = result_ty.clone();
                }
                Expr {
                    ty: result_ty,
                    kind: ExprKind::Apply { callee, args },
                    span: *span,
                }
            }
            ast::Expr::BinaryOp {
                ident,
                left,
                right,
                span,
            } => {
                let left = Box::new(self.infer_expression(left));
                let right = Box::new(self.infer_expression(right));
                let string_addition = ident.as_name() == Some("+")
                    && left.ty == builtin::string()
                    && right.ty == builtin::string();
                let operand_ty = if string_addition {
                    builtin::string()
                } else {
                    builtin::i32()
                };
                for operand in [&left.ty, &right.ty] {
                    let unify_result = {
                        let mut unifier = Unifier::new(self.state);
                        unifier.unify(operand, &operand_ty)
                    };
                    if let Err(error) = unify_result {
                        self.state
                            .add_diagnostic(CheckDiagnostic::TypeMismatch { error, span: *span });
                    }
                }
                let result_ty = match ident.as_name() {
                    Some("==" | "!=" | "<" | "<=" | ">" | ">=") => builtin::bool(),
                    _ if string_addition => builtin::string(),
                    _ => builtin::i32(),
                };
                Expr {
                    ty: result_ty,
                    kind: ExprKind::BinaryOp {
                        ident: ident.clone(),
                        left,
                        right,
                    },
                    span: *span,
                }
            }
            ast::Expr::TypeHint { expr, hint, span } => {
                let expr = Box::new(self.infer_expression(expr));
                let hint = self.lower_type_expr_checked(hint);
                let unify_result = {
                    let mut unifier = Unifier::new(self.state);
                    unifier.unify(&hint, &expr.ty)
                };
                if let Err(error) = unify_result {
                    self.state.add_diagnostic(CheckDiagnostic::TypeMismatch {
                        error,
                        span: expr.span,
                    });
                }
                Expr {
                    ty: hint.clone(),
                    kind: ExprKind::TypeHint { expr, hint },
                    span: *span,
                }
            }
            ast::Expr::Tuple { items, span } => {
                let items = items
                    .iter()
                    .map(|item| self.infer_expression(item))
                    .collect::<Vec<_>>();
                let ty = Type::Tuple(items.iter().map(|item| item.ty.clone()).collect());
                Expr {
                    ty,
                    kind: ExprKind::Tuple { items },
                    span: *span,
                }
            }
            ast::Expr::Record { name, fields, span } => {
                let fields = fields
                    .iter()
                    .map(|(name, expr)| (name.clone(), self.infer_expression(expr)))
                    .collect::<Vec<_>>();
                let info = name
                    .as_name()
                    .and_then(|name| self.state.env().constructor(name).cloned());
                let ty = if let Some(info) = &info {
                    let (expected_args, result) = self.instantiate_constructor(info);
                    let provided = fields
                        .iter()
                        .filter_map(|(field_name, _)| field_name.as_name().map(str::to_string))
                        .collect::<HashSet<_>>();
                    for expected_name in &info.field_names {
                        if let Some(name) = expected_name.as_name()
                            && !provided.contains(name)
                        {
                            self.state.add_diagnostic(CheckDiagnostic::MissingField {
                                name: name.to_string(),
                                span: *span,
                            });
                        }
                    }
                    for (field_name, expr) in &fields {
                        if let Some(index) =
                            info.field_names.iter().position(|item| item == field_name)
                        {
                            if let Some(expected) = expected_args.get(index) {
                                let unify_result = {
                                    let mut unifier = Unifier::new(self.state);
                                    unifier.unify(&expr.ty, expected)
                                };
                                if let Err(error) = unify_result {
                                    self.state.add_diagnostic(CheckDiagnostic::TypeMismatch {
                                        error,
                                        span: expr.span,
                                    });
                                }
                            }
                        } else if let Some(name) = field_name.as_name() {
                            self.state.add_diagnostic(CheckDiagnostic::UnknownField {
                                name: name.to_string(),
                                span: field_name.span(),
                            });
                        }
                    }
                    result
                } else {
                    Type::Apply {
                        ident: name.clone(),
                        arguments: Vec::new(),
                    }
                };
                Expr {
                    ty,
                    kind: ExprKind::Record {
                        name: name.clone(),
                        fields,
                    },
                    span: *span,
                }
            }
            ast::Expr::Block { exprs, span } => {
                self.state.push_scope();
                let exprs = exprs
                    .iter()
                    .map(|expr| self.infer_expression(expr))
                    .collect::<Vec<_>>();
                self.state.pop_scope().expect("block scope pop");
                let ty = exprs
                    .last()
                    .map(|expr| expr.ty.clone())
                    .unwrap_or_else(builtin::unit);
                Expr {
                    ty,
                    kind: ExprKind::Block { exprs },
                    span: *span,
                }
            }
            ast::Expr::Match {
                scrutinee,
                arms,
                span,
            } => {
                let scrutinee = Box::new(self.infer_expression(scrutinee));
                let result_ty = self.state.fresh_var();
                let mut seen_arms = HashSet::new();
                let arms: Vec<MatchArm> = arms
                    .iter()
                    .map(|arm| {
                        if let Some(key) = simple_pattern_key(&arm.pattern)
                            && !seen_arms.insert(key)
                        {
                            self.state
                                .add_diagnostic(CheckDiagnostic::RedundantMatchArm {
                                    span: arm.pattern.span(),
                                });
                        }
                        self.state.push_scope();
                        let pattern = self.infer_pattern_and_bind(&arm.pattern);
                        let unify_result = {
                            let mut unifier = Unifier::new(self.state);
                            unifier.unify(&pattern.ty, &scrutinee.ty)
                        };
                        if let Err(error) = unify_result {
                            self.state.add_diagnostic(CheckDiagnostic::TypeMismatch {
                                error,
                                span: arm.pattern.span(),
                            });
                        }
                        let body = self.infer_expression(&arm.body);
                        let unify_result = {
                            let mut unifier = Unifier::new(self.state);
                            unifier.unify(&result_ty, &body.ty)
                        };
                        if let Err(error) = unify_result {
                            self.state.add_diagnostic(CheckDiagnostic::TypeMismatch {
                                error,
                                span: arm.body.span(),
                            });
                        }
                        self.state.pop_scope().expect("match arm scope pop");
                        MatchArm {
                            pattern,
                            body,
                            span: arm.span,
                        }
                    })
                    .collect();
                let resolved_scrutinee_ty = {
                    let mut unifier = Unifier::new(self.state);
                    unifier
                        .resolve(&scrutinee.ty)
                        .unwrap_or_else(|_| scrutinee.ty.clone())
                };
                if let Some(result_name) = type_name_key(&resolved_scrutinee_ty)
                    && let Some(expected_ctors) =
                        self.state.env().constructors_for_result(&result_name)
                    && !arms.iter().any(|arm| {
                        matches!(
                            arm.pattern.kind,
                            PatternKind::Wildcard | PatternKind::Var { .. }
                        )
                    })
                {
                    let seen = arms
                        .iter()
                        .filter_map(|arm| match &arm.pattern.kind {
                            PatternKind::Constructor { name, .. } => {
                                name.as_name().map(str::to_string)
                            }
                            _ => None,
                        })
                        .collect::<HashSet<_>>();
                    let missing = expected_ctors
                        .iter()
                        .filter(|ctor| !seen.contains(*ctor))
                        .cloned()
                        .collect::<Vec<_>>();
                    for name in missing {
                        self.state
                            .add_diagnostic(CheckDiagnostic::MissingConstructor {
                                name,
                                span: *span,
                            });
                    }
                }
                Expr {
                    ty: result_ty,
                    kind: ExprKind::Match { scrutinee, arms },
                    span: *span,
                }
            }
        }
    }

    /// Infer a pattern and immediately bind any variables it introduces.
    ///
    /// This helper is used in scopes where the pattern should make names visible
    /// to a following expression, such as function parameters and match arms.
    fn infer_pattern_and_bind(&mut self, pattern: &ast::Pattern) -> Pattern {
        let mut pattern = self.infer_pattern(pattern);
        self.bind_tir_pattern(&mut pattern);
        pattern
    }

    /// Add variables introduced by an already-typed pattern to the current scope.
    ///
    /// Binding happens after type inference so every inserted variable gets the
    /// pattern type chosen by annotations, constructor metadata, tuple structure,
    /// or fresh inference variables.
    fn bind_tir_pattern(&mut self, pattern: &mut Pattern) {
        match &mut pattern.kind {
            PatternKind::Var { name, binding } => {
                if let Some(name_text) = name.as_name()
                    && let Ok(symbol) = self
                        .state
                        .add_value(name_text, Scheme::monomorphic(pattern.ty.clone()))
                {
                    *binding = symbol.binding;
                }
            }
            PatternKind::Tuple { items } => items
                .iter_mut()
                .for_each(|item| self.bind_tir_pattern(item)),
            PatternKind::TypeHint { pattern, .. } => self.bind_tir_pattern(pattern),
            PatternKind::Record { fields, .. } => fields
                .iter_mut()
                .for_each(|(_, pattern)| self.bind_tir_pattern(pattern)),
            PatternKind::Constructor { args, .. } => {
                args.iter_mut().for_each(|arg| self.bind_tir_pattern(arg))
            }
            PatternKind::Wildcard | PatternKind::Literal { .. } => {}
        }
    }

    /// Infer the type of a source pattern without mutating the value scope.
    ///
    /// Constructor and record patterns consult same-file declaration metadata when
    /// available.  Variable patterns allocate fresh types here; `bind_tir_pattern`
    /// later publishes those variables to the value environment.
    fn infer_pattern(&mut self, pattern: &ast::Pattern) -> Pattern {
        match pattern {
            ast::Pattern::Wildcard { span } => Pattern {
                ty: self.state.fresh_var(),
                kind: PatternKind::Wildcard,
                span: *span,
            },
            ast::Pattern::Var { name, span } => Pattern {
                ty: self.state.fresh_var(),
                kind: PatternKind::Var {
                    name: name.clone(),
                    binding: BindingId::new(name.clone(), 0),
                },
                span: *span,
            },
            ast::Pattern::Literal { value, span } => Pattern {
                ty: literal_type(value),
                kind: PatternKind::Literal {
                    value: value.clone(),
                },
                span: *span,
            },
            ast::Pattern::Constructor { name, args, span }
                if args.is_empty() && matches!(name.as_name(), Some(builtin::UNIT_CONSTRUCTOR)) =>
            {
                Pattern {
                    ty: builtin::unit(),
                    kind: PatternKind::Constructor {
                        name: name.clone(),
                        entity: EntityId::value(BindingId::new(name.clone(), 0)),
                        args: Vec::new(),
                    },
                    span: *span,
                }
            }
            ast::Pattern::Constructor { name, args, span } => {
                let mut args = args
                    .iter()
                    .map(|arg| self.infer_pattern(arg))
                    .collect::<Vec<_>>();
                let info = name
                    .as_name()
                    .and_then(|name| self.state.env().constructor(name).cloned());
                let ty = if let Some(info) = &info {
                    let (expected_args, result) = self.instantiate_constructor(info);
                    for (arg, expected) in args.iter_mut().zip(&expected_args) {
                        let unify_result = {
                            let mut unifier = Unifier::new(self.state);
                            unifier.unify(&arg.ty, expected)
                        };
                        if let Err(error) = unify_result {
                            self.state.add_diagnostic(CheckDiagnostic::TypeMismatch {
                                error,
                                span: arg.span,
                            });
                        }
                        arg.ty = expected.clone();
                    }
                    result
                } else {
                    if let Some(name) = name.as_name() {
                        self.state
                            .add_diagnostic(CheckDiagnostic::UnknownConstructor {
                                name: name.to_string(),
                                span: *span,
                            });
                    }
                    self.state.fresh_var()
                };
                Pattern {
                    ty,
                    kind: PatternKind::Constructor {
                        name: name.clone(),
                        entity: EntityId::value(BindingId::new(name.clone(), 0)),
                        args,
                    },
                    span: *span,
                }
            }
            ast::Pattern::Tuple { items, span } => {
                let items = items
                    .iter()
                    .map(|item| self.infer_pattern(item))
                    .collect::<Vec<_>>();
                Pattern {
                    ty: Type::Tuple(items.iter().map(|item| item.ty.clone()).collect()),
                    kind: PatternKind::Tuple { items },
                    span: *span,
                }
            }
            ast::Pattern::TypeHint {
                pattern,
                hint,
                span,
            } => {
                let pattern = Box::new(self.infer_pattern(pattern));
                let hint = self.lower_type_expr_checked(hint);
                Pattern {
                    ty: hint.clone(),
                    kind: PatternKind::TypeHint { pattern, hint },
                    span: *span,
                }
            }
            ast::Pattern::Record { name, fields, span } => {
                let mut fields = fields
                    .iter()
                    .map(|(name, pattern)| (name.clone(), self.infer_pattern(pattern)))
                    .collect::<Vec<_>>();
                let info = name
                    .as_name()
                    .and_then(|name| self.state.env().constructor(name).cloned());
                let ty = if let Some(info) = &info {
                    let (expected_args, result) = self.instantiate_constructor(info);
                    let provided = fields
                        .iter()
                        .filter_map(|(field_name, _)| field_name.as_name().map(str::to_string))
                        .collect::<HashSet<_>>();
                    for expected_name in &info.field_names {
                        if let Some(name) = expected_name.as_name()
                            && !provided.contains(name)
                        {
                            self.state.add_diagnostic(CheckDiagnostic::MissingField {
                                name: name.to_string(),
                                span: *span,
                            });
                        }
                    }
                    for (field_name, pattern) in &mut fields {
                        if let Some(index) =
                            info.field_names.iter().position(|item| item == field_name)
                        {
                            if let Some(expected) = expected_args.get(index) {
                                let unify_result = {
                                    let mut unifier = Unifier::new(self.state);
                                    unifier.unify(&pattern.ty, expected)
                                };
                                if let Err(error) = unify_result {
                                    self.state.add_diagnostic(CheckDiagnostic::TypeMismatch {
                                        error,
                                        span: pattern.span,
                                    });
                                }
                                pattern.ty = expected.clone();
                            }
                        } else if let Some(name) = field_name.as_name() {
                            self.state.add_diagnostic(CheckDiagnostic::UnknownField {
                                name: name.to_string(),
                                span: field_name.span(),
                            });
                        }
                    }
                    result
                } else {
                    Type::Apply {
                        ident: name.clone(),
                        arguments: Vec::new(),
                    }
                };
                Pattern {
                    ty,
                    kind: PatternKind::Record {
                        name: name.clone(),
                        fields,
                    },
                    span: *span,
                }
            }
        }
    }
}

fn simple_pattern_key(pattern: &ast::Pattern) -> Option<String> {
    match pattern {
        ast::Pattern::Literal { value, .. } => Some(format!("literal:{value:?}")),
        ast::Pattern::Constructor { name, args, .. } if args.is_empty() => {
            name.as_name().map(|name| format!("ctor:{name}"))
        }
        ast::Pattern::Wildcard { .. } => Some("wildcard".to_string()),
        _ => None,
    }
}

fn type_name_key(ty: &Type) -> Option<String> {
    match ty {
        Type::Apply { ident, .. } => ident.as_name().map(str::to_string),
        _ => None,
    }
}

/// Build the nominal result type introduced by a type declaration.
///
/// `type Option<'a> = ...` introduces the result type `Option<'a>`.  During
/// declaration lowering those parameters are represented as generic variables so
/// constructor schemes can refer to them consistently.
fn type_result_type(name: &Ident, params: &[ast::TypeVar]) -> Type {
    Type::Apply {
        ident: name.clone(),
        arguments: params
            .iter()
            .enumerate()
            .map(|(index, _)| Type::Generic(TypeVarId::new(index)))
            .collect(),
    }
}

fn split_arrow_type(ty: Type) -> (Vec<Type>, Type) {
    let mut args = Vec::new();
    let mut current = ty;
    while let Type::Arrow { parameter, result } = current {
        args.push(*parameter);
        current = *result;
    }
    (args, current)
}

/// Build a curried arrow type from a parameter list and final result.
///
/// TinyML function and constructor types are curried in TIR: `a, b -> c` is
/// represented as `a -> b -> c`.
fn arrow_type(params: Vec<Type>, result: Type) -> Type {
    params
        .into_iter()
        .rev()
        .fold(result, |result, parameter| Type::Arrow {
            parameter: Box::new(parameter),
            result: Box::new(result),
        })
}

fn literal_type(value: &ast::Literal) -> tir::Type {
    match value {
        ast::Literal::Int(_) => builtin::i32(),
        ast::Literal::Float(_) => builtin::f64(),
        ast::Literal::String(_) => builtin::string(),
        ast::Literal::Bool(_) => builtin::bool(),
    }
}

fn is_unit_constructor(name: &Ident, args: &[ast::Expr]) -> bool {
    args.is_empty() && matches!(name.as_name(), Some(builtin::UNIT_CONSTRUCTOR))
}

fn let_decl_span(let_decl: &ast::LetDecl) -> crate::parser::lexer::Span {
    match let_decl {
        ast::LetDecl::Const { span, .. } | ast::LetDecl::Fn { span, .. } => *span,
    }
}
