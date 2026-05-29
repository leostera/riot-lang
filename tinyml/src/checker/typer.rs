use std::collections::HashSet;

use crate::parser::{ast, ident::Ident};

use super::{
    builtin,
    diagnostic::CheckDiagnostic,
    env::EnvError,
    scheme::Scheme,
    state::State,
    tst::{
        self, BindingId, EntityId, Expr, ExprKind, LetDecl, MatchArm, ModuleInterface, ModuleItem,
        Pattern, PatternKind, Type, ValueDescription,
    },
    unifier::Unifier,
};

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleSummary {
    pub tst: tst::Module,
    pub interface: ModuleInterface,
    pub diagnostics: Vec<CheckDiagnostic>,
}

pub struct Typer<'state> {
    state: &'state mut State,
}

impl<'state> Typer<'state> {
    pub fn new(state: &'state mut State) -> Self {
        Self { state }
    }

    pub fn type_module(&mut self, module: &ast::Module) -> ModuleSummary {
        self.register_top_level_values(module);

        let mut items = Vec::new();
        let mut interface = ModuleInterface::empty();
        let mut checked_values = HashSet::new();

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
                ast::ModuleItem::TypeDecl(_) => {
                    self.state
                        .add_diagnostic(CheckDiagnostic::UnsupportedTypeDeclaration);
                }
                ast::ModuleItem::UseDecl(_) => {
                    self.state
                        .add_diagnostic(CheckDiagnostic::UnsupportedUseDeclaration);
                }
            }
        }

        ModuleSummary {
            tst: tst::Module { items },
            interface,
            diagnostics: self.state.diagnostics().to_vec(),
        }
    }

    /// Register all top-level constant names before checking bodies.
    ///
    /// TinyML module items are corecursive. This first pass gives every
    /// top-level `let` a binding, entity, and placeholder type before the
    /// second pass checks right-hand sides and unifies them with placeholders.
    fn register_top_level_values(&mut self, module: &ast::Module) {
        for item in &module.items {
            if let ast::ModuleItem::LetDecl(ast::LetDecl::Const { name, .. }) = item {
                let Some(name_text) = name.as_name() else {
                    self.state
                        .add_diagnostic(CheckDiagnostic::UnsupportedQualifiedTopLevelConstant);
                    continue;
                };
                let placeholder = Scheme::monomorphic(self.state.fresh_var());
                if let Err(err) = self.state.add_value(name_text, placeholder) {
                    match err {
                        EnvError::DuplicateValue { name } => self
                            .state
                            .add_diagnostic(CheckDiagnostic::DuplicateTopLevelValue { name }),
                        EnvError::CannotPopRootScope => unreachable!("registration does not pop"),
                    }
                }
            }
        }
    }

    fn check_let_decl(
        &mut self,
        let_decl: &ast::LetDecl,
        checked_values: &mut HashSet<String>,
    ) -> Option<(LetDecl, ValueDescription)> {
        let ast::LetDecl::Const { name, body, span } = let_decl else {
            self.state
                .add_diagnostic(CheckDiagnostic::UnsupportedTopLevelFunction);
            return None;
        };

        let Some(name_text) = name.as_name() else {
            self.state
                .add_diagnostic(CheckDiagnostic::UnsupportedQualifiedTopLevelConstant);
            return None;
        };
        if !checked_values.insert(name_text.to_string()) {
            return None;
        }
        let Some(symbol) = self.state.get_value(name_text).cloned() else {
            return None;
        };
        let value = self.infer_expression(body);
        let unify_result = {
            let mut unifier = Unifier::new(self.state);
            unifier.unify(&symbol.scheme.body, &value.ty)
        };
        if let Err(error) = unify_result {
            self.state
                .add_diagnostic(CheckDiagnostic::TypeMismatch { error });
            return None;
        }
        let resolved = {
            let mut unifier = Unifier::new(self.state);
            unifier
                .resolve(&symbol.scheme.body)
                .unwrap_or_else(|_| value.ty.clone())
        };
        let scheme = Scheme::monomorphic(resolved);
        self.state
            .update_value_scheme(symbol.entity, scheme.clone());
        Some((
            LetDecl::Const {
                binding: symbol.binding,
                entity: symbol.entity,
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
                        entity: EntityId::new(0),
                        args: Vec::new(),
                    },
                    ty: builtin::unit(),
                    span: *span,
                }
            }
            ast::Expr::Constructor { name, args, span } => {
                let args = args.iter().map(|arg| self.infer_expression(arg)).collect();
                self.unsupported_expr(
                    ExprKind::Constructor {
                        name: name.clone(),
                        entity: EntityId::new(0),
                        args,
                    },
                    *span,
                )
            }
            ast::Expr::Var { name, span } => self.unsupported_expr(
                ExprKind::Var {
                    name: name.clone(),
                    entity: EntityId::new(0),
                },
                *span,
            ),
            ast::Expr::Fn { params, body, span } => {
                let params = params
                    .iter()
                    .map(|param| self.infer_pattern(param))
                    .collect();
                let body = Box::new(self.infer_expression(body));
                self.unsupported_expr(ExprKind::Fn { params, body }, *span)
            }
            ast::Expr::Let {
                bind,
                hint,
                body,
                span,
            } => {
                let bind = self.infer_pattern(bind);
                let hint = hint
                    .as_ref()
                    .map(|annotation| Type::from_type_expr(self.state, annotation));
                let body = Box::new(self.infer_expression(body));
                self.unsupported_expr(ExprKind::Let { bind, hint, body }, *span)
            }
            ast::Expr::Apply { callee, args, span } => {
                let callee = Box::new(self.infer_expression(callee));
                let args = args.iter().map(|arg| self.infer_expression(arg)).collect();
                self.unsupported_expr(ExprKind::Apply { callee, args }, *span)
            }
            ast::Expr::BinaryOp {
                ident,
                left,
                right,
                span,
            } => {
                let left = Box::new(self.infer_expression(left));
                let right = Box::new(self.infer_expression(right));
                self.unsupported_expr(
                    ExprKind::BinaryOp {
                        ident: ident.clone(),
                        left,
                        right,
                    },
                    *span,
                )
            }
            ast::Expr::TypeHint { expr, hint, span } => {
                let expr = Box::new(self.infer_expression(expr));
                let hint = Type::from_type_expr(self.state, hint);
                let unify_result = {
                    let mut unifier = Unifier::new(self.state);
                    unifier.unify(&hint, &expr.ty)
                };
                if let Err(error) = unify_result {
                    self.state
                        .add_diagnostic(CheckDiagnostic::TypeMismatch { error });
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
                    .collect();
                self.unsupported_expr(
                    ExprKind::Record {
                        name: name.clone(),
                        fields,
                    },
                    *span,
                )
            }
            ast::Expr::Block { exprs, span } => {
                let exprs = exprs
                    .iter()
                    .map(|expr| self.infer_expression(expr))
                    .collect();
                self.unsupported_expr(ExprKind::Block { exprs }, *span)
            }
            ast::Expr::Match {
                scrutinee,
                arms,
                span,
            } => {
                let scrutinee = Box::new(self.infer_expression(scrutinee));
                let arms = arms
                    .iter()
                    .map(|arm| MatchArm {
                        pattern: self.infer_pattern(&arm.pattern),
                        body: self.infer_expression(&arm.body),
                        span: arm.span,
                    })
                    .collect();
                self.unsupported_expr(ExprKind::Match { scrutinee, arms }, *span)
            }
        }
    }

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
                    binding: BindingId::new(0),
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
            ast::Pattern::Constructor { name, args, span } => Pattern {
                ty: self.state.fresh_var(),
                kind: PatternKind::Constructor {
                    name: name.clone(),
                    entity: EntityId::new(0),
                    args: args.iter().map(|arg| self.infer_pattern(arg)).collect(),
                },
                span: *span,
            },
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
                let hint = Type::from_type_expr(self.state, hint);
                Pattern {
                    ty: hint.clone(),
                    kind: PatternKind::TypeHint { pattern, hint },
                    span: *span,
                }
            }
            ast::Pattern::Record { name, fields, span } => Pattern {
                ty: self.state.fresh_var(),
                kind: PatternKind::Record {
                    name: name.clone(),
                    fields: fields
                        .iter()
                        .map(|(name, pattern)| (name.clone(), self.infer_pattern(pattern)))
                        .collect(),
                },
                span: *span,
            },
        }
    }

    fn unsupported_expr(&mut self, kind: ExprKind, span: crate::parser::lexer::Span) -> Expr {
        self.state
            .add_diagnostic(CheckDiagnostic::UnsupportedExpression);
        Expr {
            ty: self.state.fresh_var(),
            kind,
            span,
        }
    }
}

fn literal_type(value: &ast::Literal) -> tst::Type {
    match value {
        ast::Literal::Int(_) => builtin::i32(),
        ast::Literal::Float(_) => builtin::f64(),
        ast::Literal::String(_) => builtin::string(),
        ast::Literal::Bool(_) => builtin::bool(),
    }
}

fn is_unit_constructor(name: &Ident, args: &[ast::Expr]) -> bool {
    args.is_empty() && matches!(name.as_name(), Some("()"))
}
