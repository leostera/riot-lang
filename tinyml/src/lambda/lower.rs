//! TIR-to-Lambda lowering.
//!
//! This pass erases source-only type hints, lowers binary operators to typed
//! primitives, and preserves semantic type information for backend consumers.

use crate::checker::tir;

use super::ir;

pub fn lower_module(module: &tir::Module) -> ir::Module {
    Lowerer.lower_module(module)
}

pub struct Lowerer;

impl Lowerer {
    pub fn lower_module(&mut self, module: &tir::Module) -> ir::Module {
        let values = module
            .items
            .iter()
            .filter_map(|item| self.lower_module_item(item))
            .collect();
        ir::Module { values }
    }

    fn lower_module_item(&mut self, item: &tir::ModuleItem) -> Option<ir::ValueDecl> {
        match item {
            tir::ModuleItem::LetDecl(let_decl) => Some(self.lower_let_decl(let_decl)),
            tir::ModuleItem::UseDecl(_) | tir::ModuleItem::TypeDecl(_) => None,
        }
    }

    fn lower_let_decl(&mut self, let_decl: &tir::LetDecl) -> ir::ValueDecl {
        match let_decl {
            tir::LetDecl::Const {
                binding,
                entity,
                name,
                body,
                scheme,
                span,
                ..
            } => ir::ValueDecl {
                binding: binding.clone(),
                entity: entity.clone(),
                name: name.clone(),
                body: self.lower_expr(body),
                ty: scheme.body.clone(),
                span: *span,
            },
            tir::LetDecl::Fn {
                binding,
                entity,
                name,
                args,
                body,
                scheme,
                span,
            } => ir::ValueDecl {
                binding: binding.clone(),
                entity: entity.clone(),
                name: name.clone(),
                body: ir::Expr {
                    ty: scheme.body.clone(),
                    kind: ir::ExprKind::Function {
                        params: args.iter().map(|arg| self.lower_pattern(arg)).collect(),
                        body: Box::new(self.lower_expr(body)),
                    },
                    span: *span,
                },
                ty: scheme.body.clone(),
                span: *span,
            },
        }
    }

    fn lower_expr(&mut self, expr: &tir::Expr) -> ir::Expr {
        let kind = match &expr.kind {
            tir::ExprKind::Literal { value } => {
                ir::ExprKind::Constant(ir::Constant::Literal(value.clone()))
            }
            tir::ExprKind::Var { name, entity } => ir::ExprKind::Var {
                name: name.clone(),
                entity: entity.clone(),
            },
            tir::ExprKind::Constructor { .. } if expr.is_unit_constructor() => {
                ir::ExprKind::Constant(ir::Constant::Unit)
            }
            tir::ExprKind::Constructor { name, entity, args } => ir::ExprKind::Construct {
                name: name.clone(),
                entity: entity.clone(),
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            },
            tir::ExprKind::Fn { params, body } => ir::ExprKind::Function {
                params: params
                    .iter()
                    .map(|param| self.lower_pattern(param))
                    .collect(),
                body: Box::new(self.lower_expr(body)),
            },
            tir::ExprKind::Let { bind, hint, body } => ir::ExprKind::Let {
                bind: self.lower_pattern(bind),
                hint: hint.clone(),
                body: Box::new(self.lower_expr(body)),
            },
            tir::ExprKind::Apply { callee, args } => ir::ExprKind::Apply {
                callee: Box::new(self.lower_expr(callee)),
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            },
            tir::ExprKind::BinaryOp { ident, left, right } => {
                let operand_type = if crate::checker::builtin::is_integer_type(&left.ty) {
                    left.ty.clone()
                } else if crate::checker::builtin::is_integer_type(&right.ty) {
                    right.ty.clone()
                } else if crate::checker::builtin::is_unqualified_type(&left.ty, "string") {
                    left.ty.clone()
                } else {
                    expr.ty.clone()
                };
                let left = self.lower_expr(left);
                let right = self.lower_expr(right);
                ir::ExprKind::Prim {
                    op: ir::Primitive::from_ident(ident, operand_type),
                    args: vec![left, right],
                }
            }
            tir::ExprKind::TypeHint { expr: inner, .. } => {
                return ir::Expr {
                    ty: expr.ty.clone(),
                    kind: self.lower_expr(inner).kind,
                    span: expr.span,
                };
            }
            tir::ExprKind::Tuple { items } => {
                ir::ExprKind::Tuple(items.iter().map(|item| self.lower_expr(item)).collect())
            }
            tir::ExprKind::Record { name, fields } => ir::ExprKind::Record {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(name, value)| (name.clone(), self.lower_expr(value)))
                    .collect(),
            },
            tir::ExprKind::Field { base, field } => ir::ExprKind::Field {
                base: Box::new(self.lower_expr(base)),
                field: field.clone(),
            },
            tir::ExprKind::Block { exprs } => {
                ir::ExprKind::Block(exprs.iter().map(|item| self.lower_expr(item)).collect())
            }
            tir::ExprKind::Match { scrutinee, arms } => ir::ExprKind::Match {
                scrutinee: Box::new(self.lower_expr(scrutinee)),
                arms: arms.iter().map(|arm| self.lower_match_arm(arm)).collect(),
            },
        };

        ir::Expr {
            ty: expr.ty.clone(),
            kind,
            span: expr.span,
        }
    }

    fn lower_match_arm(&mut self, arm: &tir::MatchArm) -> ir::MatchArm {
        ir::MatchArm {
            pattern: self.lower_pattern(&arm.pattern),
            body: self.lower_expr(&arm.body),
            span: arm.span,
        }
    }

    fn lower_pattern(&mut self, pattern: &tir::Pattern) -> ir::Pattern {
        let kind = match &pattern.kind {
            tir::PatternKind::Wildcard => ir::PatternKind::Wildcard,
            tir::PatternKind::Var { name, binding } => ir::PatternKind::Var {
                name: name.clone(),
                binding: binding.clone(),
            },
            tir::PatternKind::Literal { value } => {
                ir::PatternKind::Constant(ir::Constant::Literal(value.clone()))
            }
            tir::PatternKind::Constructor { .. } if pattern.is_unit_constructor() => {
                ir::PatternKind::Constant(ir::Constant::Unit)
            }
            tir::PatternKind::Constructor { name, entity, args } => ir::PatternKind::Construct {
                name: name.clone(),
                entity: entity.clone(),
                args: args.iter().map(|arg| self.lower_pattern(arg)).collect(),
            },
            tir::PatternKind::Tuple { items } => {
                ir::PatternKind::Tuple(items.iter().map(|item| self.lower_pattern(item)).collect())
            }
            tir::PatternKind::TypeHint { pattern: inner, .. } => {
                return ir::Pattern {
                    ty: pattern.ty.clone(),
                    kind: self.lower_pattern(inner).kind,
                    span: pattern.span,
                };
            }
            tir::PatternKind::Record { name, fields } => ir::PatternKind::Record {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(name, pattern)| (name.clone(), self.lower_pattern(pattern)))
                    .collect(),
            },
        };
        ir::Pattern {
            ty: pattern.ty.clone(),
            kind,
            span: pattern.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        checker::{Checker, tir},
        lambda::{ir, lower::lower_module},
        parser::{lexer::Lexer, parse::Parser},
    };

    fn checked(src: &str) -> tir::Module {
        let lexer = Lexer::new(src).expect("lex ok");
        let ast = Parser::new(lexer).parse_module().expect("parse ok");
        let summary = Checker::new().check_module(&ast);
        summary.tir
    }

    #[test]
    fn lowers_top_level_literal_const() {
        let module = checked("let answer = 42");
        let lowered = lower_module(&module);
        assert_eq!(lowered.values.len(), 1);
        assert!(matches!(
            lowered.values[0].body.kind,
            ir::ExprKind::Constant(ir::Constant::Literal(_))
        ));
    }

    #[test]
    fn lowers_unit_constructor_to_unit_constant() {
        let module = checked("let nothing = ()");
        let lowered = lower_module(&module);
        assert!(matches!(
            lowered.values[0].body.kind,
            ir::ExprKind::Constant(ir::Constant::Unit)
        ));
    }

    #[test]
    fn lowers_tuple() {
        let ty = crate::checker::builtin::unit();
        let span = crate::parser::lexer::Span::new(0, 0);
        let expr = tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::Tuple {
                items: vec![tir::Expr {
                    ty,
                    span,
                    kind: tir::ExprKind::Literal {
                        value: crate::parser::ast::Literal::Bool(true),
                    },
                }],
            },
        };
        let module = tir::Module {
            items: vec![tir::ModuleItem::LetDecl(tir::LetDecl::Const {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: crate::parser::ident::Ident::from_string("x"),
                hint: None,
                body: expr,
                scheme: crate::checker::scheme::Scheme::monomorphic(crate::checker::builtin::unit()),
                span,
            })],
        };
        let lowered = lower_module(&module);
        assert!(matches!(
            lowered.values[0].body.kind,
            ir::ExprKind::Tuple(_)
        ));
    }

    #[test]
    fn lowers_function_module_item() {
        let ty = crate::checker::builtin::unit();
        let span = crate::parser::lexer::Span::new(0, 0);
        let module = tir::Module {
            items: vec![tir::ModuleItem::LetDecl(tir::LetDecl::Fn {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: crate::parser::ident::Ident::from_string("f"),
                args: Vec::new(),
                body: tir::Expr {
                    ty: ty.clone(),
                    span,
                    kind: tir::ExprKind::Constructor {
                        name: crate::parser::ident::Ident::from_string(
                            crate::checker::builtin::UNIT_CONSTRUCTOR,
                        ),
                        entity: tir::EntityId::synthetic_value(0),
                        args: Vec::new(),
                    },
                },
                scheme: crate::checker::scheme::Scheme::monomorphic(ty),
                span,
            })],
        };
        let lowered = lower_module(&module);
        assert!(matches!(
            lowered.values[0].body.kind,
            ir::ExprKind::Function { .. }
        ));
    }

    #[test]
    fn lowers_apply() {
        let ty = crate::checker::builtin::unit();
        let span = crate::parser::lexer::Span::new(0, 0);
        let callee = tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::Var {
                name: crate::parser::ident::Ident::from_string("f"),
                entity: tir::EntityId::synthetic_value(0),
            },
        };
        let expr = tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::Apply {
                callee: Box::new(callee),
                args: Vec::new(),
            },
        };
        let module = tir::Module {
            items: vec![tir::ModuleItem::LetDecl(tir::LetDecl::Const {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: crate::parser::ident::Ident::from_string("x"),
                hint: None,
                body: expr,
                scheme: crate::checker::scheme::Scheme::monomorphic(ty),
                span,
            })],
        };
        let lowered = lower_module(&module);
        assert!(matches!(
            lowered.values[0].body.kind,
            ir::ExprKind::Apply { .. }
        ));
    }

    #[test]
    fn preserves_block_item_count() {
        let ty = crate::checker::builtin::bool();
        let span = crate::parser::lexer::Span::new(0, 0);
        let item = |value| tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::Literal {
                value: crate::parser::ast::Literal::Bool(value),
            },
        };
        let expr = tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::Block {
                exprs: vec![item(true), item(false)],
            },
        };
        let module = tir::Module {
            items: vec![tir::ModuleItem::LetDecl(tir::LetDecl::Const {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: crate::parser::ident::Ident::from_string("x"),
                hint: None,
                body: expr,
                scheme: crate::checker::scheme::Scheme::monomorphic(ty),
                span,
            })],
        };
        let lowered = lower_module(&module);
        let ir::ExprKind::Block(items) = &lowered.values[0].body.kind else {
            panic!("expected block");
        };
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn lowers_binary_op_to_prim() {
        let ty = crate::checker::builtin::i32();
        let span = crate::parser::lexer::Span::new(0, 0);
        let lit = |value: &str| tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::Literal {
                value: crate::parser::ast::Literal::Int(value.into()),
            },
        };
        let expr = tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::BinaryOp {
                ident: crate::parser::ident::Ident::from_string("+"),
                left: Box::new(lit("1")),
                right: Box::new(lit("2")),
            },
        };
        let module = tir::Module {
            items: vec![tir::ModuleItem::LetDecl(tir::LetDecl::Const {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: crate::parser::ident::Ident::from_string("x"),
                hint: None,
                body: expr,
                scheme: crate::checker::scheme::Scheme::monomorphic(ty),
                span,
            })],
        };
        let lowered = lower_module(&module);
        assert!(matches!(
            lowered.values[0].body.kind,
            ir::ExprKind::Prim {
                op: ir::Primitive::Add { .. },
                ..
            }
        ));
    }

    #[test]
    fn erases_type_hint_expression_node() {
        let ty = crate::checker::builtin::bool();
        let span = crate::parser::lexer::Span::new(0, 0);
        let inner = tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::Literal {
                value: crate::parser::ast::Literal::Bool(true),
            },
        };
        let expr = tir::Expr {
            ty: ty.clone(),
            span,
            kind: tir::ExprKind::TypeHint {
                expr: Box::new(inner),
                hint: ty.clone(),
            },
        };
        let module = tir::Module {
            items: vec![tir::ModuleItem::LetDecl(tir::LetDecl::Const {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: crate::parser::ident::Ident::from_string("x"),
                hint: None,
                body: expr,
                scheme: crate::checker::scheme::Scheme::monomorphic(ty),
                span,
            })],
        };
        let lowered = lower_module(&module);
        assert!(matches!(
            lowered.values[0].body.kind,
            ir::ExprKind::Constant(_)
        ));
    }
}
