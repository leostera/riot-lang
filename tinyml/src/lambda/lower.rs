use crate::checker::tst;

use super::ir;

pub fn lower_module(module: &tst::Module) -> ir::Module {
    Lowerer.lower_module(module)
}

pub struct Lowerer;

impl Lowerer {
    pub fn lower_module(&mut self, module: &tst::Module) -> ir::Module {
        let values = module
            .items
            .iter()
            .filter_map(|item| self.lower_module_item(item))
            .collect();
        ir::Module { values }
    }

    fn lower_module_item(&mut self, item: &tst::ModuleItem) -> Option<ir::ValueDecl> {
        match item {
            tst::ModuleItem::LetDecl(let_decl) => Some(self.lower_let_decl(let_decl)),
            tst::ModuleItem::UseDecl(_) | tst::ModuleItem::TypeDecl(_) => None,
        }
    }

    fn lower_let_decl(&mut self, let_decl: &tst::LetDecl) -> ir::ValueDecl {
        match let_decl {
            tst::LetDecl::Const {
                binding,
                entity,
                name,
                body,
                scheme,
                span,
                ..
            } => ir::ValueDecl {
                binding: *binding,
                entity: *entity,
                name: name.clone(),
                body: self.lower_expr(body),
                ty: scheme.body.clone(),
                span: *span,
            },
            tst::LetDecl::Fn {
                binding,
                entity,
                name,
                args,
                body,
                scheme,
                span,
            } => ir::ValueDecl {
                binding: *binding,
                entity: *entity,
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

    fn lower_expr(&mut self, expr: &tst::Expr) -> ir::Expr {
        let kind = match &expr.kind {
            tst::ExprKind::Literal { value } => {
                ir::ExprKind::Constant(ir::Constant::Literal(value.clone()))
            }
            tst::ExprKind::Var { name, entity } => ir::ExprKind::Var {
                name: name.clone(),
                entity: *entity,
            },
            tst::ExprKind::Constructor { .. } if expr.is_unit_constructor() => {
                ir::ExprKind::Constant(ir::Constant::Unit)
            }
            tst::ExprKind::Constructor { name, entity, args } => ir::ExprKind::Construct {
                name: name.clone(),
                entity: *entity,
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            },
            tst::ExprKind::Fn { params, body } => ir::ExprKind::Function {
                params: params
                    .iter()
                    .map(|param| self.lower_pattern(param))
                    .collect(),
                body: Box::new(self.lower_expr(body)),
            },
            tst::ExprKind::Let { bind, hint, body } => ir::ExprKind::Let {
                bind: self.lower_pattern(bind),
                hint: hint.clone(),
                body: Box::new(self.lower_expr(body)),
            },
            tst::ExprKind::Apply { callee, args } => ir::ExprKind::Apply {
                callee: Box::new(self.lower_expr(callee)),
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            },
            tst::ExprKind::BinaryOp { ident, left, right } => {
                let left = self.lower_expr(left);
                let right = self.lower_expr(right);
                ir::ExprKind::Prim {
                    op: ir::Primitive::from_ident(ident, left.ty.clone()),
                    args: vec![left, right],
                }
            }
            tst::ExprKind::TypeHint { expr: inner, .. } => {
                return ir::Expr {
                    ty: expr.ty.clone(),
                    kind: self.lower_expr(inner).kind,
                    span: expr.span,
                };
            }
            tst::ExprKind::Tuple { items } => {
                ir::ExprKind::Tuple(items.iter().map(|item| self.lower_expr(item)).collect())
            }
            tst::ExprKind::Record { name, fields } => ir::ExprKind::Record {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(name, value)| (name.clone(), self.lower_expr(value)))
                    .collect(),
            },
            tst::ExprKind::Block { exprs } => {
                ir::ExprKind::Block(exprs.iter().map(|item| self.lower_expr(item)).collect())
            }
            tst::ExprKind::Match { scrutinee, arms } => ir::ExprKind::Match {
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

    fn lower_match_arm(&mut self, arm: &tst::MatchArm) -> ir::MatchArm {
        ir::MatchArm {
            pattern: self.lower_pattern(&arm.pattern),
            body: self.lower_expr(&arm.body),
            span: arm.span,
        }
    }

    fn lower_pattern(&mut self, pattern: &tst::Pattern) -> ir::Pattern {
        let kind = match &pattern.kind {
            tst::PatternKind::Wildcard => ir::PatternKind::Wildcard,
            tst::PatternKind::Var { name, binding } => ir::PatternKind::Var {
                name: name.clone(),
                binding: *binding,
            },
            tst::PatternKind::Literal { value } => {
                ir::PatternKind::Constant(ir::Constant::Literal(value.clone()))
            }
            tst::PatternKind::Constructor { .. } if pattern.is_unit_constructor() => {
                ir::PatternKind::Constant(ir::Constant::Unit)
            }
            tst::PatternKind::Constructor { name, entity, args } => ir::PatternKind::Construct {
                name: name.clone(),
                entity: *entity,
                args: args.iter().map(|arg| self.lower_pattern(arg)).collect(),
            },
            tst::PatternKind::Tuple { items } => {
                ir::PatternKind::Tuple(items.iter().map(|item| self.lower_pattern(item)).collect())
            }
            tst::PatternKind::TypeHint { pattern: inner, .. } => {
                return ir::Pattern {
                    ty: pattern.ty.clone(),
                    kind: self.lower_pattern(inner).kind,
                    span: pattern.span,
                };
            }
            tst::PatternKind::Record { name, fields } => ir::PatternKind::Record {
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
        checker::{Checker, tst},
        lambda::{ir, lower::lower_module},
        parser::{lexer::Lexer, parse::Parser},
    };

    fn checked(src: &str) -> tst::Module {
        let lexer = Lexer::new(src).expect("lex ok");
        let ast = Parser::new(lexer).parse_module().expect("parse ok");
        let summary = Checker::new().check_module(&ast);
        summary.tst
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
        let expr = tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::Tuple {
                items: vec![tst::Expr {
                    ty,
                    span,
                    kind: tst::ExprKind::Literal {
                        value: crate::parser::ast::Literal::Bool(true),
                    },
                }],
            },
        };
        let module = tst::Module {
            items: vec![tst::ModuleItem::LetDecl(tst::LetDecl::Const {
                binding: tst::BindingId::new(0),
                entity: tst::EntityId::new(0),
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
        let module = tst::Module {
            items: vec![tst::ModuleItem::LetDecl(tst::LetDecl::Fn {
                binding: tst::BindingId::new(0),
                entity: tst::EntityId::new(0),
                name: crate::parser::ident::Ident::from_string("f"),
                args: Vec::new(),
                body: tst::Expr {
                    ty: ty.clone(),
                    span,
                    kind: tst::ExprKind::Constructor {
                        name: crate::parser::ident::Ident::from_string(
                            crate::checker::builtin::UNIT_CONSTRUCTOR,
                        ),
                        entity: tst::EntityId::new(0),
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
        let callee = tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::Var {
                name: crate::parser::ident::Ident::from_string("f"),
                entity: tst::EntityId::new(0),
            },
        };
        let expr = tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::Apply {
                callee: Box::new(callee),
                args: Vec::new(),
            },
        };
        let module = tst::Module {
            items: vec![tst::ModuleItem::LetDecl(tst::LetDecl::Const {
                binding: tst::BindingId::new(0),
                entity: tst::EntityId::new(0),
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
        let item = |value| tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::Literal {
                value: crate::parser::ast::Literal::Bool(value),
            },
        };
        let expr = tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::Block {
                exprs: vec![item(true), item(false)],
            },
        };
        let module = tst::Module {
            items: vec![tst::ModuleItem::LetDecl(tst::LetDecl::Const {
                binding: tst::BindingId::new(0),
                entity: tst::EntityId::new(0),
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
        let lit = |value: &str| tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::Literal {
                value: crate::parser::ast::Literal::Int(value.into()),
            },
        };
        let expr = tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::BinaryOp {
                ident: crate::parser::ident::Ident::from_string("+"),
                left: Box::new(lit("1")),
                right: Box::new(lit("2")),
            },
        };
        let module = tst::Module {
            items: vec![tst::ModuleItem::LetDecl(tst::LetDecl::Const {
                binding: tst::BindingId::new(0),
                entity: tst::EntityId::new(0),
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
        let inner = tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::Literal {
                value: crate::parser::ast::Literal::Bool(true),
            },
        };
        let expr = tst::Expr {
            ty: ty.clone(),
            span,
            kind: tst::ExprKind::TypeHint {
                expr: Box::new(inner),
                hint: ty.clone(),
            },
        };
        let module = tst::Module {
            items: vec![tst::ModuleItem::LetDecl(tst::LetDecl::Const {
                binding: tst::BindingId::new(0),
                entity: tst::EntityId::new(0),
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
