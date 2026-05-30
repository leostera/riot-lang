use crate::{
    backend::native::{ir, layout},
    lambda,
};

pub fn lower_module(module: &lambda::ir::Module) -> ir::Module {
    Lowerer.lower_module(module)
}

pub struct Lowerer;

impl Lowerer {
    pub fn lower_module(&mut self, module: &lambda::ir::Module) -> ir::Module {
        ir::Module {
            values: module
                .values
                .iter()
                .map(|value| self.lower_value_decl(value))
                .collect(),
        }
    }

    fn lower_value_decl(&mut self, value: &lambda::ir::ValueDecl) -> ir::ValueDecl {
        ir::ValueDecl {
            binding: value.binding,
            entity: value.entity,
            name: value.name.clone(),
            body: self.lower_expr(&value.body),
            ty: value.ty.clone(),
            span: value.span,
        }
    }

    fn lower_expr(&mut self, expr: &lambda::ir::Expr) -> ir::Expr {
        let kind = match &expr.kind {
            lambda::ir::ExprKind::Constant(value) => ir::ExprKind::Constant(value.clone().into()),
            lambda::ir::ExprKind::Var { name, entity } => ir::ExprKind::Var {
                name: name.clone(),
                entity: *entity,
            },
            lambda::ir::ExprKind::Construct { name, args, .. }
                if matches!(
                    name.as_name(),
                    Some(crate::checker::builtin::UNIT_CONSTRUCTOR)
                ) && args.is_empty() =>
            {
                ir::ExprKind::Constant(ir::Constant::Unit)
            }
            lambda::ir::ExprKind::Construct { name, args, .. } => {
                let fields = args
                    .iter()
                    .map(|arg| self.lower_expr(arg))
                    .collect::<Vec<_>>();
                let layout = layout::constructor(
                    name.clone(),
                    0,
                    layout::fields(fields.iter().map(|field| field.ty.clone())),
                );
                ir::ExprKind::MakeBlock { layout, fields }
            }
            lambda::ir::ExprKind::Function { params, body } => ir::ExprKind::Function {
                params: params
                    .iter()
                    .map(|pattern| ir::Param {
                        pattern: self.lower_pattern(pattern),
                    })
                    .collect(),
                body: Box::new(self.lower_expr(body)),
            },
            lambda::ir::ExprKind::Let { bind, hint, body } => ir::ExprKind::Let {
                bind: self.lower_pattern(bind),
                hint: hint.clone(),
                body: Box::new(self.lower_expr(body)),
            },
            lambda::ir::ExprKind::Apply { callee, args } => ir::ExprKind::Apply {
                callee: Box::new(self.lower_expr(callee)),
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            },
            lambda::ir::ExprKind::Prim { op, args } => ir::ExprKind::Prim {
                op: op.clone(),
                args: args.iter().map(|arg| self.lower_expr(arg)).collect(),
            },
            lambda::ir::ExprKind::Tuple(items) => {
                let fields = items
                    .iter()
                    .map(|item| self.lower_expr(item))
                    .collect::<Vec<_>>();
                let layout =
                    layout::tuple(layout::fields(fields.iter().map(|field| field.ty.clone())));
                ir::ExprKind::MakeBlock { layout, fields }
            }
            lambda::ir::ExprKind::Record { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(_, value)| self.lower_expr(value))
                    .collect::<Vec<_>>();
                let layout = layout::record(
                    name.clone(),
                    layout::fields(fields.iter().map(|field| field.ty.clone())),
                );
                ir::ExprKind::MakeBlock { layout, fields }
            }
            lambda::ir::ExprKind::Block(exprs) => {
                ir::ExprKind::Block(exprs.iter().map(|expr| self.lower_expr(expr)).collect())
            }
            lambda::ir::ExprKind::Match { scrutinee, arms } => ir::ExprKind::Match {
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

    fn lower_match_arm(&mut self, arm: &lambda::ir::MatchArm) -> ir::MatchArm {
        ir::MatchArm {
            pattern: self.lower_pattern(&arm.pattern),
            body: self.lower_expr(&arm.body),
            span: arm.span,
        }
    }

    fn lower_pattern(&mut self, pattern: &lambda::ir::Pattern) -> ir::Pattern {
        let kind = match &pattern.kind {
            lambda::ir::PatternKind::Wildcard => ir::PatternKind::Wildcard,
            lambda::ir::PatternKind::Var { name, binding } => ir::PatternKind::Var {
                name: name.clone(),
                binding: *binding,
            },
            lambda::ir::PatternKind::Constant(value) => {
                ir::PatternKind::Constant(value.clone().into())
            }
            lambda::ir::PatternKind::Construct { name, entity, args } => {
                ir::PatternKind::Construct {
                    name: name.clone(),
                    entity: *entity,
                    args: args.iter().map(|arg| self.lower_pattern(arg)).collect(),
                }
            }
            lambda::ir::PatternKind::Tuple(items) => {
                ir::PatternKind::Tuple(items.iter().map(|item| self.lower_pattern(item)).collect())
            }
            lambda::ir::PatternKind::Record { name, fields } => ir::PatternKind::Record {
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
        backend::native::{ir, layout, lower::lower_module},
        checker::{self, tst},
        lambda,
        parser::{ident::Ident, lexer::Span},
    };

    fn expr(kind: lambda::ir::ExprKind) -> lambda::ir::Expr {
        lambda::ir::Expr {
            ty: checker::builtin::unit(),
            kind,
            span: Span::new(0, 0),
        }
    }

    fn value(body: lambda::ir::Expr) -> lambda::ir::Module {
        lambda::ir::Module {
            values: vec![lambda::ir::ValueDecl {
                binding: tst::BindingId::new(0),
                entity: tst::EntityId::new(0),
                name: Ident::from_string("x"),
                ty: checker::builtin::unit(),
                body,
                span: Span::new(0, 0),
            }],
        }
    }

    #[test]
    fn lowers_tuple_to_make_block() {
        let module = value(expr(lambda::ir::ExprKind::Tuple(vec![expr(
            lambda::ir::ExprKind::Constant(lambda::ir::Constant::Unit),
        )])));
        let lowered = lower_module(&module);
        assert!(matches!(
            &lowered.values[0].body.kind,
            ir::ExprKind::MakeBlock {
                layout: layout::BlockLayout {
                    tag: layout::BlockTag::Tuple,
                    allocation: layout::Allocation::StackCandidate,
                    ..
                },
                ..
            }
        ));
    }

    #[test]
    fn lowers_record_to_make_block() {
        let module = value(expr(lambda::ir::ExprKind::Record {
            name: Ident::from_string("User"),
            fields: vec![(
                Ident::from_string("name"),
                expr(lambda::ir::ExprKind::Constant(lambda::ir::Constant::Unit)),
            )],
        }));
        let lowered = lower_module(&module);
        assert!(matches!(
            &lowered.values[0].body.kind,
            ir::ExprKind::MakeBlock {
                layout: layout::BlockLayout {
                    tag: layout::BlockTag::Record { .. },
                    ..
                },
                ..
            }
        ));
    }

    #[test]
    fn preserves_block_item_count() {
        let module = value(expr(lambda::ir::ExprKind::Block(vec![
            expr(lambda::ir::ExprKind::Constant(lambda::ir::Constant::Unit)),
            expr(lambda::ir::ExprKind::Constant(lambda::ir::Constant::Unit)),
        ])));
        let lowered = lower_module(&module);
        let ir::ExprKind::Block(items) = &lowered.values[0].body.kind else {
            panic!("expected block");
        };
        assert_eq!(items.len(), 2);
    }
}
