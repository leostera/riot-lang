//! Lambda-to-native SSA construction.
//!
//! This pass removes Lambda expression nesting and builds TinyML's native SSA
//! representation: simple value-producing statements, explicit blocks, block
//! parameters, and terminators. Target backends still perform instruction
//! selection and final instruction-stream emission.

use std::collections::HashMap;

use crate::{
    backend::native::ssa,
    lambda,
    parser::{ident::Ident, lexer::Span},
};

pub fn lower_module(module: &lambda::ir::Module) -> ssa::SsaProgram {
    Lowerer.lower_module(module)
}

pub struct Lowerer;

impl Lowerer {
    pub fn lower_module(&mut self, module: &lambda::ir::Module) -> ssa::SsaProgram {
        let mut globals = Vec::new();
        let mut functions = Vec::new();

        for value in &module.values {
            match &value.body.kind {
                lambda::ir::ExprKind::Function { .. } => functions.push(self.lower_function(value)),
                lambda::ir::ExprKind::Constant(constant) => globals.push(ssa::Global {
                    binding: value.binding.clone(),
                    entity: value.entity.clone(),
                    symbol: ssa::Symbol::from_ident(&value.name),
                    name: value.name.clone(),
                    value: constant.clone().into(),
                    repr: ssa::MachineRepr::of_type(&value.ty),
                    ty: value.ty.clone(),
                    span: value.span,
                }),
                _ => functions.push(self.lower_initializer_function(value)),
            }
        }

        ssa::SsaProgram { globals, functions }
    }

    fn lower_function(&mut self, value: &lambda::ir::ValueDecl) -> ssa::Function {
        let lambda::ir::ExprKind::Function { params, body } = &value.body.kind else {
            unreachable!("caller checked function body")
        };

        let mut builder = FunctionBuilder::new(
            value.binding.clone(),
            value.entity.clone(),
            value.name.clone(),
            value.ty.clone(),
            value.span,
        );

        for param in params {
            match &param.kind {
                lambda::ir::PatternKind::Var { name, .. } => {
                    let id = builder.new_value(
                        param.ty.clone(),
                        ssa::MachineRepr::of_type(&param.ty),
                        Some(name.clone()),
                        param.span,
                    );
                    builder.params.push(id);
                    builder.env.insert(ssa::Symbol::from_ident(name), id);
                }
                lambda::ir::PatternKind::Constant(lambda::ir::Constant::Unit) => {}
                _ => {
                    let id = builder.new_value(
                        param.ty.clone(),
                        ssa::MachineRepr::of_type(&param.ty),
                        Some(Ident::from_string("param")),
                        param.span,
                    );
                    builder.params.push(id);
                    builder.bind_pattern(param, id);
                }
            }
        }

        let result = builder.lower_expr(body);
        builder.finish(result)
    }

    fn lower_initializer_function(&mut self, value: &lambda::ir::ValueDecl) -> ssa::Function {
        let mut builder = FunctionBuilder::new(
            value.binding.clone(),
            value.entity.clone(),
            value.name.clone(),
            value.ty.clone(),
            value.span,
        );
        let result = builder.lower_expr(&value.body);
        builder.finish(result)
    }
}

struct FunctionBuilder {
    binding: crate::checker::tir::BindingId,
    entity: crate::checker::tir::EntityId,
    name: Ident,
    symbol: ssa::Symbol,
    ty: crate::checker::tir::Type,
    span: Span,
    params: Vec<ssa::ValueId>,
    values: Vec<ssa::ValueInfo>,
    stmts: Vec<ssa::Stmt>,
    env: HashMap<ssa::Symbol, ssa::ValueId>,
}

impl FunctionBuilder {
    fn new(
        binding: crate::checker::tir::BindingId,
        entity: crate::checker::tir::EntityId,
        name: Ident,
        ty: crate::checker::tir::Type,
        span: Span,
    ) -> Self {
        let symbol = ssa::Symbol::from_ident(&name);
        Self {
            binding,
            entity,
            name,
            symbol,
            ty,
            span,
            params: Vec::new(),
            values: Vec::new(),
            stmts: Vec::new(),
            env: HashMap::new(),
        }
    }

    fn new_value(
        &mut self,
        ty: crate::checker::tir::Type,
        repr: ssa::MachineRepr,
        debug_name: Option<Ident>,
        span: Span,
    ) -> ssa::ValueId {
        let id = ssa::ValueId(self.values.len() as u32);
        self.values.push(ssa::ValueInfo {
            repr,
            ty,
            debug_name,
            span,
        });
        id
    }

    fn assign(
        &mut self,
        ty: crate::checker::tir::Type,
        debug_name: Option<Ident>,
        span: Span,
        rhs: ssa::Rvalue,
    ) -> ssa::ValueId {
        let repr = ssa::MachineRepr::of_type(&ty);
        let dst = self.new_value(ty, repr, debug_name, span);
        self.stmts.push(ssa::Stmt::Assign { dst, rhs, span });
        dst
    }

    fn lower_expr(&mut self, expr: &lambda::ir::Expr) -> Option<ssa::ValueId> {
        match &expr.kind {
            lambda::ir::ExprKind::Constant(value) => match value {
                lambda::ir::Constant::Literal(crate::parser::ast::Literal::String(value)) => {
                    Some(self.assign(
                        expr.ty.clone(),
                        None,
                        expr.span,
                        ssa::Rvalue::StringLiteral {
                            value: value.clone(),
                        },
                    ))
                }
                lambda::ir::Constant::Literal(crate::parser::ast::Literal::Float(value)) => {
                    Some(self.assign(
                        expr.ty.clone(),
                        None,
                        expr.span,
                        ssa::Rvalue::FloatLiteral {
                            value: value.clone(),
                        },
                    ))
                }
                _ => Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::Const(value.clone().into()),
                )),
            },
            lambda::ir::ExprKind::Construct { name, args, .. }
                if matches!(
                    name.as_name(),
                    Some(crate::checker::builtin::UNIT_CONSTRUCTOR)
                ) && args.is_empty() =>
            {
                Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::Const(ssa::Constant::Unit),
                ))
            }
            lambda::ir::ExprKind::Construct { name, args, .. } if args.is_empty() => {
                Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::Const(ssa::Constant::ConstructorTag {
                        name: name.clone(),
                        tag: 0,
                    }),
                ))
            }
            lambda::ir::ExprKind::Var { name, .. } => {
                let symbol = ssa::Symbol::from_ident(name);
                if let Some(value) = self.env.get(&symbol) {
                    Some(*value)
                } else {
                    Some(self.assign(
                        expr.ty.clone(),
                        Some(name.clone()),
                        expr.span,
                        ssa::Rvalue::Global(symbol),
                    ))
                }
            }
            lambda::ir::ExprKind::Prim { op, args } => {
                let args = args.iter().filter_map(|arg| self.lower_expr(arg)).collect();
                Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::Prim {
                        op: op.clone(),
                        args,
                    },
                ))
            }
            lambda::ir::ExprKind::Apply { callee, args } => {
                let args = args.iter().filter_map(|arg| self.lower_expr(arg)).collect();
                match &callee.kind {
                    lambda::ir::ExprKind::Var { name, .. } => Some(self.assign(
                        expr.ty.clone(),
                        None,
                        expr.span,
                        ssa::Rvalue::Call {
                            callee: ssa::Symbol::from_ident(name),
                            args,
                        },
                    )),
                    _ => Some(self.assign(
                        expr.ty.clone(),
                        None,
                        expr.span,
                        ssa::Rvalue::Call {
                            callee: ssa::Symbol::new("tinyml_rt_closure_call"),
                            args,
                        },
                    )),
                }
            }
            lambda::ir::ExprKind::Let { bind, body, .. } => {
                // The current Lambda let node stores the body expression as the RHS for
                // local lets. Preserve the existing behavior by binding the lowered
                // expression when the pattern is a variable.
                let value = self.lower_expr(body);
                if let Some(value) = value {
                    self.bind_pattern(bind, value);
                }
                value
            }
            lambda::ir::ExprKind::Block(exprs) => {
                let mut last = None;
                for expr in exprs {
                    last = self.lower_expr(expr);
                }
                last
            }
            lambda::ir::ExprKind::Function { .. } => Some(self.assign(
                expr.ty.clone(),
                None,
                expr.span,
                ssa::Rvalue::Call {
                    callee: ssa::Symbol::new("tinyml_rt_closure_new"),
                    args: Vec::new(),
                },
            )),
            lambda::ir::ExprKind::Tuple(items) => {
                let fields = items
                    .iter()
                    .filter_map(|item| self.lower_expr(item))
                    .collect();
                Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::MakeBlock {
                        tag: ssa::BlockTag::Tuple,
                        fields,
                    },
                ))
            }
            lambda::ir::ExprKind::Field { base, .. } => self.lower_expr(base),
            lambda::ir::ExprKind::Record { name: _, fields } => {
                let fields = fields
                    .iter()
                    .filter_map(|(_, value)| self.lower_expr(value))
                    .collect();
                Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::MakeBlock {
                        tag: ssa::BlockTag::Record,
                        fields,
                    },
                ))
            }
            lambda::ir::ExprKind::Construct { name: _, args, .. } => {
                let fields = args.iter().filter_map(|arg| self.lower_expr(arg)).collect();
                Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::MakeBlock {
                        tag: ssa::BlockTag::Variant(0),
                        fields,
                    },
                ))
            }
            lambda::ir::ExprKind::Match { scrutinee, arms } => {
                let scrutinee = self.lower_expr(scrutinee)?;
                Some(self.assign(
                    expr.ty.clone(),
                    None,
                    expr.span,
                    ssa::Rvalue::RuntimeMatch {
                        scrutinee,
                        arm_count: arms.len(),
                    },
                ))
            }
        }
    }

    fn bind_pattern(&mut self, pattern: &lambda::ir::Pattern, value: ssa::ValueId) {
        match &pattern.kind {
            lambda::ir::PatternKind::Var { name, .. } => {
                self.env.insert(ssa::Symbol::from_ident(name), value);
            }
            lambda::ir::PatternKind::Tuple(items) => {
                for (index, item) in items.iter().enumerate() {
                    let field = self.assign(
                        item.ty.clone(),
                        None,
                        item.span,
                        ssa::Rvalue::GetField {
                            block: value,
                            index,
                        },
                    );
                    self.bind_pattern(item, field);
                }
            }
            lambda::ir::PatternKind::Record { fields, .. } => {
                for (index, (field_name, item)) in fields.iter().enumerate() {
                    let field = self.assign(
                        item.ty.clone(),
                        Some(field_name.clone()),
                        item.span,
                        ssa::Rvalue::GetField {
                            block: value,
                            index,
                        },
                    );
                    self.bind_pattern(item, field);
                }
            }
            lambda::ir::PatternKind::Construct { args, .. } => {
                for (index, item) in args.iter().enumerate() {
                    let field = self.assign(
                        item.ty.clone(),
                        None,
                        item.span,
                        ssa::Rvalue::GetField {
                            block: value,
                            index,
                        },
                    );
                    self.bind_pattern(item, field);
                }
            }
            lambda::ir::PatternKind::Wildcard | lambda::ir::PatternKind::Constant(_) => {}
        }
    }

    fn finish(self, result: Option<ssa::ValueId>) -> ssa::Function {
        let entry = ssa::BlockId(0);
        ssa::Function {
            binding: self.binding,
            entity: self.entity,
            name: self.name,
            symbol: self.symbol,
            params: self.params.clone(),
            values: self.values,
            entry,
            blocks: vec![ssa::Block {
                id: entry,
                params: self.params,
                stmts: self.stmts,
                terminator: ssa::Terminator::Return(result),
            }],
            ret: ssa::MachineRepr::of_type(&self.ty),
            ty: self.ty,
            span: self.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        backend::native::ssa::{self, builder::lower_module},
        checker::{self, tir},
        lambda,
        parser::{ast::Literal, ident::Ident, lexer::Span},
    };

    fn expr_with_ty(ty: checker::tir::Type, kind: lambda::ir::ExprKind) -> lambda::ir::Expr {
        lambda::ir::Expr {
            ty,
            kind,
            span: Span::new(0, 0),
        }
    }

    fn value(body: lambda::ir::Expr, ty: checker::tir::Type) -> lambda::ir::Module {
        lambda::ir::Module {
            values: vec![lambda::ir::ValueDecl {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: Ident::from_string("x"),
                ty,
                body,
                span: Span::new(0, 0),
            }],
        }
    }

    #[test]
    fn lowers_top_level_literal_to_global() {
        let ty = checker::builtin::i32();
        let module = value(
            expr_with_ty(
                ty.clone(),
                lambda::ir::ExprKind::Constant(lambda::ir::Constant::Literal(Literal::Int(
                    "1".into(),
                ))),
            ),
            ty,
        );
        let lowered = lower_module(&module);
        assert_eq!(lowered.globals.len(), 1);
        assert!(matches!(lowered.globals[0].repr, ssa::MachineRepr::I32));
    }

    #[test]
    fn lowers_primitive_to_ssa_stmt() {
        let ty = checker::builtin::i32();
        let lit = |value: &str| {
            expr_with_ty(
                ty.clone(),
                lambda::ir::ExprKind::Constant(lambda::ir::Constant::Literal(Literal::Int(
                    value.into(),
                ))),
            )
        };
        let module = value(
            expr_with_ty(
                ty.clone(),
                lambda::ir::ExprKind::Prim {
                    op: lambda::ir::Primitive::Add {
                        operand: ty.clone(),
                    },
                    args: vec![lit("1"), lit("2")],
                },
            ),
            ty,
        );
        let lowered = lower_module(&module);
        let block = &lowered.functions[0].blocks[0];
        assert_eq!(block.stmts.len(), 3);
        assert!(matches!(block.terminator, ssa::Terminator::Return(Some(_))));
    }
}
