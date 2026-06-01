//! AArch64 instruction selection from native IR.

use std::collections::{HashMap, HashSet};

use crate::{
    backend::native::ir::{self, Symbol},
    lambda,
    parser::{ast::Literal, ident::Ident},
};

use super::mir::{Diagnostic, Function, Instr, Mir, PhysReg};

/// Select AArch64 MIR from native IR.
pub fn select(nir: &crate::backend::native::Nir) -> Mir {
    Selector::new().select_module(nir)
}

struct Selector {
    diagnostics: Vec<Diagnostic>,
    known_symbols: HashSet<Symbol>,
}

impl Selector {
    fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            known_symbols: HashSet::new(),
        }
    }

    fn select_module(mut self, module: &ir::Module) -> Mir {
        self.known_symbols = module
            .values
            .iter()
            .map(|value| value.symbol.clone())
            .collect();
        let functions = module
            .values
            .iter()
            .map(|value| self.select_value(value))
            .collect();
        Mir {
            functions,
            diagnostics: self.diagnostics,
        }
    }

    fn select_value(&mut self, value: &ir::ValueDecl) -> Function {
        let name = value.symbol.clone();
        let mut builder =
            FunctionBuilder::new(name.clone(), &mut self.diagnostics, &self.known_symbols);
        builder.instructions.push(Instr::Label(name.clone()));

        match &value.body.kind {
            ir::ExprKind::Function { params, body } => {
                if params.len() > 8 {
                    builder
                        .diagnostics
                        .push(Diagnostic::TooManyFunctionParameters {
                            name: name.clone(),
                            count: params.len(),
                        });
                }
                for (index, param) in params.iter().take(8).enumerate() {
                    if let ir::PatternKind::Var { name, .. } = &param.pattern.kind {
                        let slot = builder.bind_local(name);
                        builder.instructions.push(Instr::StoreStack {
                            slot,
                            src: PhysReg::X(index as u8),
                        });
                    } else if !param.pattern.is_unit_constant() {
                        builder.diagnostics.push(Diagnostic::UnsupportedExpression {
                            context: "function parameter pattern",
                        });
                    }
                }
                builder.emit_expr_to(body, PhysReg::X(0));
            }
            _ => builder.emit_expr_to(&value.body, PhysReg::X(0)),
        }

        builder.instructions.push(Instr::Ret);
        builder.finish()
    }
}

// FunctionBuilder owns the mutable state needed while selecting one NIR value
// into one target function: emitted instructions, local stack slots, diagnostics,
// and the set of known callable symbols. It keeps this per-function state out of
// the module-level selector so each function can be finished into an immutable
// MIR function independently.
struct FunctionBuilder<'a> {
    name: Symbol,
    instructions: Vec<Instr>,
    diagnostics: &'a mut Vec<Diagnostic>,
    known_symbols: &'a HashSet<Symbol>,
    locals: HashMap<Symbol, usize>,
    next_slot: usize,
}

impl<'a> FunctionBuilder<'a> {
    fn new(
        name: Symbol,
        diagnostics: &'a mut Vec<Diagnostic>,
        known_symbols: &'a HashSet<Symbol>,
    ) -> Self {
        Self {
            name,
            instructions: Vec::new(),
            diagnostics,
            known_symbols,
            locals: HashMap::new(),
            next_slot: 0,
        }
    }

    fn finish(self) -> Function {
        Function {
            name: self.name,
            instructions: self.instructions,
            stack_slots: self.next_slot,
        }
    }

    fn bind_local(&mut self, name: &Ident) -> usize {
        let name = Symbol::from_ident(name);
        if let Some(slot) = self.locals.get(&name) {
            return *slot;
        }
        let slot = self.next_slot;
        self.next_slot += 1;
        self.locals.insert(name, slot);
        slot
    }

    fn emit_expr_to(&mut self, expr: &ir::Expr, dst: PhysReg) {
        match &expr.kind {
            ir::ExprKind::Constant(value) => self.emit_constant_to(value, dst),
            ir::ExprKind::Var { symbol, .. } => {
                let name = symbol.clone();
                if let Some(slot) = self.locals.get(&name) {
                    self.instructions
                        .push(Instr::LoadStack { dst, slot: *slot });
                } else if self.known_symbols.contains(&name) {
                    self.instructions.push(Instr::Call { symbol: name });
                    emit_move_if_needed(&mut self.instructions, dst, PhysReg::X(0));
                } else {
                    self.diagnostics.push(Diagnostic::UnsupportedExpression {
                        context: "unbound variable",
                    });
                    self.instructions
                        .push(Instr::Comment(format!("unsupported variable {name}")));
                    self.instructions.push(Instr::MovImm { dst, value: 0 });
                }
            }
            ir::ExprKind::Prim { op, args } => self.emit_prim_to(op, args, dst),
            ir::ExprKind::Apply { callee, args } => self.emit_apply_to(callee, args, dst),
            ir::ExprKind::Block(exprs) => {
                for (index, expr) in exprs.iter().enumerate() {
                    let dst = if index + 1 == exprs.len() {
                        dst
                    } else {
                        PhysReg::X(9)
                    };
                    self.emit_expr_to(expr, dst);
                }
            }
            ir::ExprKind::Let { bind, body, .. } => {
                self.emit_expr_to(body, PhysReg::X(9));
                if let ir::PatternKind::Var { name, .. } = &bind.kind {
                    let slot = self.bind_local(name);
                    self.instructions.push(Instr::StoreStack {
                        slot,
                        src: PhysReg::X(9),
                    });
                    emit_move_if_needed(&mut self.instructions, dst, PhysReg::X(9));
                } else {
                    self.diagnostics.push(Diagnostic::UnsupportedExpression {
                        context: "let pattern",
                    });
                    self.instructions
                        .push(Instr::Comment("unsupported let pattern".into()));
                }
            }
            ir::ExprKind::MakeBlock { .. } => {
                self.unsupported(dst, "block allocation", "unsupported block allocation")
            }
            ir::ExprKind::Function { .. } => {
                self.unsupported(dst, "nested function", "unsupported nested function")
            }
            ir::ExprKind::Match { .. } => self.unsupported(dst, "match", "unsupported match"),
        }
    }

    fn unsupported(&mut self, dst: PhysReg, context: &'static str, comment: &str) {
        self.diagnostics
            .push(Diagnostic::UnsupportedExpression { context });
        self.instructions.push(Instr::Comment(comment.into()));
        self.instructions.push(Instr::MovImm { dst, value: 0 });
    }

    fn emit_constant_to(&mut self, value: &ir::Constant, dst: PhysReg) {
        match value {
            ir::Constant::Unit => self.instructions.push(Instr::MovImm { dst, value: 0 }),
            ir::Constant::Literal(Literal::Int(value)) => match value.parse::<i64>() {
                Ok(value) => self.instructions.push(Instr::MovImm { dst, value }),
                Err(_) => {
                    self.diagnostics.push(Diagnostic::UnsupportedLiteral {
                        literal: value.clone(),
                    });
                    self.instructions.push(Instr::MovImm { dst, value: 0 });
                }
            },
            ir::Constant::Literal(Literal::Bool(value)) => self.instructions.push(Instr::MovImm {
                dst,
                value: i64::from(*value),
            }),
            ir::Constant::Literal(Literal::String(value) | Literal::Float(value)) => {
                self.diagnostics.push(Diagnostic::UnsupportedLiteral {
                    literal: value.clone(),
                });
                self.instructions
                    .push(Instr::Comment(format!("unsupported literal {value:?}")));
                self.instructions.push(Instr::MovImm { dst, value: 0 });
            }
        }
    }

    fn emit_prim_to(&mut self, op: &lambda::ir::Primitive, args: &[ir::Expr], dst: PhysReg) {
        if args.len() != 2 {
            self.diagnostics.push(Diagnostic::UnsupportedExpression {
                context: "primitive arity",
            });
            self.instructions.push(Instr::MovImm { dst, value: 0 });
            return;
        }
        self.emit_expr_to(&args[0], PhysReg::X(9));
        self.emit_expr_to(&args[1], PhysReg::X(10));
        match op {
            lambda::ir::Primitive::Add { .. } => self.instructions.push(Instr::Add {
                dst,
                lhs: PhysReg::X(9),
                rhs: PhysReg::X(10),
            }),
            lambda::ir::Primitive::Sub { .. } => self.instructions.push(Instr::Sub {
                dst,
                lhs: PhysReg::X(9),
                rhs: PhysReg::X(10),
            }),
            lambda::ir::Primitive::Mul { .. } => self.instructions.push(Instr::Mul {
                dst,
                lhs: PhysReg::X(9),
                rhs: PhysReg::X(10),
            }),
            lambda::ir::Primitive::Div { .. } => self.instructions.push(Instr::SDiv {
                dst,
                lhs: PhysReg::X(9),
                rhs: PhysReg::X(10),
            }),
            lambda::ir::Primitive::Rem { .. } => {
                self.instructions.push(Instr::SDiv {
                    dst: PhysReg::X(11),
                    lhs: PhysReg::X(9),
                    rhs: PhysReg::X(10),
                });
                self.instructions.push(Instr::MSub {
                    dst,
                    lhs: PhysReg::X(11),
                    rhs: PhysReg::X(10),
                    acc: PhysReg::X(9),
                });
            }
            lambda::ir::Primitive::Named(_) => {
                self.unsupported(dst, "named primitive", "unsupported named primitive")
            }
        }
    }

    fn emit_apply_to(&mut self, callee: &ir::Expr, args: &[ir::Expr], dst: PhysReg) {
        if args.len() > 8 {
            self.diagnostics.push(Diagnostic::UnsupportedExpression {
                context: "too many call arguments",
            });
        }
        for (index, arg) in args.iter().take(8).enumerate() {
            self.emit_expr_to(arg, PhysReg::X(index as u8));
        }
        if let ir::ExprKind::Var { symbol, .. } = &callee.kind {
            self.instructions.push(Instr::Call {
                symbol: symbol.clone(),
            });
            emit_move_if_needed(&mut self.instructions, dst, PhysReg::X(0));
        } else {
            self.unsupported(dst, "indirect call", "unsupported indirect call");
        }
    }
}

impl ir::Pattern {
    fn is_unit_constant(&self) -> bool {
        matches!(&self.kind, ir::PatternKind::Constant(ir::Constant::Unit))
    }
}

fn emit_move_if_needed(instructions: &mut Vec<Instr>, dst: PhysReg, src: PhysReg) {
    if dst != src {
        instructions.push(Instr::Mov { dst, src });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        backend::native::{ir, layout},
        checker::{self, tir},
        lambda,
        parser::{ast::Literal, ident::Ident, lexer::Span},
    };

    fn unit() -> checker::tir::Type {
        checker::builtin::unit()
    }

    fn expr(kind: ir::ExprKind) -> ir::Expr {
        ir::Expr {
            ty: unit(),
            kind,
            span: Span::new(0, 0),
        }
    }

    fn decl(name: &str, body: ir::Expr) -> ir::ValueDecl {
        let name = Ident::from_string(name);
        ir::ValueDecl {
            binding: tir::BindingId::synthetic(0),
            entity: tir::EntityId::synthetic_value(0),
            symbol: ir::Symbol::from_ident(&name),
            name,
            ty: unit(),
            body,
            span: Span::new(0, 0),
        }
    }

    fn value(body: ir::Expr) -> ir::Module {
        ir::Module {
            values: vec![decl("answer", body)],
        }
    }

    fn var_pattern(name: &str) -> ir::Pattern {
        ir::Pattern {
            ty: unit(),
            kind: ir::PatternKind::Var {
                name: Ident::from_string(name),
                binding: tir::BindingId::synthetic(0),
            },
            span: Span::new(0, 0),
        }
    }

    fn var_expr(name: &str) -> ir::Expr {
        let name = Ident::from_string(name);
        expr(ir::ExprKind::Var {
            symbol: ir::Symbol::from_ident(&name),
            name,
            entity: tir::EntityId::synthetic_value(0),
        })
    }

    fn int(value: &str) -> ir::Expr {
        expr(ir::ExprKind::Constant(ir::Constant::Literal(Literal::Int(
            value.into(),
        ))))
    }

    #[test]
    fn selects_integer_constant_and_return() {
        let mir = select(&value(int("42")));
        assert!(matches!(
            mir.functions[0].instructions.as_slice(),
            [Instr::Label(_), Instr::MovImm { value: 42, .. }, Instr::Ret]
        ));
    }

    #[test]
    fn selects_integer_arithmetic() {
        let module = value(expr(ir::ExprKind::Prim {
            op: lambda::ir::Primitive::Add { operand: unit() },
            args: vec![int("1"), int("2")],
        }));
        let mir = select(&module);
        assert!(
            mir.functions[0]
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::Add { .. }))
        );
    }

    #[test]
    fn selects_direct_top_level_calls() {
        let module = ir::Module {
            values: vec![
                decl("callee", int("1")),
                decl(
                    "caller",
                    expr(ir::ExprKind::Apply {
                        callee: Box::new(var_expr("callee")),
                        args: Vec::new(),
                    }),
                ),
            ],
        };
        let mir = select(&module);
        assert!(
            mir.functions[1].instructions.iter().any(
                |instr| matches!(instr, Instr::Call { symbol } if symbol.as_str() == "callee")
            )
        );
    }

    #[test]
    fn spills_function_parameters_to_stack_slots() {
        let module = value(expr(ir::ExprKind::Function {
            params: vec![ir::Param {
                pattern: var_pattern("x"),
            }],
            body: Box::new(var_expr("x")),
        }));
        let mir = select(&module);
        assert_eq!(mir.functions[0].stack_slots, 1);
        assert!(
            mir.functions[0]
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::StoreStack { slot: 0, .. }))
        );
    }

    #[test]
    fn lowers_block_and_simple_let() {
        let module = value(expr(ir::ExprKind::Block(vec![
            expr(ir::ExprKind::Let {
                bind: var_pattern("x"),
                body: Box::new(int("1")),
                hint: None,
            }),
            var_expr("x"),
        ])));
        let mir = select(&module);
        assert!(
            mir.functions[0]
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::LoadStack { slot: 0, .. }))
        );
    }

    #[test]
    fn records_unsupported_block_allocation() {
        let module = value(expr(ir::ExprKind::MakeBlock {
            fields: Vec::new(),
            layout: layout::tuple(Vec::new()),
        }));
        let mir = select(&module);
        assert_eq!(
            mir.diagnostics,
            vec![Diagnostic::UnsupportedExpression {
                context: "block allocation"
            }]
        );
        assert!(mir.functions[0]
            .instructions
            .iter()
            .any(|instr| matches!(instr, Instr::Comment(comment) if comment == "unsupported block allocation"))
        );
    }
}
