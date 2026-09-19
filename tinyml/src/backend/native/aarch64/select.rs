//! AArch64 instruction selection from native SSA IR.

use std::collections::{HashMap, HashSet};

use crate::{
    backend::native::ssa::{self, MachineRepr, Symbol, ValueId},
    lambda,
    parser::ast::Literal,
};

use super::mir::{Diagnostic, Entry, Instr, Mir, PhysReg};

/// Select AArch64 MIR from native IR.
pub fn select(nir: &crate::backend::native::ssa::SsaProgram) -> Mir {
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

    fn select_module(mut self, module: &ssa::SsaProgram) -> Mir {
        self.known_symbols = module
            .globals
            .iter()
            .map(|global| global.symbol.clone())
            .chain(
                module
                    .functions
                    .iter()
                    .map(|function| function.symbol.clone()),
            )
            .collect();

        let mut entries = Vec::new();
        for global in &module.globals {
            entries.push(self.select_global(global));
        }
        for function in &module.functions {
            entries.push(self.select_function(function));
        }

        Mir {
            entries,
            diagnostics: self.diagnostics,
        }
    }

    fn select_global(&mut self, global: &ssa::Global) -> Entry {
        let name = global.symbol.clone();
        let mut builder = FunctionBuilder::new(
            name.clone(),
            &mut self.diagnostics,
            &self.known_symbols,
            &[],
        );
        builder.instructions.push(Instr::Label(name.clone()));
        builder.emit_constant_to(&global.value, PhysReg::X(0));
        builder.instructions.push(Instr::Ret);
        builder.finish()
    }

    fn select_function(&mut self, function: &ssa::Function) -> Entry {
        let name = function.symbol.clone();
        let mut builder = FunctionBuilder::new(
            name.clone(),
            &mut self.diagnostics,
            &self.known_symbols,
            &function.values,
        );
        builder.instructions.push(Instr::Label(name.clone()));

        if function.params.len() > 8 {
            builder
                .diagnostics
                .push(Diagnostic::TooManyFunctionParameters {
                    name: name.clone(),
                    count: function.params.len(),
                });
        }

        for (index, param) in function.params.iter().take(8).enumerate() {
            let slot = builder.alloc_slot(*param);
            builder.instructions.push(Instr::StoreStack {
                slot,
                src: PhysReg::X(index as u8),
            });
            builder.values.insert(
                *param,
                ValueLoc::StackSlot {
                    slot,
                    repr: builder.repr(*param),
                },
            );
        }

        for block in &function.blocks {
            for param in &block.params {
                builder.alloc_slot(*param);
            }
        }

        for block in &function.blocks {
            builder.emit_block(block);
        }

        builder.finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueLoc {
    Immediate { value: i64, repr: MachineRepr },
    StackSlot { slot: usize, repr: MachineRepr },
}

struct FunctionBuilder<'a> {
    name: Symbol,
    instructions: Vec<Instr>,
    diagnostics: &'a mut Vec<Diagnostic>,
    infos: &'a [ssa::ValueInfo],
    values: HashMap<ValueId, ValueLoc>,
    slots: HashMap<ValueId, usize>,
    next_slot: usize,
}

impl<'a> FunctionBuilder<'a> {
    fn new(
        name: Symbol,
        diagnostics: &'a mut Vec<Diagnostic>,
        _known_symbols: &'a HashSet<Symbol>,
        infos: &'a [ssa::ValueInfo],
    ) -> Self {
        Self {
            name,
            instructions: Vec::new(),
            diagnostics,
            infos,
            values: HashMap::new(),
            slots: HashMap::new(),
            next_slot: 0,
        }
    }

    fn finish(self) -> Entry {
        Entry {
            name: self.name,
            instructions: self.instructions,
            stack_slots: self.next_slot,
        }
    }

    fn repr(&self, value: ValueId) -> MachineRepr {
        self.infos
            .get(value.0 as usize)
            .map(|info| info.repr)
            .unwrap_or(MachineRepr::Ptr)
    }

    fn alloc_slot(&mut self, value: ValueId) -> usize {
        if let Some(slot) = self.slots.get(&value) {
            return *slot;
        }
        let slot = self.next_slot;
        self.next_slot += 1;
        self.slots.insert(value, slot);
        slot
    }

    fn emit_block(&mut self, block: &ssa::Block) {
        if block.id.0 != 0 {
            self.instructions
                .push(Instr::BlockLabel(self.block_label(block.id)));
        }
        for param in &block.params {
            let slot = self.alloc_slot(*param);
            self.values.insert(
                *param,
                ValueLoc::StackSlot {
                    slot,
                    repr: self.repr(*param),
                },
            );
        }
        for stmt in &block.stmts {
            self.emit_stmt(stmt);
        }
        self.emit_terminator(&block.terminator);
    }

    fn emit_stmt(&mut self, stmt: &ssa::Stmt) {
        let ssa::Stmt::Assign { dst, rhs, .. } = stmt;
        let repr = self.repr(*dst);
        match rhs {
            ssa::Rvalue::Const(constant) => {
                if let Some(value) = constant_i64(constant) {
                    self.values
                        .insert(*dst, ValueLoc::Immediate { value, repr });
                } else {
                    self.emit_constant_to(constant, PhysReg::X(9));
                    self.store_value(*dst, PhysReg::X(9), repr);
                }
            }
            ssa::Rvalue::Copy(src) => {
                let reg = self.ensure_reg(*src, PhysReg::X(9));
                self.store_value(*dst, reg, repr);
            }
            ssa::Rvalue::Global(symbol) => {
                self.instructions.push(Instr::Call {
                    symbol: symbol.clone(),
                });
                self.store_value(*dst, PhysReg::X(0), repr);
            }
            ssa::Rvalue::Call { callee, args } => {
                self.emit_call(callee.clone(), args, *dst, repr);
            }
            ssa::Rvalue::Prim { op, args } => self.emit_prim(*dst, repr, op, args),
            ssa::Rvalue::MakeBlock { tag, fields } => self.emit_block_new(tag, fields, *dst, repr),
            ssa::Rvalue::GetField { block, index } => {
                self.emit_indexed_rt_call(rt::abi::BLOCK_GET, *block, *index, *dst, repr)
            }
            ssa::Rvalue::GetTag { block } => {
                self.emit_unary_rt_call(rt::abi::BLOCK_TAG, *block, *dst, repr)
            }
            ssa::Rvalue::StringLiteral { .. } => {
                self.emit_symbol_call(rt::abi::STRING_NEW, *dst, repr)
            }
            ssa::Rvalue::FloatLiteral { .. } => {
                self.emit_symbol_call(rt::abi::FLOAT_NEW, *dst, repr)
            }
            ssa::Rvalue::RuntimeMatch { scrutinee, .. } => {
                self.emit_unary_rt_call(rt::abi::MATCH_FAILURE, *scrutinee, *dst, repr)
            }
        }
    }

    fn emit_call(&mut self, callee: Symbol, args: &[ValueId], dst: ValueId, repr: MachineRepr) {
        for (index, arg) in args.iter().take(8).enumerate() {
            let reg = self.ensure_reg(*arg, PhysReg::X(index as u8));
            emit_move_if_needed(&mut self.instructions, PhysReg::X(index as u8), reg);
        }
        self.instructions.push(Instr::Call { symbol: callee });
        self.store_value(dst, PhysReg::X(0), repr);
    }

    fn emit_symbol_call(&mut self, symbol: &str, dst: ValueId, repr: MachineRepr) {
        self.instructions.push(Instr::Call {
            symbol: Symbol::new(symbol),
        });
        self.store_value(dst, PhysReg::X(0), repr);
    }

    fn emit_unary_rt_call(
        &mut self,
        symbol: &str,
        value: ValueId,
        dst: ValueId,
        repr: MachineRepr,
    ) {
        let reg = self.ensure_reg(value, PhysReg::X(0));
        emit_move_if_needed(&mut self.instructions, PhysReg::X(0), reg);
        self.emit_symbol_call(symbol, dst, repr);
    }

    fn emit_indexed_rt_call(
        &mut self,
        symbol: &str,
        value: ValueId,
        index: usize,
        dst: ValueId,
        repr: MachineRepr,
    ) {
        let reg = self.ensure_reg(value, PhysReg::X(0));
        emit_move_if_needed(&mut self.instructions, PhysReg::X(0), reg);
        self.instructions.push(Instr::MovImm {
            dst: PhysReg::X(1),
            value: index as i64,
        });
        self.emit_symbol_call(symbol, dst, repr);
    }

    fn emit_block_new(
        &mut self,
        tag: &ssa::BlockTag,
        fields: &[ValueId],
        dst: ValueId,
        repr: MachineRepr,
    ) {
        self.instructions.push(Instr::MovImm {
            dst: PhysReg::X(0),
            value: block_tag_value(tag),
        });
        self.instructions.push(Instr::MovImm {
            dst: PhysReg::X(1),
            value: fields.len() as i64,
        });
        for (index, field) in fields.iter().take(6).enumerate() {
            let reg = self.ensure_reg(*field, PhysReg::X(index as u8 + 2));
            emit_move_if_needed(&mut self.instructions, PhysReg::X(index as u8 + 2), reg);
        }
        self.emit_symbol_call(rt::abi::BLOCK_NEW, dst, repr);
    }

    fn emit_prim(
        &mut self,
        dst: ValueId,
        repr: MachineRepr,
        op: &lambda::ir::Primitive,
        args: &[ValueId],
    ) {
        if args.len() != 2 {
            self.instructions.push(Instr::MovImm {
                dst: PhysReg::X(9),
                value: 0,
            });
            self.store_value(dst, PhysReg::X(9), repr);
            return;
        }
        let lhs = self.ensure_reg(args[0], PhysReg::X(9));
        let rhs = self.ensure_reg(args[1], PhysReg::X(10));
        let out = PhysReg::X(9);
        match op {
            lambda::ir::Primitive::Add { .. } => {
                self.instructions.push(Instr::Add { dst: out, lhs, rhs })
            }
            lambda::ir::Primitive::Sub { .. } => {
                self.instructions.push(Instr::Sub { dst: out, lhs, rhs })
            }
            lambda::ir::Primitive::Mul { .. } => {
                self.instructions.push(Instr::Mul { dst: out, lhs, rhs })
            }
            lambda::ir::Primitive::Div { .. } => {
                self.instructions.push(Instr::SDiv { dst: out, lhs, rhs })
            }
            lambda::ir::Primitive::Rem { .. } => {
                self.instructions.push(Instr::SDiv {
                    dst: PhysReg::X(11),
                    lhs,
                    rhs,
                });
                self.instructions.push(Instr::MSub {
                    dst: out,
                    lhs: PhysReg::X(11),
                    rhs,
                    acc: lhs,
                });
            }
            lambda::ir::Primitive::Eq { .. }
            | lambda::ir::Primitive::Ne { .. }
            | lambda::ir::Primitive::Lt { .. }
            | lambda::ir::Primitive::Lte { .. }
            | lambda::ir::Primitive::Gt { .. }
            | lambda::ir::Primitive::Gte { .. } => {
                self.emit_symbol_call("tinyml_rt_compare", dst, repr);
                return;
            }
            lambda::ir::Primitive::Named(name) => {
                self.emit_symbol_call(&ssa::Symbol::from_ident(name).to_string(), dst, repr);
                return;
            }
        }
        self.store_value(dst, out, repr);
    }

    fn emit_terminator(&mut self, terminator: &ssa::Terminator) {
        match terminator {
            ssa::Terminator::Return(Some(value)) => {
                let reg = self.ensure_reg(*value, PhysReg::X(0));
                emit_move_if_needed(&mut self.instructions, PhysReg::X(0), reg);
                self.instructions.push(Instr::Ret);
            }
            ssa::Terminator::Return(None) => {
                self.instructions.push(Instr::MovImm {
                    dst: PhysReg::X(0),
                    value: 0,
                });
                self.instructions.push(Instr::Ret);
            }
            ssa::Terminator::Jump { target, args } => {
                self.store_block_args(*target, args);
                self.instructions.push(Instr::B {
                    label: self.block_label(*target),
                });
            }
            ssa::Terminator::Branch {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } => {
                let reg = self.ensure_reg(*cond, PhysReg::X(9));
                self.instructions.push(Instr::CmpImm { lhs: reg, imm: 0 });
                self.store_block_args(*then_target, then_args);
                self.instructions.push(Instr::BCond {
                    cond: "ne".into(),
                    label: self.block_label(*then_target),
                });
                self.store_block_args(*else_target, else_args);
                self.instructions.push(Instr::B {
                    label: self.block_label(*else_target),
                });
            }
            ssa::Terminator::Switch {
                discr,
                cases,
                default,
                default_args,
            } => {
                let reg = self.ensure_reg(*discr, PhysReg::X(9));
                for (tag, target, args) in cases {
                    self.instructions.push(Instr::CmpImm {
                        lhs: reg,
                        imm: *tag,
                    });
                    self.store_block_args(*target, args);
                    self.instructions.push(Instr::BCond {
                        cond: "eq".into(),
                        label: self.block_label(*target),
                    });
                }
                self.store_block_args(*default, default_args);
                self.instructions.push(Instr::B {
                    label: self.block_label(*default),
                });
            }
        }
    }

    fn store_block_args(&mut self, _target: ssa::BlockId, args: &[ValueId]) {
        for arg in args {
            let _ = self.ensure_reg(*arg, PhysReg::X(9));
        }
    }

    fn block_label(&self, block: ssa::BlockId) -> String {
        format!("L_{}_block{}", self.name.as_str(), block.0)
    }

    fn ensure_reg(&mut self, value: ValueId, preferred: PhysReg) -> PhysReg {
        match self
            .values
            .get(&value)
            .copied()
            .unwrap_or(ValueLoc::Immediate {
                value: 0,
                repr: MachineRepr::Ptr,
            }) {
            ValueLoc::Immediate { value, .. } => {
                self.instructions.push(Instr::MovImm {
                    dst: preferred,
                    value,
                });
                preferred
            }
            ValueLoc::StackSlot { slot, .. } => {
                self.instructions.push(Instr::LoadStack {
                    dst: preferred,
                    slot,
                });
                preferred
            }
        }
    }

    fn store_value(&mut self, value: ValueId, reg: PhysReg, repr: MachineRepr) {
        if repr.is_zero_sized() {
            self.values
                .insert(value, ValueLoc::Immediate { value: 0, repr });
            return;
        }
        let slot = self.alloc_slot(value);
        self.instructions.push(Instr::StoreStack { slot, src: reg });
        self.values
            .insert(value, ValueLoc::StackSlot { slot, repr });
    }

    fn emit_constant_to(&mut self, value: &ssa::Constant, dst: PhysReg) {
        match value {
            ssa::Constant::Unit => self.instructions.push(Instr::MovImm { dst, value: 0 }),
            ssa::Constant::ConstructorTag { tag, .. } => self.instructions.push(Instr::MovImm {
                dst,
                value: i64::from(*tag),
            }),
            ssa::Constant::Literal(Literal::Int(value)) => match value.parse::<i64>() {
                Ok(value) => self.instructions.push(Instr::MovImm { dst, value }),
                Err(_) => {
                    self.diagnostics.push(Diagnostic::UnsupportedLiteral {
                        literal: value.clone(),
                    });
                    self.instructions.push(Instr::MovImm { dst, value: 0 });
                }
            },
            ssa::Constant::Literal(Literal::Bool(value)) => self.instructions.push(Instr::MovImm {
                dst,
                value: i64::from(*value),
            }),
            ssa::Constant::Literal(Literal::String(_) | Literal::Float(_)) => {
                self.instructions.push(Instr::MovImm { dst, value: 0 });
            }
        }
    }
}

fn constant_i64(value: &ssa::Constant) -> Option<i64> {
    match value {
        ssa::Constant::Unit => Some(0),
        ssa::Constant::ConstructorTag { tag, .. } => Some(i64::from(*tag)),
        ssa::Constant::Literal(Literal::Int(value)) => value.parse().ok(),
        ssa::Constant::Literal(Literal::Bool(value)) => Some(i64::from(*value)),
        ssa::Constant::Literal(Literal::String(_) | Literal::Float(_)) => None,
    }
}

fn block_tag_value(tag: &ssa::BlockTag) -> i64 {
    match tag {
        ssa::BlockTag::Tuple | ssa::BlockTag::Record => 0,
        ssa::BlockTag::Variant(tag) => i64::from(*tag),
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
        checker::{self, tir},
        parser::{ast::Literal, ident::Ident, lexer::Span},
    };

    fn value_info(repr: MachineRepr) -> ssa::ValueInfo {
        ssa::ValueInfo {
            repr,
            ty: checker::builtin::i32(),
            debug_name: None,
            span: Span::new(0, 0),
        }
    }

    fn function(stmts: Vec<ssa::Stmt>, ret: ValueId) -> ssa::SsaProgram {
        let name = Ident::from_string("answer");
        ssa::SsaProgram {
            globals: Vec::new(),
            functions: vec![ssa::Function {
                binding: tir::BindingId::synthetic(0),
                entity: tir::EntityId::synthetic_value(0),
                name: name.clone(),
                symbol: ssa::Symbol::from_ident(&name),
                params: Vec::new(),
                values: vec![
                    value_info(MachineRepr::I32),
                    value_info(MachineRepr::I32),
                    value_info(MachineRepr::I32),
                ],
                entry: ssa::BlockId(0),
                blocks: vec![ssa::Block {
                    id: ssa::BlockId(0),
                    params: Vec::new(),
                    stmts,
                    terminator: ssa::Terminator::Return(Some(ret)),
                }],
                ret: MachineRepr::I32,
                ty: checker::builtin::i32(),
                span: Span::new(0, 0),
            }],
        }
    }

    #[test]
    fn selects_ssa_integer_arithmetic() {
        let module = function(
            vec![
                ssa::Stmt::Assign {
                    dst: ValueId(0),
                    rhs: ssa::Rvalue::Const(ssa::Constant::Literal(Literal::Int("1".into()))),
                    span: Span::new(0, 0),
                },
                ssa::Stmt::Assign {
                    dst: ValueId(1),
                    rhs: ssa::Rvalue::Const(ssa::Constant::Literal(Literal::Int("2".into()))),
                    span: Span::new(0, 0),
                },
                ssa::Stmt::Assign {
                    dst: ValueId(2),
                    rhs: ssa::Rvalue::Prim {
                        op: lambda::ir::Primitive::Add {
                            operand: checker::builtin::i32(),
                        },
                        args: vec![ValueId(0), ValueId(1)],
                    },
                    span: Span::new(0, 0),
                },
            ],
            ValueId(2),
        );
        let mir = select(&module);
        assert!(
            mir.entries[0]
                .instructions
                .iter()
                .any(|instr| matches!(instr, Instr::Add { .. }))
        );
    }
}
