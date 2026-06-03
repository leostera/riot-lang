//! AArch64 assembly emission.

use std::fmt;

use crate::backend::native::ssa::Symbol;

use super::mir::{Entry, Instr, Mir, PhysReg};

impl Mir {
    pub fn emit(&self, sink: &mut impl fmt::Write) -> fmt::Result {
        Emitter::new(self).emit(sink)
    }

    pub fn to_asm_string(&self) -> String {
        Emitter::new(self).to_string()
    }
}

pub struct Emitter<'a> {
    module: &'a Mir,
}

impl<'a> Emitter<'a> {
    pub fn new(module: &'a Mir) -> Self {
        Self { module }
    }

    pub fn emit(&self, sink: &mut impl fmt::Write) -> fmt::Result {
        sink.write_str(".text\n")?;
        for entry in &self.module.entries {
            self.emit_entry(sink, entry)?;
        }
        if !self.module.diagnostics.is_empty() {
            sink.write_str("\n// diagnostics\n")?;
            for diagnostic in &self.module.diagnostics {
                writeln!(sink, "// {diagnostic:?}")?;
            }
        }
        Ok(())
    }

    pub fn to_string(&self) -> String {
        let mut out = String::new();
        self.emit(&mut out).expect("write to string");
        out
    }

    fn emit_entry(&self, sink: &mut impl fmt::Write, entry: &Entry) -> fmt::Result {
        let symbol = asm_symbol(&entry.name);
        writeln!(sink, ".globl {symbol}")?;
        writeln!(sink, "{symbol}:")?;
        self.emit_prologue(sink, entry)?;
        for instruction in &entry.instructions {
            self.emit_instr(sink, instruction)?;
        }
        Ok(())
    }

    fn emit_prologue(&self, sink: &mut impl fmt::Write, entry: &Entry) -> fmt::Result {
        // Keep prologue generation in the target MIR emitter, like Zig's target
        // MIR emitters. Frame metadata is already part of AArch64 MIR.
        writeln!(sink, "  stp x29, x30, [sp, #-16]!")?;
        writeln!(sink, "  mov x29, sp")?;
        let frame_size = aligned_frame_size(entry.stack_slots);
        if frame_size > 0 {
            writeln!(sink, "  sub sp, sp, #{frame_size}")?;
        }
        Ok(())
    }

    fn emit_instr(&self, sink: &mut impl fmt::Write, instr: &Instr) -> fmt::Result {
        match instr {
            Instr::Label(_) => Ok(()),
            Instr::BlockLabel(label) => writeln!(sink, "{label}:"),
            Instr::MovImm { dst, value } => writeln!(sink, "  mov {}, #{value}", reg(*dst)),
            Instr::Mov { dst, src } => writeln!(sink, "  mov {}, {}", reg(*dst), reg(*src)),
            Instr::LoadStack { dst, slot } => {
                writeln!(sink, "  ldr {}, [sp, #{}]", reg(*dst), slot_offset(*slot))
            }
            Instr::StoreStack { slot, src } => {
                writeln!(sink, "  str {}, [sp, #{}]", reg(*src), slot_offset(*slot))
            }
            Instr::Add { dst, lhs, rhs } => {
                writeln!(sink, "  add {}, {}, {}", reg(*dst), reg(*lhs), reg(*rhs))
            }
            Instr::Sub { dst, lhs, rhs } => {
                writeln!(sink, "  sub {}, {}, {}", reg(*dst), reg(*lhs), reg(*rhs))
            }
            Instr::Mul { dst, lhs, rhs } => {
                writeln!(sink, "  mul {}, {}, {}", reg(*dst), reg(*lhs), reg(*rhs))
            }
            Instr::SDiv { dst, lhs, rhs } => {
                writeln!(sink, "  sdiv {}, {}, {}", reg(*dst), reg(*lhs), reg(*rhs))
            }
            Instr::MSub { dst, lhs, rhs, acc } => writeln!(
                sink,
                "  msub {}, {}, {}, {}",
                reg(*dst),
                reg(*lhs),
                reg(*rhs),
                reg(*acc)
            ),
            Instr::CmpImm { lhs, imm } => writeln!(sink, "  cmp {}, #{imm}", reg(*lhs)),
            Instr::BCond { cond, label } => writeln!(sink, "  b.{cond} {label}"),
            Instr::B { label } => writeln!(sink, "  b {label}"),
            Instr::Call { symbol } => writeln!(sink, "  bl {}", asm_symbol(symbol)),
            Instr::Ret => {
                writeln!(sink, "  mov sp, x29")?;
                writeln!(sink, "  ldp x29, x30, [sp], #16")?;
                writeln!(sink, "  ret")
            }
            Instr::Comment(comment) => writeln!(sink, "  // {comment}"),
        }
    }
}

fn aligned_frame_size(stack_slots: usize) -> usize {
    let bytes = stack_slots * 8;
    if bytes == 0 {
        0
    } else {
        bytes.div_ceil(16) * 16
    }
}

fn slot_offset(slot: usize) -> usize {
    slot * 8
}

fn reg(reg: PhysReg) -> String {
    match reg {
        PhysReg::X(index) => format!("x{index}"),
    }
}

fn asm_symbol(symbol: &Symbol) -> String {
    format!("_{symbol}")
}

#[cfg(test)]
mod tests {
    use super::super::{Entry, Instr, Mir, PhysReg};
    use crate::backend::native::ssa::Symbol;

    #[test]
    fn emits_function_prologue_and_return() {
        let mir = Mir {
            entries: vec![Entry {
                name: Symbol::new("answer"),
                instructions: vec![
                    Instr::Label(Symbol::new("answer")),
                    Instr::MovImm {
                        dst: PhysReg::X(0),
                        value: 42,
                    },
                    Instr::Ret,
                ],
                stack_slots: 0,
            }],
            diagnostics: Vec::new(),
        };
        insta::assert_snapshot!(mir.to_asm_string(), @r#"
.text
.globl _answer
_answer:
  stp x29, x30, [sp, #-16]!
  mov x29, sp
  mov x0, #42
  mov sp, x29
  ldp x29, x30, [sp], #16
  ret
"#);
    }
}
