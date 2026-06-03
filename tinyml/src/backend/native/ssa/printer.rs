use crate::{lambda::ir::Primitive, parser::ast};

use super::*;

impl std::fmt::Display for SsaProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for global in &self.globals {
            writeln!(
                f,
                "global {}: {} = {};",
                global.symbol, global.repr, global.value
            )?;
        }
        if !self.globals.is_empty() && !self.functions.is_empty() {
            writeln!(f)?;
        }
        for (index, function) in self.functions.iter().enumerate() {
            if index > 0 {
                writeln!(f)?;
            }
            function.fmt(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.symbol)?;
        for (index, param) in self.params.iter().enumerate() {
            if index > 0 {
                write!(f, ", ")?;
            }
            write!(
                f,
                "{}: {}",
                self.value_name(*param),
                self.value_repr(*param)
            )?;
        }
        writeln!(f, ") -> {} {{", self.ret)?;
        for block in &self.blocks {
            self.fmt_block(f, block)?;
        }
        writeln!(f, "}}")
    }
}

impl Function {
    fn fmt_block(&self, f: &mut std::fmt::Formatter<'_>, block: &Block) -> std::fmt::Result {
        write!(f, "{}", block_name(block.id))?;
        if !block.params.is_empty() {
            write!(f, "(")?;
            for (index, param) in block.params.iter().enumerate() {
                if index > 0 {
                    write!(f, ", ")?;
                }
                write!(
                    f,
                    "{}: {}",
                    self.value_name(*param),
                    self.value_repr(*param)
                )?;
            }
            write!(f, ")")?;
        }
        writeln!(f, ":")?;
        for stmt in &block.stmts {
            self.fmt_stmt(f, stmt)?;
        }
        self.fmt_terminator(f, &block.terminator)
    }

    fn fmt_stmt(&self, f: &mut std::fmt::Formatter<'_>, stmt: &Stmt) -> std::fmt::Result {
        let Stmt::Assign { dst, rhs, .. } = stmt;
        writeln!(
            f,
            "  {}: {} = {};",
            self.value_name(*dst),
            self.value_repr(*dst),
            self.rvalue_string(rhs)
        )
    }

    fn fmt_terminator(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        terminator: &Terminator,
    ) -> std::fmt::Result {
        match terminator {
            Terminator::Return(Some(value)) => writeln!(f, "  return {};", self.value_name(*value)),
            Terminator::Return(None) => writeln!(f, "  return;"),
            Terminator::Jump { target, args } => {
                writeln!(
                    f,
                    "  jump {}({});",
                    block_name(*target),
                    self.value_list(args)
                )
            }
            Terminator::Branch {
                cond,
                then_target,
                then_args,
                else_target,
                else_args,
            } => writeln!(
                f,
                "  branch {} ? {}({}) : {}({});",
                self.value_name(*cond),
                block_name(*then_target),
                self.value_list(then_args),
                block_name(*else_target),
                self.value_list(else_args)
            ),
            Terminator::Switch {
                discr,
                cases,
                default,
                default_args,
            } => {
                write!(f, "  switch {} [", self.value_name(*discr))?;
                for (index, (tag, target, args)) in cases.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{tag} => {}({})",
                        block_name(*target),
                        self.value_list(args)
                    )?;
                }
                writeln!(
                    f,
                    "] default {}({});",
                    block_name(*default),
                    self.value_list(default_args)
                )
            }
        }
    }

    fn rvalue_string(&self, rhs: &Rvalue) -> String {
        match rhs {
            Rvalue::Const(value) => value.to_string(),
            Rvalue::Copy(value) => self.value_name(*value),
            Rvalue::Prim { op, args } => {
                format!("{}({})", primitive_name(op), self.value_list(args))
            }
            Rvalue::Call { callee, args } => format!("call {}({})", callee, self.value_list(args)),
            Rvalue::Global(symbol) => format!("global {symbol}"),
            Rvalue::MakeBlock { tag, fields } => {
                format!(
                    "make_block({}, {})",
                    block_tag_name(tag),
                    self.value_list(fields)
                )
            }
            Rvalue::GetField { block, index } => {
                format!("get_field({}, {index})", self.value_name(*block))
            }
            Rvalue::GetTag { block } => format!("get_tag({})", self.value_name(*block)),
            Rvalue::StringLiteral { value } => format!("string {value:?}"),
            Rvalue::FloatLiteral { value } => format!("float {value}"),
            Rvalue::RuntimeMatch {
                scrutinee,
                arm_count,
            } => format!(
                "match_runtime({}, arms = {arm_count})",
                self.value_name(*scrutinee)
            ),
        }
    }

    fn value_list(&self, values: &[ValueId]) -> String {
        values
            .iter()
            .map(|value| self.value_name(*value))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn value_name(&self, value: ValueId) -> String {
        let Some(info) = self.values.get(value.0 as usize) else {
            return format!("%v{}", value.0);
        };
        let Some(name) = &info.debug_name else {
            return format!("%v{}", value.0);
        };
        match name.as_name() {
            Some(name) if name != "_" => format!("%{name}{}", value.0),
            _ => format!("%v{}", value.0),
        }
    }

    fn value_repr(&self, value: ValueId) -> MachineRepr {
        self.values
            .get(value.0 as usize)
            .map(|info| info.repr)
            .unwrap_or(MachineRepr::Ptr)
    }
}

fn block_name(block: BlockId) -> String {
    format!("block{}", block.0)
}

fn block_tag_name(tag: &BlockTag) -> String {
    match tag {
        BlockTag::Tuple => "tuple".to_string(),
        BlockTag::Record => "record".to_string(),
        BlockTag::Variant(tag) => format!("variant#{tag}"),
    }
}

fn primitive_name(op: &Primitive) -> &'static str {
    match op {
        Primitive::Add { .. } => "add",
        Primitive::Sub { .. } => "sub",
        Primitive::Mul { .. } => "mul",
        Primitive::Div { .. } => "div",
        Primitive::Rem { .. } => "rem",
        Primitive::Named(_) => "primitive",
    }
}

impl std::fmt::Display for MachineRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MachineRepr::Unit => f.write_str("unit"),
            MachineRepr::Bool => f.write_str("bool"),
            MachineRepr::I32 => f.write_str("i32"),
            MachineRepr::I64 => f.write_str("i64"),
            MachineRepr::F32 => f.write_str("f32"),
            MachineRepr::F64 => f.write_str("f64"),
            MachineRepr::Ptr => f.write_str("ptr"),
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Unit => f.write_str("()"),
            Constant::Literal(ast::Literal::Int(value)) => f.write_str(value),
            Constant::Literal(ast::Literal::Bool(value)) => write!(f, "{value}"),
            Constant::Literal(ast::Literal::Float(value)) => f.write_str(value),
            Constant::Literal(ast::Literal::String(value)) => write!(f, "{value:?}"),
            Constant::ConstructorTag { name, tag } => match name.as_name() {
                Some(name) => write!(f, "constructor_tag {name}#{tag}"),
                None => write!(f, "constructor_tag #{tag}"),
            },
        }
    }
}
