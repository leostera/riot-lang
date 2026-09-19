//! TypeScript source emitter for the JS backend IR.
//!
//! The emitter projects one combined backend IR into `.ts` text, preserving the
//! lowered runtime declarations and type declarations in stable snapshot-friendly
//! order.

use std::fmt;

use super::ir::{self, BinaryOp, TsType};

pub struct Emitter<'a> {
    module: &'a ir::Module,
}

impl<'a> Emitter<'a> {
    pub fn new(module: &'a ir::Module) -> Self {
        Self { module }
    }

    pub fn emit_js(&self, sink: &mut impl fmt::Write) -> fmt::Result {
        let mut index = 0;
        for decl in self.module.items.iter().filter(|item| item.is_js_item()) {
            if index > 0 {
                sink.write_char('\n')?;
            }
            emit_js_decl(sink, decl)?;
            sink.write_char('\n')?;
            index += 1;
        }
        Ok(())
    }

    pub fn emit_ts(&self, sink: &mut impl fmt::Write) -> fmt::Result {
        let mut index = 0;
        for item in self
            .module
            .items
            .iter()
            .filter(|item| !matches!(item, ir::Item::TypeValue { .. }))
        {
            if index > 0 {
                sink.write_char('\n')?;
            }
            match item {
                ir::Item::TypeAlias { .. } => emit_ts_decl(sink, item)?,
                _ => emit_ts_runtime_item(sink, item, self.type_for_item(item))?,
            }
            sink.write_char('\n')?;
            index += 1;
        }
        Ok(())
    }

    pub fn to_ts_string(&self) -> String {
        let mut out = String::new();
        self.emit_ts(&mut out).expect("write to string");
        out
    }

    fn type_for_item(&self, item: &ir::Item) -> Option<&TsType> {
        let item_name = match item {
            ir::Item::Const { name, .. } | ir::Item::Function { name, .. } => name,
            _ => return None,
        };
        self.module
            .items
            .iter()
            .find_map(|candidate| match candidate {
                ir::Item::TypeValue { name, ty, .. } if name == item_name => Some(ty),
                _ => None,
            })
    }
}

impl ir::Item {
    fn is_js_item(&self) -> bool {
        matches!(
            self,
            Self::Import { .. } | Self::Const { .. } | Self::Function { .. } | Self::Export { .. }
        )
    }
}

fn emit_ts_runtime_item(
    out: &mut impl fmt::Write,
    decl: &ir::Item,
    ty: Option<&TsType>,
) -> fmt::Result {
    match decl {
        ir::Item::Const {
            name,
            ty: item_ty,
            value,
        } => {
            write!(out, "const {name}")?;
            if let Some(ty) = item_ty.as_ref().or(ty) {
                out.write_str(": ")?;
                emit_type(out, ty, false)?;
            }
            out.write_str(" = ")?;
            emit_expr(out, value)?;
            out.write_char(';')
        }
        _ => emit_decl(out, decl),
    }
}

fn emit_js_decl(out: &mut impl fmt::Write, decl: &ir::Item) -> fmt::Result {
    match decl {
        ir::Item::Function { name, params, body, .. } => {
            write!(out, "function {name}(")?;
            for (index, param) in params.iter().enumerate() {
                if index > 0 { out.write_str(", ")?; }
                out.write_str(&param.name)?;
            }
            out.write_str(") {")?;
            emit_stmt_block_contents(out, body, 1)?;
            out.write_char('\n')?;
            out.write_char('}')
        }
        _ => emit_decl(out, decl),
    }
}

fn emit_decl(out: &mut impl fmt::Write, decl: &ir::Item) -> fmt::Result {
    match decl {
        ir::Item::Import { names, from } => {
            out.write_str("import { ")?;
            for (index, name) in names.iter().enumerate() {
                if index > 0 {
                    out.write_str(", ")?;
                }
                out.write_str(&name.imported)?;
                if name.imported != name.local {
                    write!(out, " as {}", name.local)?;
                }
            }
            out.write_str(" } from ")?;
            emit_string(out, from)?;
            out.write_char(';')
        }
        ir::Item::Const { name, value, .. } => {
            write!(out, "const {name} = ")?;
            emit_expr(out, value)?;
            out.write_char(';')
        }
        ir::Item::Function {
            name,
            params,
            return_type,
            body,
        } => {
            emit_function_header(out, name, params, return_type.as_ref())?;
            emit_stmt_block_contents(out, body, 1)?;
            out.write_char('\n')?;
            out.write_char('}')
        }
        ir::Item::Export { names } => write!(out, "export {{ {} }};", names.join(", ")),
        _ => Ok(()),
    }
}

fn emit_function_header(
    out: &mut impl fmt::Write,
    name: &str,
    params: &[ir::Param],
    return_type: Option<&TsType>,
) -> fmt::Result {
    write!(out, "function {name}(")?;
    for (index, param) in params.iter().enumerate() {
        if index > 0 {
            out.write_str(", ")?;
        }
        out.write_str(&param.name)?;
        if let Some(ty) = &param.ty {
            out.write_str(": ")?;
            emit_type(out, ty, false)?;
        }
    }
    out.write_char(')')?;
    if let Some(return_type) = return_type {
        out.write_str(": ")?;
        emit_type(out, return_type, false)?;
    }
    out.write_str(" {")
}

fn emit_stmt_block_contents(
    out: &mut impl fmt::Write,
    stmts: &[ir::Stmt],
    indent: usize,
) -> fmt::Result {
    for stmt in stmts {
        out.write_char('\n')?;
        emit_indent(out, indent)?;
        emit_stmt(out, stmt, indent)?;
    }
    Ok(())
}

fn emit_stmt(out: &mut impl fmt::Write, stmt: &ir::Stmt, indent: usize) -> fmt::Result {
    match stmt {
        ir::Stmt::Const { name, value } => {
            write!(out, "const {name} = ")?;
            emit_expr(out, value)?;
            out.write_char(';')
        }
        ir::Stmt::Expr(expr) => {
            emit_expr(out, expr)?;
            out.write_char(';')
        }
        ir::Stmt::Return(expr) => {
            out.write_str("return ")?;
            emit_expr(out, expr)?;
            out.write_char(';')
        }
        ir::Stmt::Block(stmts) => {
            out.write_char('{')?;
            emit_stmt_block_contents(out, stmts, indent + 1)?;
            out.write_char('\n')?;
            emit_indent(out, indent)?;
            out.write_char('}')
        }
        ir::Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            out.write_str("if (")?;
            emit_expr(out, condition)?;
            out.write_str(") {")?;
            emit_stmt_block_contents(out, then_branch, indent + 1)?;
            out.write_char('\n')?;
            emit_indent(out, indent)?;
            out.write_char('}')?;
            if !else_branch.is_empty() {
                out.write_str(" else {")?;
                emit_stmt_block_contents(out, else_branch, indent + 1)?;
                out.write_char('\n')?;
                emit_indent(out, indent)?;
                out.write_char('}')?;
            }
            Ok(())
        }
        ir::Stmt::Switch {
            discriminant,
            cases,
            default,
        } => {
            out.write_str("switch (")?;
            emit_expr(out, discriminant)?;
            out.write_str(") {")?;
            for case in cases {
                out.write_char('\n')?;
                emit_indent(out, indent + 1)?;
                out.write_str("case ")?;
                emit_expr(out, &case.test)?;
                out.write_char(':')?;
                emit_stmt_block_contents(out, &case.body, indent + 2)?;
            }
            if !default.is_empty() {
                out.write_char('\n')?;
                emit_indent(out, indent + 1)?;
                out.write_str("default:")?;
                emit_stmt_block_contents(out, default, indent + 2)?;
            }
            out.write_char('\n')?;
            emit_indent(out, indent)?;
            out.write_char('}')
        }
    }
}

fn emit_expr(out: &mut impl fmt::Write, expr: &ir::Expr) -> fmt::Result {
    match expr {
        ir::Expr::Number(value) => out.write_str(value),
        ir::Expr::String(value) => emit_string(out, value),
        ir::Expr::Bool(value) => out.write_str(if *value { "true" } else { "false" }),
        ir::Expr::Undefined => out.write_str("undefined"),
        ir::Expr::Null => out.write_str("null"),
        ir::Expr::Var(name) => out.write_str(name),
        ir::Expr::Function { params, body } => {
            write!(out, "({}) => {{", params.join(", "))?;
            emit_stmt_block_contents(out, body, 1)?;
            out.write_char('\n')?;
            out.write_char('}')
        }
        ir::Expr::Call { callee, args } => {
            let wrap_callee = matches!(callee.as_ref(), ir::Expr::Function { .. });
            if wrap_callee {
                out.write_char('(')?;
            }
            emit_expr(out, callee)?;
            if wrap_callee {
                out.write_char(')')?;
            }
            out.write_char('(')?;
            for (index, arg) in args.iter().enumerate() {
                if index > 0 {
                    out.write_str(", ")?;
                }
                emit_expr(out, arg)?;
            }
            out.write_char(')')
        }
        ir::Expr::Binary { op, left, right } => {
            out.write_char('(')?;
            emit_expr(out, left)?;
            write!(out, " {} ", op.as_js())?;
            emit_expr(out, right)?;
            out.write_char(')')
        }
        ir::Expr::Array(items) => {
            out.write_char('[')?;
            for (index, item) in items.iter().enumerate() {
                if index > 0 {
                    out.write_str(", ")?;
                }
                emit_expr(out, item)?;
            }
            out.write_char(']')
        }
        ir::Expr::Object(properties) => {
            out.write_str("{ ")?;
            for (index, property) in properties.iter().enumerate() {
                if index > 0 {
                    out.write_str(", ")?;
                }
                write!(out, "{}: ", property.name)?;
                emit_expr(out, &property.value)?;
            }
            out.write_str(" }")
        }
        ir::Expr::Member { object, property } => {
            emit_expr(out, object)?;
            if is_valid_js_property(property) {
                write!(out, ".{property}")
            } else {
                out.write_char('[')?;
                emit_string(out, property)?;
                out.write_char(']')
            }
        }
    }
}

fn emit_ts_decl(out: &mut impl fmt::Write, decl: &ir::Item) -> fmt::Result {
    match decl {
        ir::Item::TypeAlias {
            name,
            params,
            ty,
            exported,
        } => {
            if *exported {
                out.write_str("export ")?;
            }
            write!(out, "type {name}")?;
            if !params.is_empty() {
                write!(out, "<{}>", params.join(", "))?;
            }
            out.write_str(" = ")?;
            emit_type(out, ty, false)?;
            out.write_char(';')
        }
        _ => Ok(()),
    }
}

fn emit_type(out: &mut impl fmt::Write, ty: &TsType, nested: bool) -> fmt::Result {
    match ty {
        TsType::Number => out.write_str("number"),
        TsType::Boolean => out.write_str("boolean"),
        TsType::String => out.write_str("string"),
        TsType::Undefined => out.write_str("undefined"),
        TsType::Null => out.write_str("null"),
        TsType::Void => out.write_str("void"),
        TsType::Unknown => out.write_str("unknown"),
        TsType::TypeParam(name) => out.write_str(name),
        TsType::Tuple(items) => {
            out.write_str("readonly [")?;
            for (index, item) in items.iter().enumerate() {
                if index > 0 {
                    out.write_str(", ")?;
                }
                emit_type(out, item, false)?;
            }
            out.write_char(']')
        }
        TsType::Function { params, result } => {
            if nested {
                out.write_char('(')?;
            }
            out.write_char('(')?;
            for (index, param) in params.iter().enumerate() {
                if index > 0 {
                    out.write_str(", ")?;
                }
                write!(out, "arg{index}: ")?;
                emit_type(out, param, false)?;
            }
            out.write_str(") => ")?;
            emit_type(out, result, true)?;
            if nested {
                out.write_char(')')?;
            }
            Ok(())
        }
        TsType::Object(fields) => {
            out.write_str("{ ")?;
            for (index, field) in fields.iter().enumerate() {
                if index > 0 {
                    out.write_str("; ")?;
                }
                write!(
                    out,
                    "readonly {}{}: ",
                    field.name,
                    if field.optional { "?" } else { "" }
                )?;
                emit_type(out, &field.ty, false)?;
            }
            out.write_str(" }")
        }
        TsType::Union(types) => {
            if nested {
                out.write_char('(')?;
            }
            for (index, ty) in types.iter().enumerate() {
                if index > 0 {
                    out.write_str(" | ")?;
                }
                emit_type(out, ty, true)?;
            }
            if nested {
                out.write_char(')')?;
            }
            Ok(())
        }
        TsType::Named { name, args } => {
            out.write_str(name)?;
            if !args.is_empty() {
                out.write_char('<')?;
                for (index, arg) in args.iter().enumerate() {
                    if index > 0 {
                        out.write_str(", ")?;
                    }
                    emit_type(out, arg, false)?;
                }
                out.write_char('>')?;
            }
            Ok(())
        }
    }
}

impl BinaryOp {
    fn as_js(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::Eq => "===",
            Self::Ne => "!==",
            Self::Lt => "<",
            Self::Lte => "<=",
            Self::Gt => ">",
            Self::Gte => ">=",
        }
    }
}

fn emit_indent(out: &mut impl fmt::Write, indent: usize) -> fmt::Result {
    for _ in 0..indent {
        out.write_str("  ")?;
    }
    Ok(())
}

fn is_valid_js_property(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|ch| ch == '_' || ch == '$' || ch.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch == '$' || ch.is_ascii_alphanumeric())
}

fn emit_string(out: &mut impl fmt::Write, value: &str) -> fmt::Result {
    out.write_char('"')?;
    for ch in value.chars() {
        match ch {
            '\\' => out.write_str("\\\\")?,
            '"' => out.write_str("\\\"")?,
            '\n' => out.write_str("\\n")?,
            '\r' => out.write_str("\\r")?,
            '\t' => out.write_str("\\t")?,
            _ => out.write_char(ch)?,
        }
    }
    out.write_char('"')
}
