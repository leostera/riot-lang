use std::collections::BTreeSet;

use crate::signature::{RsigType, TypeName};

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct RsigTypeParser;

impl RsigTypeParser {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn parse_signature_with_variants(
        &self,
        text: &str,
        variants: &BTreeSet<TypeName>,
    ) -> (Vec<RsigType>, RsigType) {
        let mut parts = split_top_level_arrows(text);
        if parts.len() < 2 {
            return (Vec::new(), self.parse_with_variants(text, variants));
        }
        let result = self.parse_with_variants(parts.pop().unwrap_or("_"), variants);
        let params = parts
            .into_iter()
            .map(|part| self.parse_with_variants(part, variants))
            .collect();
        (params, result)
    }

    pub(crate) fn parse(&self, text: &str) -> RsigType {
        let text = text.trim();
        if text == "()" {
            return RsigType::Unit;
        }
        if let Some(inner) = strip_wrapping_parens(text)
            && split_top_level_arrows(inner).len() > 1
        {
            return self.parse(inner);
        }
        let mut arrow_parts = split_top_level_arrows(text);
        if arrow_parts.len() > 1 {
            let result = self.parse(arrow_parts.pop().unwrap_or("_"));
            return arrow_parts
                .into_iter()
                .rev()
                .fold(result, |result, parameter| RsigType::Arrow {
                    parameter: Box::new(self.parse(parameter)),
                    result: Box::new(result),
                });
        }
        if let Some((name, args)) = parse_named_type_app(text)
            && name == "List"
        {
            let args = split_top_level_commas(args);
            if let [item] = args.as_slice() {
                return RsigType::List(Box::new(self.parse(item)));
            }
        }
        match text {
            "bool" => RsigType::Bool,
            "char" => RsigType::Char,
            "f64" | "float" => RsigType::F64,
            "i32" => RsigType::I32,
            "i64" | "int" => RsigType::I64,
            "String" => RsigType::String,
            "unit" => RsigType::Unit,
            "_" | "" => RsigType::Unknown,
            _ if text.starts_with('\'') => RsigType::Var(text.to_owned()),
            _ if text.starts_with("actor_id<") && text.ends_with('>') => {
                let inner = &text[9..text.len() - 1];
                RsigType::ActorId(Box::new(self.parse(inner)))
            }
            _ if text.starts_with('(') && text.ends_with(')') && text.contains(',') => {
                let inner = &text[1..text.len() - 1];
                RsigType::Tuple(
                    split_top_level_commas(inner)
                        .into_iter()
                        .map(|item| self.parse(item))
                        .collect(),
                )
            }
            _ => RsigType::Record(TypeName::new(text)),
        }
    }

    pub(crate) fn parse_with_variants(
        &self,
        text: &str,
        variants: &BTreeSet<TypeName>,
    ) -> RsigType {
        let text = text.trim();
        if let Some((name, args)) = parse_named_type_app(text)
            && name == "List"
        {
            let args = split_top_level_commas(args);
            if let [item] = args.as_slice() {
                return RsigType::List(Box::new(self.parse_with_variants(item, variants)));
            }
        }
        if let Some((name, args)) = parse_named_type_app(text) {
            let name = TypeName::new(name);
            let args = split_top_level_commas(args)
                .into_iter()
                .map(|arg| self.parse_with_variants(arg, variants))
                .collect::<Vec<_>>();
            if variants.contains(&name) {
                return RsigType::VariantApp { name, args };
            }
        }
        resolve_declared_variants(self.parse(text), variants)
    }
}

fn resolve_declared_variants(type_: RsigType, variants: &BTreeSet<TypeName>) -> RsigType {
    match type_ {
        RsigType::ActorId(message) => {
            RsigType::ActorId(Box::new(resolve_declared_variants(*message, variants)))
        }
        RsigType::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(resolve_declared_variants(*parameter, variants)),
            result: Box::new(resolve_declared_variants(*result, variants)),
        },
        RsigType::List(element) => {
            RsigType::List(Box::new(resolve_declared_variants(*element, variants)))
        }
        RsigType::Tuple(items) => RsigType::Tuple(
            items
                .into_iter()
                .map(|item| resolve_declared_variants(item, variants))
                .collect(),
        ),
        RsigType::VariantApp { name, args } => RsigType::VariantApp {
            name,
            args: args
                .into_iter()
                .map(|arg| resolve_declared_variants(arg, variants))
                .collect(),
        },
        RsigType::Record(name) => {
            if variants.contains(&name) {
                RsigType::Variant(name)
            } else {
                RsigType::Record(name)
            }
        }
        other => other,
    }
}

fn strip_wrapping_parens(text: &str) -> Option<&str> {
    let inner = text.strip_prefix('(')?.strip_suffix(')')?;
    let mut depth = 0_i32;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 && index + ch.len_utf8() < text.len() {
                    return None;
                }
            }
            _ => {}
        }
    }
    Some(inner)
}

fn parse_named_type_app(text: &str) -> Option<(&str, &str)> {
    let (name, rest) = text.split_once('<')?;
    let args = rest.strip_suffix('>')?;
    let name = name.trim();
    if name.is_empty() || name == "actor_id" {
        return None;
    }
    Some((name, args))
}

fn split_top_level_arrows(text: &str) -> Vec<&str> {
    let bytes = text.as_bytes();
    let mut depth = 0_i32;
    let mut start = 0;
    let mut parts = Vec::new();
    let mut index = 0;
    while index < bytes.len() {
        match bytes[index] {
            b'(' | b'<' => depth += 1,
            b')' => depth -= 1,
            b'>' if index == 0 || bytes[index - 1] != b'-' => depth -= 1,
            b'-' if depth == 0 && bytes.get(index + 1) == Some(&b'>') => {
                parts.push(text[start..index].trim());
                index += 2;
                start = index;
                continue;
            }
            _ => {}
        }
        index += 1;
    }
    parts.push(text[start..].trim());
    parts
}

fn split_top_level_commas(text: &str) -> Vec<&str> {
    let mut depth = 0_i32;
    let mut start = 0;
    let mut parts = Vec::new();
    for (index, ch) in text.char_indices() {
        match ch {
            '(' | '<' => depth += 1,
            ')' | '>' => depth -= 1,
            ',' if depth == 0 => {
                parts.push(text[start..index].trim());
                start = index + ch.len_utf8();
            }
            _ => {}
        }
    }
    parts.push(text[start..].trim());
    parts
}
