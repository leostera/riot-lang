//! Type-hint and hover extraction from checked TIR.

use crate::{
    checker::{ModuleSummary, scheme::Scheme, tir},
    parser::lexer::Span,
};
use tower_lsp::lsp_types::{
    Hover, HoverContents, InlayHint, InlayHintKind, InlayHintLabel, MarkupContent, MarkupKind,
    Position, Range,
};

use super::position::{byte_to_position, position_to_byte, span_to_range};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeFact {
    pub span: Span,
    pub label: String,
}

pub fn inlay_hints(source: &str, summary: &ModuleSummary) -> Vec<InlayHint> {
    binding_type_facts(summary)
        .into_iter()
        .map(|fact| InlayHint {
            position: byte_to_position(source, fact.span.end),
            label: InlayHintLabel::String(format!(": {}", fact.label)),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: Some(false),
            padding_right: Some(true),
            data: None,
        })
        .collect()
}

pub fn hover(source: &str, summary: &ModuleSummary, position: Position) -> Option<Hover> {
    let byte = position_to_byte(source, position);
    let fact = type_facts(summary)
        .into_iter()
        .filter(|fact| fact.span.start <= byte && byte <= fact.span.end)
        .min_by_key(|fact| fact.span.end.saturating_sub(fact.span.start))?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```tinyml\n{}\n```", fact.label),
        }),
        range: Some(span_to_range(source, fact.span)),
    })
}

pub fn type_facts(summary: &ModuleSummary) -> Vec<TypeFact> {
    let mut facts = Vec::new();
    for item in &summary.tir.items {
        collect_item(item, &mut facts);
    }
    facts
}

pub fn binding_type_facts(summary: &ModuleSummary) -> Vec<TypeFact> {
    let mut facts = Vec::new();
    for item in &summary.tir.items {
        collect_item_bindings(item, &mut facts);
    }
    facts
}

fn collect_item(item: &tir::ModuleItem, facts: &mut Vec<TypeFact>) {
    match item {
        tir::ModuleItem::LetDecl(decl) => collect_let_decl(decl, facts),
        tir::ModuleItem::UseDecl(_) | tir::ModuleItem::TypeDecl(_) => {}
    }
}

fn collect_item_bindings(item: &tir::ModuleItem, facts: &mut Vec<TypeFact>) {
    match item {
        tir::ModuleItem::LetDecl(decl) => collect_let_decl_bindings(decl, facts),
        tir::ModuleItem::UseDecl(_) | tir::ModuleItem::TypeDecl(_) => {}
    }
}

fn collect_let_decl(decl: &tir::LetDecl, facts: &mut Vec<TypeFact>) {
    match decl {
        tir::LetDecl::Const {
            name, body, scheme, ..
        } => {
            facts.push(TypeFact {
                span: name.span(),
                label: format_scheme(scheme),
            });
            collect_expr(body, facts);
        }
        tir::LetDecl::Fn {
            name,
            args,
            body,
            scheme,
            ..
        } => {
            facts.push(TypeFact {
                span: name.span(),
                label: format_scheme(scheme),
            });
            for arg in args {
                collect_pattern(arg, facts);
            }
            collect_expr(body, facts);
        }
    }
}

fn collect_let_decl_bindings(decl: &tir::LetDecl, facts: &mut Vec<TypeFact>) {
    match decl {
        tir::LetDecl::Const {
            name, body, scheme, ..
        } => {
            facts.push(TypeFact {
                span: name.span(),
                label: format_scheme(scheme),
            });
            collect_expr_bindings(body, facts);
        }
        tir::LetDecl::Fn {
            name,
            args,
            body,
            scheme,
            ..
        } => {
            facts.push(TypeFact {
                span: name.span(),
                label: format_scheme(scheme),
            });
            for arg in args {
                collect_pattern_bindings(arg, facts);
            }
            collect_expr_bindings(body, facts);
        }
    }
}

fn collect_expr(expr: &tir::Expr, facts: &mut Vec<TypeFact>) {
    facts.push(TypeFact {
        span: expr.span,
        label: format_type(&expr.ty),
    });

    match &expr.kind {
        tir::ExprKind::Literal { .. } | tir::ExprKind::Var { .. } => {}
        tir::ExprKind::Constructor { args, .. } => {
            for arg in args {
                collect_expr(arg, facts);
            }
        }
        tir::ExprKind::Fn { params, body } => {
            for param in params {
                collect_pattern(param, facts);
            }
            collect_expr(body, facts);
        }
        tir::ExprKind::Let { bind, body, .. } => {
            collect_pattern(bind, facts);
            collect_expr(body, facts);
        }
        tir::ExprKind::Apply { callee, args } => {
            collect_expr(callee, facts);
            for arg in args {
                collect_expr(arg, facts);
            }
        }
        tir::ExprKind::BinaryOp { left, right, .. } => {
            collect_expr(left, facts);
            collect_expr(right, facts);
        }
        tir::ExprKind::TypeHint { expr, .. } => collect_expr(expr, facts),
        tir::ExprKind::Tuple { items } | tir::ExprKind::Block { exprs: items } => {
            for item in items {
                collect_expr(item, facts);
            }
        }
        tir::ExprKind::Record { fields, .. } => {
            for (_, field) in fields {
                collect_expr(field, facts);
            }
        }
        tir::ExprKind::Field { base, .. } => collect_expr(base, facts),
        tir::ExprKind::Match { scrutinee, arms } => {
            collect_expr(scrutinee, facts);
            for arm in arms {
                collect_pattern(&arm.pattern, facts);
                collect_expr(&arm.body, facts);
            }
        }
    }
}

fn collect_expr_bindings(expr: &tir::Expr, facts: &mut Vec<TypeFact>) {
    match &expr.kind {
        tir::ExprKind::Literal { .. } | tir::ExprKind::Var { .. } => {}
        tir::ExprKind::Constructor { args, .. } => {
            for arg in args {
                collect_expr_bindings(arg, facts);
            }
        }
        tir::ExprKind::Fn { params, body } => {
            for param in params {
                collect_pattern_bindings(param, facts);
            }
            collect_expr_bindings(body, facts);
        }
        tir::ExprKind::Let { bind, body, .. } => {
            collect_pattern_bindings(bind, facts);
            collect_expr_bindings(body, facts);
        }
        tir::ExprKind::Apply { callee, args } => {
            collect_expr_bindings(callee, facts);
            for arg in args {
                collect_expr_bindings(arg, facts);
            }
        }
        tir::ExprKind::BinaryOp { left, right, .. } => {
            collect_expr_bindings(left, facts);
            collect_expr_bindings(right, facts);
        }
        tir::ExprKind::TypeHint { expr, .. } => collect_expr_bindings(expr, facts),
        tir::ExprKind::Tuple { items } | tir::ExprKind::Block { exprs: items } => {
            for item in items {
                collect_expr_bindings(item, facts);
            }
        }
        tir::ExprKind::Record { fields, .. } => {
            for (_, field) in fields {
                collect_expr_bindings(field, facts);
            }
        }
        tir::ExprKind::Field { base, .. } => collect_expr_bindings(base, facts),
        tir::ExprKind::Match { scrutinee, arms } => {
            collect_expr_bindings(scrutinee, facts);
            for arm in arms {
                collect_pattern_bindings(&arm.pattern, facts);
                collect_expr_bindings(&arm.body, facts);
            }
        }
    }
}

fn collect_pattern(pattern: &tir::Pattern, facts: &mut Vec<TypeFact>) {
    if let Some(span) = pattern_label_span(pattern) {
        facts.push(TypeFact {
            span,
            label: format_type(&pattern.ty),
        });
    }

    match &pattern.kind {
        tir::PatternKind::Wildcard | tir::PatternKind::Literal { .. } => {}
        tir::PatternKind::Var { .. } => {}
        tir::PatternKind::Constructor { args, .. } | tir::PatternKind::Tuple { items: args } => {
            for arg in args {
                collect_pattern(arg, facts);
            }
        }
        tir::PatternKind::TypeHint { pattern, .. } => collect_pattern(pattern, facts),
        tir::PatternKind::Record { fields, .. } => {
            for (_, field) in fields {
                collect_pattern(field, facts);
            }
        }
    }
}

fn collect_pattern_bindings(pattern: &tir::Pattern, facts: &mut Vec<TypeFact>) {
    match &pattern.kind {
        tir::PatternKind::Var { name, .. } => facts.push(TypeFact {
            span: name.span(),
            label: format_type(&pattern.ty),
        }),
        tir::PatternKind::Constructor { args, .. } | tir::PatternKind::Tuple { items: args } => {
            for arg in args {
                collect_pattern_bindings(arg, facts);
            }
        }
        tir::PatternKind::TypeHint { pattern, .. } => collect_pattern_bindings(pattern, facts),
        tir::PatternKind::Record { fields, .. } => {
            for (_, field) in fields {
                collect_pattern_bindings(field, facts);
            }
        }
        tir::PatternKind::Wildcard | tir::PatternKind::Literal { .. } => {}
    }
}

fn pattern_label_span(pattern: &tir::Pattern) -> Option<Span> {
    match &pattern.kind {
        tir::PatternKind::Var { name, .. } => Some(name.span()),
        tir::PatternKind::Constructor { name, .. } => Some(name.span()),
        tir::PatternKind::Record { name, .. } => Some(name.span()),
        _ => None,
    }
}

pub fn format_scheme(scheme: &Scheme) -> String {
    format_type(&scheme.body)
}

pub fn format_type(ty: &tir::Type) -> String {
    match ty {
        tir::Type::Var(id) => format!("?{}", id.index()),
        tir::Type::Generic(id) => format!("'{}", generic_name(id.index())),
        tir::Type::Tuple(items) => {
            let items = items.iter().map(format_type).collect::<Vec<_>>().join(", ");
            format!("({items})")
        }
        tir::Type::Arrow { .. } => format_arrow(ty),
        tir::Type::Apply { ident, arguments } => {
            let name = ident.as_name().unwrap_or("<qualified>");
            if arguments.is_empty() {
                name.to_string()
            } else {
                let args = arguments
                    .iter()
                    .map(format_type)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}<{args}>")
            }
        }
    }
}

fn format_arrow(ty: &tir::Type) -> String {
    let mut parts = Vec::new();
    let mut current = ty;
    while let tir::Type::Arrow { parameter, result } = current {
        parts.push(format_type(parameter));
        current = result;
    }
    parts.push(format_type(current));
    parts.join(" -> ")
}

fn generic_name(index: usize) -> String {
    let letter = (b'a' + (index % 26) as u8) as char;
    if index < 26 {
        letter.to_string()
    } else {
        format!("{letter}{}", index / 26)
    }
}

pub fn range_contains(range: &Range, position: Position) -> bool {
    (range.start.line, range.start.character) <= (position.line, position.character)
        && (position.line, position.character) <= (range.end.line, range.end.character)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{checker::Checker, parser::lexer::Lexer, parser::parse::Parser};

    fn check(source: &str) -> ModuleSummary {
        let lexer = Lexer::new(source).expect("lex");
        let ast = Parser::new(lexer).parse_module().expect("parse");
        Checker::new().check_module(&ast)
    }

    #[test]
    fn extracts_top_level_type_hints() {
        let source = "let id x = x";
        let summary = check(source);
        let facts = binding_type_facts(&summary);
        assert!(facts.iter().any(|fact| fact.label.contains(" -> ")));
    }

    #[test]
    fn inlay_hints_only_target_value_bindings() {
        let source = "let x = { let y = 1; y + 2 }";
        let summary = check(source);
        let hints = inlay_hints(source, &summary);
        let positions = hints
            .iter()
            .map(|hint| (hint.position.line, hint.position.character))
            .collect::<Vec<_>>();
        assert!(positions.contains(&(0, 5)), "top-level x should get a hint");
        assert!(positions.contains(&(0, 15)), "local y should get a hint");
        assert_eq!(positions.len(), 2, "expressions should not get inlay hints");
    }

    #[test]
    fn hover_chooses_smallest_containing_type_fact() {
        let source = "let answer = 1";
        let summary = check(source);
        let hover = hover(source, &summary, Position::new(0, 5)).expect("hover");
        assert!(matches!(hover.contents, HoverContents::Markup(_)));
    }
}
