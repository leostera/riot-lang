use camino::Utf8Path;
use chumsky::prelude::*;

use crate::ast::{AstBlock, AstExpr, AstFnDecl, AstPath, AstProgram, AstStmt, TextSpan};
use crate::diagnostic::to_source_diagnostic;

pub(crate) fn parse_source(source_path: &Utf8Path, source: &str) -> miette::Result<AstProgram> {
    let (program, errors) = parser().parse(source).into_output_errors();

    if let Some(error) = errors.into_iter().next() {
        return Err(to_source_diagnostic(
            source_path,
            source,
            *error.span(),
            "could not parse Riot ML source",
            error.to_string(),
            Some("stage0 currently accepts top-level zero-argument fn declarations with println string bodies"),
        )
        .into());
    }

    program.ok_or_else(|| {
        to_source_diagnostic(
            source_path,
            source,
            SimpleSpan::new((), 0..source.len().min(1)),
            "could not parse Riot ML source",
            "expected a top-level function",
            Some("try: fn main() { println(\"hello world\") }"),
        )
        .into()
    })
}

fn parser<'src>() -> impl Parser<'src, &'src str, AstProgram, extra::Err<Rich<'src, char>>> {
    let lower_ident = lower_ident();
    let ident = ident();

    let string_char = none_of("\\\"")
        .ignored()
        .or(just('\\').then(any()).ignored());
    let string = just('"')
        .then(string_char.repeated())
        .then(just('"'))
        .to_slice()
        .try_map(|raw: &str, span| {
            snailquote::unescape(raw)
                .map_err(|error| Rich::custom(span, format!("invalid string literal: {error}")))
        })
        .padded();

    let expr = recursive(|expr| {
        let string_expr =
            string
                .clone()
                .spanned()
                .map(
                    |value: chumsky::span::Spanned<String, TextSpan>| AstExpr::String {
                        value: value.inner,
                        span: value.span,
                    },
                );

        let int_expr = text::int(10)
            .to_slice()
            .try_map(|raw: &str, span| {
                raw.parse::<i64>()
                    .map_err(|error| Rich::custom(span, format!("invalid integer literal: {error}")))
            })
            .padded()
            .map_with(|value, extra| AstExpr::Int {
                value,
                span: extra.span(),
            });

        let bool_expr = text::keyword("true")
            .to(true)
            .or(text::keyword("false").to(false))
            .padded()
            .map_with(|value, extra| AstExpr::Bool {
                value,
                span: extra.span(),
            });

        let path = ident
            .clone()
            .then(
                just('.')
                    .padded()
                    .ignore_then(ident.clone())
                    .repeated()
                    .collect(),
            )
            .map(|(head, mut tail): (String, Vec<String>)| {
                let mut segments = Vec::with_capacity(tail.len() + 1);
                segments.push(head);
                segments.append(&mut tail);
                AstPath { segments }
            })
            .padded();

        let arg_list = expr
            .clone()
            .separated_by(just(',').padded())
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just('(').padded(), just(')').padded());

        let call_expr = path
            .clone()
            .then(arg_list)
            .map(|(callee, args): (AstPath, Vec<AstExpr>)| AstExpr::Call { callee, args });

        let path_expr = path.map_with(|path, extra| AstExpr::Path {
            path,
            span: extra.span(),
        });

        let paren_expr = expr
            .clone()
            .delimited_by(just('(').padded(), just(')').padded());

        choice((call_expr, string_expr, bool_expr, int_expr, path_expr, paren_expr))
            .labelled("expression")
    });

    let let_stmt = text::keyword("let")
        .padded()
        .ignore_then(lower_ident.clone().spanned())
        .then_ignore(just('=').padded())
        .then(expr.clone())
        .then_ignore(just(';').padded())
        .map_with(
            |(name, value): (chumsky::span::Spanned<String, TextSpan>, AstExpr), extra| {
                AstStmt::Let {
                    name: name.inner,
                    name_span: name.span,
                    value,
                    span: extra.span(),
                }
            },
        );

    let expr_stmt = expr
        .clone()
        .then_ignore(just(';').padded())
        .map(AstStmt::Expr);

    let stmt = choice((let_stmt, expr_stmt));

    let block = just('{')
        .padded()
        .ignore_then(stmt.repeated().collect::<Vec<_>>())
        .then(expr.clone().or_not())
        .then_ignore(just('}').padded())
        .map_with(|(statements, tail), extra| AstBlock {
            statements,
            tail,
            span: extra.span(),
        });

    let empty_params = just('(').padded().then_ignore(just(')').padded());

    let fn_decl = text::keyword("fn")
        .padded()
        .ignore_then(lower_ident.spanned())
        .then_ignore(empty_params)
        .then(block)
        .map_with(
            |(name, body): (chumsky::span::Spanned<String, TextSpan>, AstBlock), extra| AstFnDecl {
                name: name.inner,
                name_span: name.span,
                body,
                span: extra.span(),
            },
        );

    fn_decl
        .repeated()
        .collect()
        .then_ignore(end())
        .map(|decls| AstProgram { decls })
}

fn ident<'src>() -> impl Parser<'src, &'src str, String, extra::Err<Rich<'src, char>>> + Clone {
    text::ident()
        .try_map(|ident: &str, span| {
            if is_reserved_word(ident) {
                Err(Rich::custom(
                    span,
                    format!("reserved word `{ident}` cannot be used as an identifier"),
                ))
            } else {
                Ok(ident.to_owned())
            }
        })
        .padded()
}

fn lower_ident<'src>() -> impl Parser<'src, &'src str, String, extra::Err<Rich<'src, char>>> + Clone
{
    text::ident()
        .try_map(|ident: &str, span| {
            let is_lower = ident
                .chars()
                .next()
                .is_some_and(|c| c == '_' || c.is_ascii_lowercase());
            if is_reserved_word(ident) {
                Err(Rich::custom(
                    span,
                    format!("reserved word `{ident}` cannot be used as an identifier"),
                ))
            } else if !is_lower {
                Err(Rich::custom(
                    span,
                    format!("expected lowercase identifier, found `{ident}`"),
                ))
            } else {
                Ok(ident.to_owned())
            }
        })
        .padded()
}

fn is_reserved_word(ident: &str) -> bool {
    matches!(
        ident,
        "pub"
            | "type"
            | "sealed"
            | "opaque"
            | "mod"
            | "fn"
            | "let"
            | "mut"
            | "use"
            | "match"
            | "receive"
            | "spawn"
            | "send"
            | "when"
            | "if"
            | "else"
            | "after"
            | "true"
            | "false"
            | "unit"
    )
}
