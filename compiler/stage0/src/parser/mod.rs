use camino::Utf8Path;

use crate::ast::{
    AstBlock, AstExpr, AstFnDecl, AstPath, AstProgram, AstStmt, AstTypeAnnotation, TextSpan,
};
use crate::diagnostic::to_source_diagnostic;
use crate::lexer::{Token, TokenKind, lex};

mod error;
mod literal;
mod span;

use error::ParseError;
use literal::{parse_char_literal, parse_int_literal};
use span::expr_span;

pub(crate) fn parse_source(source_path: &Utf8Path, source: &str) -> miette::Result<AstProgram> {
    let tokens = lex(source).map_err(|error| {
        to_source_diagnostic(
            source_path,
            source,
            error.span,
            "could not lex Riot ML source",
            error.message,
            Some("stage0 currently accepts a small Riot ML subset"),
        )
    })?;

    Parser::new(source, tokens)
        .parse_program()
        .map_err(|error| {
            to_source_diagnostic(
                source_path,
                source,
                error.span,
                "could not parse Riot ML source",
                error.message,
                error.help,
            )
            .into()
        })
}

struct Parser<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    cursor: usize,
}

impl<'src> Parser<'src> {
    fn new(source: &'src str, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            cursor: 0,
        }
    }

    fn parse_program(&mut self) -> Result<AstProgram, ParseError> {
        let mut decls = Vec::new();

        while !self.at(TokenKind::Eof) {
            decls.push(self.parse_fn_decl()?);
        }

        Ok(AstProgram { decls })
    }

    fn parse_fn_decl(&mut self) -> Result<AstFnDecl, ParseError> {
        let start = self.expect(TokenKind::Fn, "expected top-level function declaration")?;
        let (name, name_span) = self.expect_lower_ident()?;

        self.expect(TokenKind::LParen, "expected `(` after function name")?;
        if !self.at(TokenKind::RParen) {
            return Err(self.error_at_current(
                "stage0 function parameters are not supported yet",
                Some("try: fn main() { dbg(\"hello world\") }"),
            ));
        }
        self.expect(TokenKind::RParen, "expected `)` after function parameters")?;

        let body = self.parse_block()?;
        let span = start.span.join(body.span);
        Ok(AstFnDecl {
            name,
            name_span,
            body,
            span,
        })
    }

    fn parse_block(&mut self) -> Result<AstBlock, ParseError> {
        let start = self.expect(TokenKind::LBrace, "expected `{`")?;
        let mut statements = Vec::new();
        let mut tail = None;

        while !self.at(TokenKind::RBrace) {
            if self.at(TokenKind::Eof) {
                return Err(self.error_at_current("expected `}`", Some("close this block")));
            }

            if self.at(TokenKind::Let) {
                statements.push(self.parse_let_stmt()?);
                continue;
            }

            let expr = self.parse_expr()?;
            if self.match_kind(TokenKind::Semicolon).is_some() {
                statements.push(AstStmt::Expr(expr));
            } else {
                tail = Some(expr);
                break;
            }
        }

        let end = self.expect(TokenKind::RBrace, "expected `}` after block")?;
        Ok(AstBlock {
            statements,
            tail,
            span: start.span.join(end.span),
        })
    }

    fn parse_let_stmt(&mut self) -> Result<AstStmt, ParseError> {
        let start = self.expect(TokenKind::Let, "expected `let`")?;
        let (name, name_span) = self.expect_lower_ident()?;

        let type_annotation = if self.match_kind(TokenKind::Colon).is_some() {
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        self.expect(TokenKind::Eq, "expected `=` in let binding")?;
        let value = self.parse_expr()?;
        let end = self.expect(TokenKind::Semicolon, "expected `;` after let binding")?;

        Ok(AstStmt::Let {
            name,
            name_span,
            type_annotation,
            value,
            span: start.span.join(end.span),
        })
    }

    fn parse_type_annotation(&mut self) -> Result<AstTypeAnnotation, ParseError> {
        let start = self.current().span;
        while !self.at(TokenKind::Eq) {
            if self.at(TokenKind::Semicolon)
                || self.at(TokenKind::RBrace)
                || self.at(TokenKind::Eof)
            {
                return Err(self.error_at_current("expected `=` after type annotation", None));
            }
            self.bump();
        }

        let end = self.previous_span();
        if start.start >= end.end {
            return Err(self.error_at_current("expected type annotation after `:`", None));
        }

        let span = TextSpan::new(start.start, end.end);
        Ok(AstTypeAnnotation {
            text: self.text(span).trim().to_owned(),
            span,
        })
    }

    fn parse_expr(&mut self) -> Result<AstExpr, ParseError> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<AstExpr, ParseError> {
        let mut expr = self.parse_and()?;
        while self.match_kind(TokenKind::OrOr).is_some() {
            let rhs = self.parse_and()?;
            let span = expr_span(&expr).join(expr_span(&rhs));
            expr = AstExpr::Or {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<AstExpr, ParseError> {
        let mut expr = self.parse_equality()?;
        while self.match_kind(TokenKind::AndAnd).is_some() {
            let rhs = self.parse_equality()?;
            let span = expr_span(&expr).join(expr_span(&rhs));
            expr = AstExpr::And {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<AstExpr, ParseError> {
        let mut expr = self.parse_comparison()?;
        while self.match_kind(TokenKind::EqEq).is_some() {
            let rhs = self.parse_comparison()?;
            let span = expr_span(&expr).join(expr_span(&rhs));
            expr = AstExpr::Eq {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<AstExpr, ParseError> {
        let mut expr = self.parse_additive()?;
        while self.match_kind(TokenKind::Lt).is_some() {
            let rhs = self.parse_additive()?;
            let span = expr_span(&expr).join(expr_span(&rhs));
            expr = AstExpr::Lt {
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span,
            };
        }
        Ok(expr)
    }

    fn parse_additive(&mut self) -> Result<AstExpr, ParseError> {
        let mut expr = self.parse_multiplicative()?;

        loop {
            if self.match_kind(TokenKind::Plus).is_some() {
                let rhs = self.parse_multiplicative()?;
                let span = expr_span(&expr).join(expr_span(&rhs));
                expr = AstExpr::Add {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    span,
                };
            } else if self.match_kind(TokenKind::Minus).is_some() {
                let rhs = self.parse_multiplicative()?;
                let span = expr_span(&expr).join(expr_span(&rhs));
                expr = AstExpr::Sub {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> Result<AstExpr, ParseError> {
        let mut expr = self.parse_unary()?;

        loop {
            if self.match_kind(TokenKind::Star).is_some() {
                let rhs = self.parse_unary()?;
                let span = expr_span(&expr).join(expr_span(&rhs));
                expr = AstExpr::Mul {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    span,
                };
            } else if self.match_kind(TokenKind::Slash).is_some() {
                let rhs = self.parse_unary()?;
                let span = expr_span(&expr).join(expr_span(&rhs));
                expr = AstExpr::Div {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    span,
                };
            } else if self.match_kind(TokenKind::Percent).is_some() {
                let rhs = self.parse_unary()?;
                let span = expr_span(&expr).join(expr_span(&rhs));
                expr = AstExpr::Mod {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<AstExpr, ParseError> {
        if let Some(start) = self.match_kind(TokenKind::Minus) {
            let expr = self.parse_unary()?;
            let span = start.join(expr_span(&expr));
            Ok(AstExpr::Neg {
                expr: Box::new(expr),
                span,
            })
        } else if let Some(start) = self.match_kind(TokenKind::Bang) {
            let expr = self.parse_unary()?;
            let span = start.join(expr_span(&expr));
            Ok(AstExpr::Not {
                expr: Box::new(expr),
                span,
            })
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Result<AstExpr, ParseError> {
        let mut expr = self.parse_primary()?;

        while self.at(TokenKind::LParen) {
            let AstExpr::Path { path, span } = expr else {
                return Err(self.error_at_current(
                    "stage0 only supports calling named functions",
                    Some("try: dbg(\"hello world\")"),
                ));
            };
            let args = self.parse_arg_list()?;
            let end = self.previous_span();
            expr = AstExpr::Call {
                callee: path,
                args,
                span: span.join(end),
            };
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<AstExpr, ParseError> {
        if self.at(TokenKind::True) || self.at(TokenKind::False) {
            let token = self.bump();
            return Ok(AstExpr::Bool {
                value: token.kind == TokenKind::True,
                span: token.span,
            });
        }

        match self.current().kind {
            TokenKind::String => self.parse_string(),
            TokenKind::Char => self.parse_char(),
            TokenKind::Float => self.parse_float(),
            TokenKind::Int => self.parse_int(),
            TokenKind::Ident => {
                let (path, span) = self.parse_path()?;
                let is_record_path = path
                    .segments
                    .first()
                    .and_then(|segment| segment.chars().next())
                    .is_some_and(|ch| ch.is_ascii_uppercase());
                if is_record_path && self.at(TokenKind::LBrace) {
                    self.parse_record(path, span)
                } else {
                    Ok(AstExpr::Path { path, span })
                }
            }
            TokenKind::If => self.parse_if(),
            TokenKind::LParen => self.parse_paren_or_tuple_or_unit(),
            TokenKind::LBracket => self.parse_list(),
            _ => Err(self.error_at_current("expected expression", None)),
        }
    }

    fn parse_if(&mut self) -> Result<AstExpr, ParseError> {
        let start = self.expect(TokenKind::If, "expected `if`")?;
        let condition = self.parse_expr()?;
        let then_branch = self.parse_block_expr()?;
        self.expect(TokenKind::Else, "expected `else` after if branch")?;
        let else_branch = if self.at(TokenKind::If) {
            self.parse_if()?
        } else {
            self.parse_block_expr()?
        };

        let span = start.span.join(expr_span(&else_branch));
        Ok(AstExpr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
            span,
        })
    }

    fn parse_block_expr(&mut self) -> Result<AstExpr, ParseError> {
        let block = self.parse_block()?;
        if block.statements.is_empty() {
            if let Some(tail) = block.tail {
                return Ok(tail);
            }
        }

        Err(ParseError {
            span: block.span,
            message: "stage0 expression blocks must contain a single tail expression".to_owned(),
            help: Some("try: if condition { value } else { other_value }"),
        })
    }

    fn parse_arg_list(&mut self) -> Result<Vec<AstExpr>, ParseError> {
        self.expect(TokenKind::LParen, "expected `(`")?;
        let mut args = Vec::new();

        if self.match_kind(TokenKind::RParen).is_some() {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expr()?);

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }

            if self.at(TokenKind::RParen) {
                break;
            }
        }

        self.expect(TokenKind::RParen, "expected `)` after arguments")?;
        Ok(args)
    }

    fn parse_paren_or_tuple_or_unit(&mut self) -> Result<AstExpr, ParseError> {
        let start = self.expect(TokenKind::LParen, "expected `(`")?;

        if let Some(end) = self.match_kind(TokenKind::RParen) {
            return Ok(AstExpr::Unit {
                span: start.span.join(end),
            });
        }

        let first = self.parse_expr()?;
        if self.match_kind(TokenKind::Comma).is_none() {
            self.expect(TokenKind::RParen, "expected `)` after expression")?;
            return Ok(first);
        }

        let mut items = vec![first];
        while !self.at(TokenKind::RParen) {
            items.push(self.parse_expr()?);

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }
        }

        let end = self.expect(TokenKind::RParen, "expected `)` after tuple")?;
        Ok(AstExpr::Tuple {
            items,
            span: start.span.join(end.span),
        })
    }

    fn parse_list(&mut self) -> Result<AstExpr, ParseError> {
        let start = self.expect(TokenKind::LBracket, "expected `[`")?;
        let mut items = Vec::new();

        if let Some(end) = self.match_kind(TokenKind::RBracket) {
            return Ok(AstExpr::List {
                items,
                span: start.span.join(end),
            });
        }

        loop {
            items.push(self.parse_expr()?);

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }

            if self.at(TokenKind::RBracket) {
                break;
            }
        }

        let end = self.expect(TokenKind::RBracket, "expected `]` after list")?;
        Ok(AstExpr::List {
            items,
            span: start.span.join(end.span),
        })
    }

    fn parse_record(&mut self, path: AstPath, path_span: TextSpan) -> Result<AstExpr, ParseError> {
        self.expect(TokenKind::LBrace, "expected `{` after record path")?;
        let mut fields = Vec::new();

        if let Some(end) = self.match_kind(TokenKind::RBrace) {
            return Ok(AstExpr::Record {
                path,
                fields,
                span: path_span.join(end),
            });
        }

        loop {
            let (name, _) = self.expect_lower_ident()?;
            self.expect(TokenKind::Colon, "expected `:` after record field name")?;
            let value = self.parse_expr()?;
            fields.push((name, value));

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }

            if self.at(TokenKind::RBrace) {
                break;
            }
        }

        let end = self.expect(TokenKind::RBrace, "expected `}` after record fields")?;
        Ok(AstExpr::Record {
            path,
            fields,
            span: path_span.join(end.span),
        })
    }

    fn parse_path(&mut self) -> Result<(AstPath, TextSpan), ParseError> {
        let (head, start) = self.expect_ident()?;
        let mut segments = vec![head];
        let mut span = start;

        while self.match_kind(TokenKind::Dot).is_some() {
            let (segment, segment_span) = self.expect_ident()?;
            span = span.join(segment_span);
            segments.push(segment);
        }

        Ok((AstPath { segments }, span))
    }

    fn parse_string(&mut self) -> Result<AstExpr, ParseError> {
        let token = self.expect(TokenKind::String, "expected string literal")?;
        let raw = self.text(token.span);
        let value = snailquote::unescape(raw).map_err(|error| ParseError {
            span: token.span,
            message: format!("invalid string literal: {error}"),
            help: None,
        })?;
        Ok(AstExpr::String {
            value,
            span: token.span,
        })
    }

    fn parse_char(&mut self) -> Result<AstExpr, ParseError> {
        let token = self.expect(TokenKind::Char, "expected character literal")?;
        let value = parse_char_literal(self.text(token.span)).map_err(|message| ParseError {
            span: token.span,
            message,
            help: None,
        })?;
        Ok(AstExpr::Char {
            value,
            span: token.span,
        })
    }

    fn parse_float(&mut self) -> Result<AstExpr, ParseError> {
        let token = self.expect(TokenKind::Float, "expected float literal")?;
        let raw = self.text(token.span);
        let normalized = raw.replace('_', "");
        normalized.parse::<f64>().map_err(|error| ParseError {
            span: token.span,
            message: format!("invalid float literal: {error}"),
            help: None,
        })?;
        Ok(AstExpr::Float {
            value: raw.to_owned(),
            span: token.span,
        })
    }

    fn parse_int(&mut self) -> Result<AstExpr, ParseError> {
        let token = self.expect(TokenKind::Int, "expected integer literal")?;
        let raw = self.text(token.span);
        let value = parse_int_literal(raw).map_err(|message| ParseError {
            span: token.span,
            message,
            help: None,
        })?;
        Ok(AstExpr::Int {
            value,
            span: token.span,
        })
    }

    fn expect_lower_ident(&mut self) -> Result<(String, TextSpan), ParseError> {
        let (ident, span) = self.expect_ident()?;
        let is_lower = ident
            .chars()
            .next()
            .is_some_and(|ch| ch == '_' || ch.is_ascii_lowercase());

        if is_lower {
            Ok((ident, span))
        } else {
            Err(ParseError {
                span,
                message: format!("expected lowercase identifier, found `{ident}`"),
                help: None,
            })
        }
    }

    fn expect_ident(&mut self) -> Result<(String, TextSpan), ParseError> {
        let token = self.expect(TokenKind::Ident, "expected identifier")?;
        Ok((self.text(token.span).to_owned(), token.span))
    }

    fn expect(&mut self, kind: TokenKind, message: &'static str) -> Result<Token, ParseError> {
        if self.at(kind) {
            Ok(self.bump())
        } else {
            Err(self.error_at_current(message, None))
        }
    }

    fn match_kind(&mut self, kind: TokenKind) -> Option<TextSpan> {
        if self.at(kind) {
            Some(self.bump().span)
        } else {
            None
        }
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current().kind == kind
    }

    fn current(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    fn bump(&mut self) -> Token {
        let token = self.current().clone();
        if token.kind != TokenKind::Eof {
            self.cursor += 1;
        }
        token
    }

    fn previous_span(&self) -> TextSpan {
        self.tokens
            .get(self.cursor.saturating_sub(1))
            .map(|token| token.span)
            .unwrap_or_else(|| TextSpan::new(0, 0))
    }

    fn text(&self, span: TextSpan) -> &'src str {
        &self.source[span.start..span.end]
    }

    fn error_at_current(
        &self,
        message: impl Into<String>,
        help: Option<&'static str>,
    ) -> ParseError {
        ParseError {
            span: self.current().span,
            message: message.into(),
            help,
        }
    }
}
