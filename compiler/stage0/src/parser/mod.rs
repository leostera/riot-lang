use camino::Utf8Path;

use crate::ast::{
    AstBlock, AstDecl, AstExpr, AstExternalDecl, AstFnDecl, AstMatchArm, AstPath, AstPattern,
    AstProgram, AstRecordTypeField, AstStmt, AstTypeAnnotation, AstTypeBody, AstTypeDecl,
    AstUseDecl, AstVariantConstructor, TextSpan,
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
            decls.push(self.parse_decl()?);
        }

        Ok(AstProgram { decls })
    }

    fn parse_decl(&mut self) -> Result<AstDecl, ParseError> {
        match self.current().kind {
            TokenKind::Use => self.parse_use_decl().map(AstDecl::Use),
            TokenKind::External => self.parse_external_decl().map(AstDecl::External),
            TokenKind::Type => self.parse_type_decl().map(AstDecl::Type),
            TokenKind::Fn => self.parse_fn_decl().map(AstDecl::Function),
            _ => Err(self.error_at_current(
                "expected top-level `use`, `external`, `type`, or `fn` declaration",
                Some("try: fn main() { println(\"hello world\") }"),
            )),
        }
    }

    fn parse_use_decl(&mut self) -> Result<AstUseDecl, ParseError> {
        let start = self.expect(TokenKind::Use, "expected `use`")?;
        let (name, name_span) = self.expect_ident()?;
        let is_module_name = name
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_uppercase());
        if !is_module_name {
            return Err(ParseError {
                span: name_span,
                message: format!("expected module name after `use`, found `{name}`"),
                help: Some("stage0 `use` imports a ClassCase module name, for example `use Math`"),
            });
        }
        if self.at(TokenKind::Dot) {
            return Err(self.error_at_current(
                "stage0 `use` only accepts simple module names",
                Some("try: use Math"),
            ));
        }
        Ok(AstUseDecl {
            name,
            name_span,
            span: start.span.join(name_span),
        })
    }

    fn parse_external_decl(&mut self) -> Result<AstExternalDecl, ParseError> {
        let start = self.expect(TokenKind::External, "expected `external`")?;
        let (name, name_span) = self.expect_lower_ident()?;
        self.expect(TokenKind::Colon, "expected `:` after external name")?;
        let type_start = self.current().span;
        while !self.at(TokenKind::Eq) {
            if self.at(TokenKind::Semicolon)
                || self.at(TokenKind::LBrace)
                || self.at(TokenKind::RBrace)
                || self.at(TokenKind::Eof)
            {
                return Err(self.error_at_current("expected `=` after external type", None));
            }
            self.bump();
        }
        let type_end = self.previous_span();
        if type_start.start >= type_end.end {
            return Err(self.error_at_current("expected external type after `:`", None));
        }
        let type_span = TextSpan::new(type_start.start, type_end.end);
        self.expect(TokenKind::Eq, "expected `=` after external type")?;
        let abi_token = self.expect(TokenKind::String, "expected external ABI string")?;
        let abi = snailquote::unescape(self.text(abi_token.span)).map_err(|error| ParseError {
            span: abi_token.span,
            message: format!("invalid external ABI string: {error}"),
            help: None,
        })?;
        Ok(AstExternalDecl {
            name,
            name_span,
            type_text: self.text(type_span).trim().to_owned(),
            type_span,
            abi,
            span: start.span.join(abi_token.span),
        })
    }

    fn parse_type_decl(&mut self) -> Result<AstTypeDecl, ParseError> {
        let start = self.expect(TokenKind::Type, "expected type declaration")?;
        let (name, name_span) = self.expect_lower_ident()?;
        self.expect(TokenKind::Eq, "expected `=` after type name")?;

        if self.at(TokenKind::LBrace) {
            return self.parse_record_type_decl(start.span, name, name_span);
        }

        let mut constructors = Vec::new();
        loop {
            let (constructor, constructor_span) = self.expect_upper_ident()?;
            let payload = if self.match_kind(TokenKind::LParen).is_some() {
                let mut payload = Vec::new();
                if self.match_kind(TokenKind::RParen).is_none() {
                    loop {
                        payload.push(self.parse_type_annotation_until(
                            &[TokenKind::Comma, TokenKind::RParen],
                            "expected `,` or `)` after variant constructor payload type",
                        )?);
                        if self.match_kind(TokenKind::Comma).is_none() {
                            break;
                        }
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                    }
                    self.expect(
                        TokenKind::RParen,
                        "expected `)` after variant payload types",
                    )?;
                }
                payload
            } else {
                Vec::new()
            };
            constructors.push(AstVariantConstructor {
                name: constructor,
                name_span: constructor_span,
                payload,
            });

            if self.match_kind(TokenKind::Pipe).is_none() {
                break;
            }
        }

        if constructors.is_empty() {
            return Err(ParseError {
                span: name_span,
                message: "variant type needs at least one constructor".to_owned(),
                help: Some("try: type color = Red | Green | Blue"),
            });
        }

        let span = start
            .span
            .join(constructors.last().expect("constructor exists").name_span);
        Ok(AstTypeDecl {
            name,
            name_span,
            body: AstTypeBody::Variant { constructors },
            span,
        })
    }

    fn parse_record_type_decl(
        &mut self,
        start: TextSpan,
        name: String,
        name_span: TextSpan,
    ) -> Result<AstTypeDecl, ParseError> {
        self.expect(TokenKind::LBrace, "expected `{` in record type")?;
        let mut fields = Vec::new();
        while !self.at(TokenKind::RBrace) {
            let (field, field_span) = self.expect_lower_ident()?;
            self.expect(TokenKind::Colon, "expected `:` after record field name")?;
            let type_annotation = self.parse_type_annotation_until(
                &[TokenKind::Comma, TokenKind::RBrace],
                "expected `,` or `}` after record field type",
            )?;
            fields.push(AstRecordTypeField {
                name: field,
                name_span: field_span,
                type_annotation,
            });

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }
            if self.at(TokenKind::RBrace) {
                break;
            }
        }
        let end = self.expect(TokenKind::RBrace, "expected `}` after record type fields")?;
        if fields.is_empty() {
            return Err(ParseError {
                span: name_span,
                message: "record type needs at least one field".to_owned(),
                help: Some("try: type point = { x: i64, y: i64 }"),
            });
        }

        Ok(AstTypeDecl {
            name,
            name_span,
            body: AstTypeBody::Record { fields },
            span: start.join(end.span),
        })
    }

    fn parse_fn_decl(&mut self) -> Result<AstFnDecl, ParseError> {
        let start = self.expect(TokenKind::Fn, "expected top-level function declaration")?;
        let (name, name_span) = self.expect_lower_ident()?;

        let (params, param_types) = self.parse_param_list()?;
        let return_type = if self.match_kind(TokenKind::Arrow).is_some() {
            Some(self.parse_type_annotation_until(
                &[TokenKind::LBrace],
                "expected `{` after return type",
            )?)
        } else {
            None
        };

        let body = self.parse_block()?;
        let span = start.span.join(body.span);
        Ok(AstFnDecl {
            name,
            name_span,
            params,
            param_types,
            return_type,
            body,
            span,
        })
    }

    fn parse_param_list(
        &mut self,
    ) -> Result<(Vec<String>, Vec<Option<AstTypeAnnotation>>), ParseError> {
        self.expect(TokenKind::LParen, "expected `(` after function name")?;
        let mut params = Vec::new();
        let mut param_types = Vec::new();

        if self.match_kind(TokenKind::RParen).is_some() {
            return Ok((params, param_types));
        }

        loop {
            let (name, _) = self.expect_lower_ident()?;
            params.push(name);
            let type_annotation = if self.match_kind(TokenKind::Colon).is_some() {
                Some(self.parse_type_annotation_until(
                    &[TokenKind::Comma, TokenKind::RParen],
                    "expected `,` or `)` after parameter type",
                )?)
            } else {
                None
            };
            param_types.push(type_annotation);

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }

            if self.at(TokenKind::RParen) {
                break;
            }
        }

        self.expect(TokenKind::RParen, "expected `)` after function parameters")?;
        Ok((params, param_types))
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
        self.parse_type_annotation_until(&[TokenKind::Eq], "expected `=` after type annotation")
    }

    fn parse_type_annotation_until(
        &mut self,
        terminators: &[TokenKind],
        missing_message: &str,
    ) -> Result<AstTypeAnnotation, ParseError> {
        let start = self.current().span;
        let mut depth = 0_u32;
        while depth != 0 || !terminators.iter().any(|kind| self.at(*kind)) {
            if self.at(TokenKind::Semicolon)
                || (self.at(TokenKind::RBrace)
                    && depth == 0
                    && !terminators.contains(&TokenKind::RBrace))
                || self.at(TokenKind::Eof)
            {
                return Err(self.error_at_current(missing_message, None));
            }
            match self.current().kind {
                TokenKind::LParen | TokenKind::LBracket | TokenKind::Lt => depth += 1,
                TokenKind::RParen | TokenKind::RBracket | TokenKind::Gt if depth > 0 => depth -= 1,
                _ => {}
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

        loop {
            if self.at(TokenKind::Dot) {
                self.bump();
                if self.at(TokenKind::Int) {
                    let token = self.bump();
                    let index =
                        self.text(token.span)
                            .parse::<usize>()
                            .map_err(|error| ParseError {
                                span: token.span,
                                message: format!("invalid tuple projection index: {error}"),
                                help: None,
                            })?;
                    let span = expr_span(&expr).join(token.span);
                    expr = AstExpr::TupleIndex {
                        base: Box::new(expr),
                        index,
                        span,
                    };
                    continue;
                }
                let (field, field_span) = self.expect_ident()?;
                let span = expr_span(&expr).join(field_span);
                expr = match expr {
                    AstExpr::Path { mut path, span: _ }
                        if path
                            .segments
                            .first()
                            .and_then(|segment| segment.chars().next())
                            .is_some_and(|ch| ch.is_ascii_uppercase()) =>
                    {
                        path.segments.push(field);
                        AstExpr::Path { path, span }
                    }
                    base => AstExpr::Field {
                        base: Box::new(base),
                        field,
                        span,
                    },
                };
                continue;
            }

            if self.at(TokenKind::LParen) {
                let callee_span = expr_span(&expr);
                let args = self.parse_arg_list()?;
                let end = self.previous_span();
                expr = if let AstExpr::Path { path, span } = expr {
                    AstExpr::Call {
                        callee: path,
                        args,
                        span: span.join(end),
                    }
                } else {
                    AstExpr::Apply {
                        callee: Box::new(expr),
                        args,
                        span: callee_span.join(end),
                    }
                };
                continue;
            }

            if let AstExpr::Path { path, span } = &expr
                && self.record_literal_follows(path)
            {
                let path = path.clone();
                let span = *span;
                expr = self.parse_record(path, span)?;
                continue;
            }

            break;
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
            TokenKind::Fn => self.parse_lambda(),
            TokenKind::Spawn => self.parse_spawn(),
            TokenKind::Receive => self.parse_receive(),
            TokenKind::Match => self.parse_match(),
            TokenKind::Ident => {
                let (head, span) = self.expect_ident()?;
                let path = AstPath {
                    segments: vec![head],
                };
                if self.record_literal_follows(&path) {
                    self.parse_record(path, span)
                } else {
                    Ok(AstExpr::Path { path, span })
                }
            }
            TokenKind::If => self.parse_if(),
            TokenKind::LBrace => self.parse_block_expr(),
            TokenKind::LParen => self.parse_paren_or_tuple_or_unit(),
            TokenKind::LBracket => self.parse_list(),
            _ => Err(self.error_at_current("expected expression", None)),
        }
    }

    fn parse_lambda(&mut self) -> Result<AstExpr, ParseError> {
        let start = self.expect(TokenKind::Fn, "expected lambda expression")?;
        let (params, param_types) = self.parse_param_list()?;
        let body = self.parse_block()?;
        let span = start.span.join(body.span);
        Ok(AstExpr::Lambda {
            params,
            param_types,
            body: Box::new(body),
            span,
        })
    }

    fn parse_match(&mut self) -> Result<AstExpr, ParseError> {
        let start = self.expect(TokenKind::Match, "expected `match`")?;
        let scrutinee = self.parse_expr()?;
        self.expect(TokenKind::LBrace, "expected `{` after match scrutinee")?;
        let mut arms = Vec::new();

        if self.at(TokenKind::RBrace) {
            return Err(self.error_at_current("match expression needs at least one arm", None));
        }

        loop {
            let pattern = self.parse_pattern()?;
            self.expect(TokenKind::Arrow, "expected `->` after match pattern")?;
            let body = self.parse_expr()?;
            arms.push(AstMatchArm { pattern, body });

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }

            if self.at(TokenKind::RBrace) {
                break;
            }
        }

        let end = self.expect(TokenKind::RBrace, "expected `}` after match arms")?;
        Ok(AstExpr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            span: start.span.join(end.span),
        })
    }

    fn parse_pattern(&mut self) -> Result<AstPattern, ParseError> {
        if self.at(TokenKind::True) || self.at(TokenKind::False) {
            let token = self.bump();
            return Ok(AstPattern::Bool {
                value: token.kind == TokenKind::True,
                span: token.span,
            });
        }

        match self.current().kind {
            TokenKind::Ident => {
                let (name, mut span) = self.expect_ident()?;
                if name == "_" {
                    Ok(AstPattern::Wildcard { span })
                } else if is_upper_ident(&name) {
                    let mut path = AstPath {
                        segments: vec![name],
                    };
                    while self.match_kind(TokenKind::Dot).is_some() {
                        let (segment, segment_span) = self.expect_ident()?;
                        span = span.join(segment_span);
                        path.segments.push(segment);
                    }
                    if self.at(TokenKind::LBrace) {
                        return self.parse_record_pattern(path, span);
                    }
                    let payload = if self.match_kind(TokenKind::LParen).is_some() {
                        let mut payload = Vec::new();
                        if self.match_kind(TokenKind::RParen).is_none() {
                            loop {
                                payload.push(self.parse_pattern()?);
                                if self.match_kind(TokenKind::Comma).is_none() {
                                    break;
                                }
                                if self.at(TokenKind::RParen) {
                                    break;
                                }
                            }
                            let end = self.expect(
                                TokenKind::RParen,
                                "expected `)` after constructor pattern payload",
                            )?;
                            span = span.join(end.span);
                        }
                        payload
                    } else {
                        Vec::new()
                    };
                    Ok(AstPattern::Constructor {
                        path,
                        payload,
                        span,
                    })
                } else if is_lower_ident(&name) {
                    if self.at(TokenKind::LBrace) {
                        return self.parse_record_pattern(
                            AstPath {
                                segments: vec![name],
                            },
                            span,
                        );
                    }
                    Ok(AstPattern::Bind { name, span })
                } else {
                    Err(ParseError {
                        span,
                        message: format!("expected pattern identifier, found `{name}`"),
                        help: Some(
                            "stage0 patterns currently support literals, constructors, `_`, and binders",
                        ),
                    })
                }
            }
            TokenKind::Int => {
                let token = self.expect(TokenKind::Int, "expected integer pattern")?;
                let raw = self.text(token.span);
                let value = parse_int_literal(raw).map_err(|message| ParseError {
                    span: token.span,
                    message,
                    help: None,
                })?;
                Ok(AstPattern::Int {
                    value,
                    span: token.span,
                })
            }
            TokenKind::String => {
                let token = self.expect(TokenKind::String, "expected string pattern")?;
                let raw = self.text(token.span);
                let value = snailquote::unescape(raw).map_err(|error| ParseError {
                    span: token.span,
                    message: format!("invalid string pattern: {error}"),
                    help: None,
                })?;
                Ok(AstPattern::String {
                    value,
                    span: token.span,
                })
            }
            TokenKind::LParen => {
                let start = self.expect(TokenKind::LParen, "expected tuple or unit pattern")?;
                if let Some(end) = self.match_kind(TokenKind::RParen) {
                    return Ok(AstPattern::Unit {
                        span: start.span.join(end),
                    });
                }
                let first = self.parse_pattern()?;
                if self.match_kind(TokenKind::Comma).is_none() {
                    self.expect(TokenKind::RParen, "expected `)` after pattern")?;
                    return Ok(first);
                }
                let mut items = vec![first];
                while !self.at(TokenKind::RParen) {
                    items.push(self.parse_pattern()?);
                    if self.match_kind(TokenKind::Comma).is_none() {
                        break;
                    }
                }
                let end = self.expect(TokenKind::RParen, "expected `)` after tuple pattern")?;
                Ok(AstPattern::Tuple {
                    items,
                    span: start.span.join(end.span),
                })
            }
            _ => Err(self.error_at_current(
                "expected match pattern",
                Some("stage0 patterns currently support literals, constructors, `_`, and binders"),
            )),
        }
    }

    fn parse_record_pattern(
        &mut self,
        path: AstPath,
        path_span: TextSpan,
    ) -> Result<AstPattern, ParseError> {
        self.expect(TokenKind::LBrace, "expected `{` after record pattern path")?;
        let mut fields = Vec::new();

        if let Some(end) = self.match_kind(TokenKind::RBrace) {
            return Ok(AstPattern::Record {
                path,
                fields,
                span: path_span.join(end),
            });
        }

        loop {
            let (field, field_span) = self.expect_lower_ident()?;
            let pattern = if self.match_kind(TokenKind::Colon).is_some() {
                self.parse_pattern()?
            } else {
                AstPattern::Bind {
                    name: field.clone(),
                    span: field_span,
                }
            };
            fields.push((field, pattern));

            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }

            if self.at(TokenKind::RBrace) {
                break;
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "expected `}` after record pattern fields",
        )?;
        Ok(AstPattern::Record {
            path,
            fields,
            span: path_span.join(end.span),
        })
    }

    fn parse_spawn(&mut self) -> Result<AstExpr, ParseError> {
        let start = self.expect(TokenKind::Spawn, "expected `spawn`")?;
        let body = self.parse_block()?;
        let span = start.span.join(body.span);
        Ok(AstExpr::Spawn {
            body: Box::new(body),
            span,
        })
    }

    fn parse_receive(&mut self) -> Result<AstExpr, ParseError> {
        let start = self.expect(TokenKind::Receive, "expected `receive`")?;
        self.expect(TokenKind::LBrace, "expected `{` after `receive`")?;
        let (binder, binder_span) = self.expect_lower_ident()?;
        self.expect(TokenKind::Arrow, "expected `->` after receive binder")?;
        let body = self.parse_expr()?;
        let end = self.expect(TokenKind::RBrace, "expected `}` after receive body")?;
        Ok(AstExpr::Receive {
            binder,
            binder_span,
            body: Box::new(body),
            span: start.span.join(end.span),
        })
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

        let span = block.span;
        Ok(AstExpr::Block {
            block: Box::new(block),
            span,
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

    fn record_literal_follows(&self, path: &AstPath) -> bool {
        if !self.at(TokenKind::LBrace) {
            return false;
        }

        let path_allows_empty_record = path.segments.len() > 1
            || path
                .segments
                .first()
                .and_then(|segment| segment.chars().next())
                .is_some_and(|ch| ch.is_ascii_uppercase());

        match (self.peek_kind(1), self.peek_kind(2)) {
            (Some(TokenKind::RBrace), _) => path_allows_empty_record,
            (Some(TokenKind::Ident), Some(TokenKind::Colon)) => true,
            _ => false,
        }
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

        if is_lower_ident(&ident) {
            Ok((ident, span))
        } else {
            Err(ParseError {
                span,
                message: format!("expected lowercase identifier, found `{ident}`"),
                help: None,
            })
        }
    }

    fn expect_upper_ident(&mut self) -> Result<(String, TextSpan), ParseError> {
        let (ident, span) = self.expect_ident()?;

        if is_upper_ident(&ident) {
            Ok((ident, span))
        } else {
            Err(ParseError {
                span,
                message: format!("expected uppercase variant constructor, found `{ident}`"),
                help: Some("variant constructors are ClassCase, for example `Some` or `None`"),
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

    fn peek_kind(&self, offset: usize) -> Option<TokenKind> {
        self.tokens
            .get(self.cursor + offset)
            .map(|token| token.kind)
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

fn is_lower_ident(ident: &str) -> bool {
    ident
        .chars()
        .next()
        .is_some_and(|ch| ch == '_' || ch.is_ascii_lowercase())
}

fn is_upper_ident(ident: &str) -> bool {
    ident
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

#[cfg(test)]
mod tests {
    use camino::Utf8Path;

    use crate::ast::{AstDecl, AstExpr, AstStmt, AstTypeBody};

    use super::parse_source;

    #[test]
    fn parses_expression_lambdas() {
        let program = parse_source(
            Utf8Path::new("lambda.ml"),
            "fn main() { let f = fn(n) { fn(x) { x + n } }; dbg(\"ok\") }\n",
        )
        .unwrap();

        let AstDecl::Function(function) = &program.decls[0] else {
            panic!("expected function declaration");
        };
        let AstStmt::Let { value, .. } = &function.body.statements[0] else {
            panic!("expected lambda let binding");
        };
        let AstExpr::Lambda { params, body, .. } = value else {
            panic!("expected outer lambda");
        };
        assert_eq!(params, &["n"]);
        assert!(matches!(body.tail.as_ref(), Some(AstExpr::Lambda { .. })));
    }

    #[test]
    fn parses_apply_for_non_path_callees() {
        let program = parse_source(
            Utf8Path::new("apply.ml"),
            "fn main() { dbg((fn(x) { x })(1)) }\n",
        )
        .unwrap();

        let AstDecl::Function(function) = &program.decls[0] else {
            panic!("expected function declaration");
        };
        let Some(AstExpr::Call { args, .. }) = &function.body.tail else {
            panic!("expected dbg call");
        };
        assert!(matches!(args.first(), Some(AstExpr::Apply { .. })));
    }

    #[test]
    fn parses_record_type_declarations() {
        let program = parse_source(
            Utf8Path::new("record_type.ml"),
            "type point = { coords: (i64, i64), label: string }\nfn main() { dbg(\"ok\") }\n",
        )
        .unwrap();

        let AstDecl::Type(type_) = &program.decls[0] else {
            panic!("expected type declaration");
        };
        let AstTypeBody::Record { fields } = &type_.body else {
            panic!("expected record type declaration");
        };
        assert_eq!(fields[0].name, "coords");
        assert_eq!(fields[0].type_annotation.text, "(i64, i64)");
        assert_eq!(fields[1].name, "label");
    }

    #[test]
    fn parses_qualified_record_literals() {
        let program = parse_source(
            Utf8Path::new("record_literal.ml"),
            "fn main() { dbg(Geometry.point { x: 1, y: 2 }.x) }\n",
        )
        .unwrap();

        let AstDecl::Function(function) = &program.decls[0] else {
            panic!("expected function declaration");
        };
        let Some(AstExpr::Call { args, .. }) = &function.body.tail else {
            panic!("expected dbg call");
        };
        let Some(AstExpr::Field { base, .. }) = args.first() else {
            panic!("expected record field projection");
        };
        let AstExpr::Record { path, .. } = base.as_ref() else {
            panic!("expected qualified record literal");
        };
        assert_eq!(path.segments, ["Geometry", "point"]);
    }
}
