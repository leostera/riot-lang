//! Recursive-descent parser for TinyML.
//!
//! This parser consumes the lossless token stream produced by `lexer.rs` and
//! builds the surface AST from `ast.rs`. The shape is intentionally simple:
//!
//! - module-level forms are parsed by declaration parsers (`parse_*_decl`)
//! - local `let` bindings are parsed as expressions (`parse_let_expr`)
//! - expressions use a Pratt parser for precedence and left/right binding power
//! - patterns and types are parsed separately from expressions, even when they
//!   share syntax such as constructors, records, tuples, and unit
//!
//! Style note: when all branches consume the current token, use `next()`; when
//! routing depends on inspecting a token before optionally consuming it, use
//! `peek()` followed by `bump()` in the consuming branch.

use std::collections::HashMap;

use miette::{Diagnostic, SourceSpan};
use once_cell::sync::Lazy;
use thiserror::Error;

use super::ast::*;
use super::lexer::{Lexer, Span, Token, TokenKind};

static INFIX_BINDING_POWER: Lazy<HashMap<TokenKind, (&'static str, u8, u8)>> = Lazy::new(|| {
    HashMap::from([
        (TokenKind::Plus, ("+", 10, 11)),
        (TokenKind::Minus, ("-", 10, 11)),
        (TokenKind::Star, ("*", 12, 13)),
        (TokenKind::Slash, ("/", 12, 13)),
        (TokenKind::Percent, ("%", 12, 13)),
    ])
});

#[derive(Debug, Error, Diagnostic)]
#[error("parse error")]
pub struct ParseError {
    #[source_code]
    pub src: String,
    #[label("{message}")]
    pub span: SourceSpan,
    pub message: String,
}

/// Parse a full TinyML source file into a module AST.
///
/// The lexer provides a `TokenKind::Eof` sentinel; this function parses module
/// items until it reaches that sentinel.
pub fn parse_module(src: &str, lexer: Lexer) -> Result<Module, ParseError> {
    Parser { src, lexer }.parse_module()
}

/// Stateful recursive-descent parser.
///
/// `src` provides diagnostic source text. `lexer` owns the token stream and
/// cursor.
struct Parser<'a> {
    src: &'a str,
    lexer: Lexer,
}

impl Parser<'_> {
    /// Parse all top-level module items.
    ///
    /// TinyML module items appear as adjacent declarations. Each declaration
    /// parser consumes exactly the tokens belonging to that declaration.
    fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut items = Vec::new();
        while !self.lexer.at(&TokenKind::Eof) {
            items.push(self.parse_item()?);
        }
        Ok(Module { items })
    }

    /// Route a top-level declaration by its leading keyword.
    ///
    /// This is a good `next()` use-case: every successful branch consumes the
    /// keyword and then delegates to the matching declaration parser.
    fn parse_item(&mut self) -> Result<ModuleItem, ParseError> {
        match self.lexer.next() {
            TokenKind::Use => self.parse_use_decl().map(ModuleItem::UseDecl),
            TokenKind::Type => self.parse_type_decl().map(ModuleItem::TypeDecl),
            TokenKind::Let => self.parse_let_decl().map(ModuleItem::LetDecl),
            _ => self.err("expected item: use, type, or let"),
        }
    }

    /// Parse a module-level `let` declaration.
    ///
    /// Module declarations are implicitly corecursive, so `let rec` is rejected.
    /// After the name, `=` means a constant declaration; otherwise we parse one
    /// or more function argument patterns before `=`.
    fn parse_let_decl(&mut self) -> Result<LetDecl, ParseError> {
        let start = self.lexer.prev_span().start;
        if self.lexer.at(&TokenKind::Rec) {
            return self.err("module-level let declarations are already corecursive; remove `rec`");
        }
        let name = self.parse_ident()?;

        let mut args = Vec::new();
        while !self.lexer.at(&TokenKind::Equal) {
            args.push(self.parse_pattern()?);
        }
        self.expect(&TokenKind::Equal)?;
        let body = self.parse_expr()?;
        let span = Span::new(start, self.lexer.prev_span().end);
        if args.is_empty() {
            Ok(LetDecl::Const { name, body, span })
        } else {
            Ok(LetDecl::Fn {
                name,
                args,
                body,
                span,
            })
        }
    }

    /// Parse a local/block `let` expression.
    ///
    /// Local `let` binds a full pattern and becomes an `Expr::Let` inside an
    /// `Expr::Block`.
    fn parse_let_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::Let)?.span;
        let bind = self.parse_pattern()?;
        let hint = match self.lexer.peek() {
            TokenKind::Colon => {
                self.lexer.bump();
                Some(self.parse_type_expr()?)
            }
            _ => None,
        };
        self.expect(&TokenKind::Equal)?;
        let value = self.parse_expr()?;
        let span = start.join(value.span());
        Ok(Expr::Let {
            bind,
            hint,
            body: Box::new(value),
            span,
        })
    }

    /// Parse a module-level type declaration.
    ///
    /// The right-hand side can be a record body, a variant body, or an alias.
    fn parse_type_decl(&mut self) -> Result<TypeDecl, ParseError> {
        let start = self.lexer.prev_span().start;
        let name = self.parse_ident()?;
        let params = self.parse_type_params()?;
        self.expect(&TokenKind::Equal)?;
        let body = match self.lexer.peek() {
            TokenKind::Pipe | TokenKind::Constructor(_) => {
                let variants = self.parse_variants()?;
                let span = variants
                    .first()
                    .map(|first| first.span().join(variants.last().expect("variant").span()))
                    .unwrap_or_else(|| self.lexer.prev_span());
                TypeExpr::Variant { variants, span }
            }
            _ => self.parse_type_expr()?,
        };
        Ok(TypeDecl {
            name,
            params,
            body,
            span: Span::new(start, self.lexer.prev_span().end),
        })
    }

    /// Parse a variant constructor list.
    fn parse_variants(&mut self) -> Result<Vec<Variant>, ParseError> {
        let mut variants = Vec::new();
        while self.lexer.eat(&TokenKind::Pipe).is_some() || variants.is_empty() {
            let ident = self.expect_constructor()?;
            let variant = match self.lexer.peek() {
                TokenKind::LBrace => {
                    self.lexer.bump();
                    let fields = self.parse_record_type_fields()?;
                    let span = ident.span().join(self.lexer.prev_span());
                    Variant::RecordConstructor {
                        ident,
                        fields,
                        span,
                    }
                }
                TokenKind::LParen => {
                    self.lexer.bump();
                    let args = self.parse_comma_types_until(TokenKind::RParen)?;
                    let end = self.expect(&TokenKind::RParen)?.span;
                    let span = ident.span().join(end);
                    Variant::TupleConstructor { ident, args, span }
                }
                _ => {
                    let span = ident.span();
                    Variant::TupleConstructor {
                        ident,
                        args: Vec::new(),
                        span,
                    }
                }
            };
            variants.push(variant);
            if !self.lexer.at(&TokenKind::Pipe) {
                break;
            }
        }
        Ok(variants)
    }

    /// Parse an expression using the Pratt parser entry point.
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_bp(0)
    }

    // Pratt parser: prefix atoms + infix operators + postfix/call application.
    /// Pratt parser core.
    ///
    /// `min_bp` is the minimum left binding power accepted by this call.
    /// Higher binding power means tighter binding. Postfix record construction
    /// binds tighter than function application, and application binds tighter
    /// than arithmetic-like binary operators.
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix()?;

        loop {
            if self.lexer.at(&TokenKind::LBrace) {
                let l_bp = 30;
                if l_bp < min_bp {
                    break;
                }
                match expr_into_ident(lhs) {
                    Ok(name) => {
                        self.lexer.bump();
                        let fields = self.parse_record_expr_fields()?;
                        let end = self.expect(&TokenKind::RBrace)?.span;
                        let span = name.span().join(end);
                        lhs = Expr::Record { name, fields, span };
                        continue;
                    }
                    Err(expr) => lhs = expr,
                }
            }

            // Juxtaposition application: `f x y`, tighter than binary ops.
            if self.starts_prefix() {
                let (l_bp, r_bp) = (21, 22);
                if l_bp < min_bp {
                    break;
                }
                let mut args = vec![self.parse_expr_bp(r_bp)?];
                while self.starts_prefix() {
                    args.push(self.parse_expr_bp(r_bp)?);
                }
                let span = lhs
                    .span()
                    .join(args.last().expect("application arg").span());
                lhs = Expr::Apply {
                    callee: Box::new(lhs),
                    args,
                    span,
                };
                continue;
            }

            let Some((ident, l_bp, r_bp)) = self.infix_binding_power() else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            self.lexer.bump();
            let rhs = self.parse_expr_bp(r_bp)?;
            let span = lhs.span().join(rhs.span());
            lhs = Expr::BinaryOp {
                ident,
                left: Box::new(lhs),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(lhs)
    }

    /// Return binary operator identity and binding powers for the current token.
    ///
    /// Operators are represented in the AST as identifiers. Later phases can
    /// treat operators like ordinary named functions.
    fn infix_binding_power(&self) -> Option<(Ident, u8, u8)> {
        INFIX_BINDING_POWER
            .get(self.lexer.peek())
            .map(|(name, l_bp, r_bp)| {
                (
                    Ident::Name {
                        name: (*name).into(),
                        span: self.lexer.current_span(),
                    },
                    *l_bp,
                    *r_bp,
                )
            })
    }

    /// Parse expression forms that can appear at the start of an expression.
    ///
    /// This handles literals, variables, constructors, unit, lambdas, matches,
    /// parenthesized/tuple expressions, and block expressions.
    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        match self.lexer.peek() {
            TokenKind::Int(n) => {
                let n = n.to_string();
                let span = self.lexer.bump().span;
                Ok(Expr::Literal {
                    value: Literal::Int(n),
                    span,
                })
            }
            TokenKind::Float(n) => {
                let n = n.to_string();
                let span = self.lexer.bump().span;
                Ok(Expr::Literal {
                    value: Literal::Float(n),
                    span,
                })
            }
            TokenKind::String(s) => {
                let s = s.to_string();
                let span = self.lexer.bump().span;
                Ok(Expr::Literal {
                    value: Literal::String(s),
                    span,
                })
            }
            TokenKind::True => {
                let span = self.lexer.bump().span;
                Ok(Expr::Literal {
                    value: Literal::Bool(true),
                    span,
                })
            }
            TokenKind::False => {
                let span = self.lexer.bump().span;
                Ok(Expr::Literal {
                    value: Literal::Bool(false),
                    span,
                })
            }
            TokenKind::Ident(_) => {
                let name = self.parse_ident()?;
                let span = name.span();
                Ok(Expr::Var { name, span })
            }
            TokenKind::Unit => {
                let span = self.lexer.bump().span;
                Ok(Expr::Constructor {
                    name: Ident::Name {
                        name: "()".into(),
                        span,
                    },
                    args: vec![],
                    span,
                })
            }
            TokenKind::Constructor(name) => {
                let name = name.to_string();
                let name_span = self.lexer.bump().span;
                let args = match self.lexer.peek() {
                    TokenKind::LParen => {
                        self.lexer.bump();
                        let mut args = Vec::new();
                        while !self.lexer.at(&TokenKind::RParen) {
                            args.push(self.parse_expr()?);
                            if self.lexer.eat(&TokenKind::Comma).is_none() {
                                break;
                            }
                        }
                        self.expect(&TokenKind::RParen)?;
                        args
                    }
                    _ => Vec::new(),
                };
                let span = args
                    .last()
                    .map(|arg| name_span.join(arg.span()))
                    .unwrap_or(name_span);
                Ok(Expr::Constructor {
                    name: Ident::Name {
                        name,
                        span: name_span,
                    },
                    args,
                    span,
                })
            }
            TokenKind::Fn => self.parse_fn(),
            TokenKind::Match => self.parse_match(),
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::LBrace => self.parse_brace_expr(),
            _ => self.err("expected expression"),
        }
    }

    /// Parse lambda syntax: `fn <patterns...> -> <expr>`.
    fn parse_fn(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::Fn)?.span;
        let mut params = Vec::new();
        while self.starts_pattern() {
            params.push(self.parse_pattern()?);
        }
        if params.is_empty() {
            return self.err("expected at least one function parameter");
        }
        self.expect(&TokenKind::Arrow)?;
        let body = self.parse_expr()?;
        let span = start.join(body.span());
        Ok(Expr::Fn {
            params,
            body: Box::new(body),
            span,
        })
    }

    /// Parse match syntax: `match <expr> { | <pat> -> <expr>, ... }`.
    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::Match)?.span;
        let scrutinee = self.parse_expr_bp(31)?;
        self.expect(&TokenKind::LBrace)?;
        let mut arms = Vec::new();
        while self.lexer.eat(&TokenKind::Pipe).is_some() {
            let pattern = self.parse_pattern()?;
            self.expect(&TokenKind::Arrow)?;
            let body = self.parse_expr()?;
            let span = pattern.span().join(body.span());
            arms.push(MatchArm {
                pattern,
                body,
                span,
            });
            self.lexer.eat(&TokenKind::Comma);
        }
        let end = self.expect(&TokenKind::RBrace)?.span;
        if arms.is_empty() {
            return self.err("expected at least one match arm");
        }
        Ok(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            span: start.join(end),
        })
    }

    /// Parse parenthesized expressions and tuple expressions.
    ///
    /// Unit `()` is lexed as a dedicated token and handled by `parse_prefix`.
    /// This function parses grouped expressions and tuple expressions.
    fn parse_paren_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::LParen)?.span;
        let first = self.parse_expr()?;
        match self.lexer.peek() {
            TokenKind::Colon => {
                self.lexer.bump();
                let hint = self.parse_type_expr()?;
                let end = self.expect(&TokenKind::RParen)?.span;
                Ok(Expr::TypeHint {
                    expr: Box::new(first),
                    hint,
                    span: start.join(end),
                })
            }
            TokenKind::Comma => {
                self.lexer.bump();
                let mut items = vec![first];
                while !self.lexer.at(&TokenKind::RParen) {
                    items.push(self.parse_expr()?);
                    if self.lexer.eat(&TokenKind::Comma).is_none() {
                        break;
                    }
                }
                let end = self.expect(&TokenKind::RParen)?.span;
                Ok(Expr::Tuple {
                    items,
                    span: start.join(end),
                })
            }
            _ => {
                self.expect(&TokenKind::RParen)?;
                Ok(first)
            }
        }
    }

    /// Parse block expressions.
    ///
    /// Blocks contain a semicolon-separated sequence of expressions. Local
    /// `let` forms are parsed as `Expr::Let`; the block itself is `Expr::Block`.
    /// Empty `{}` currently lowers to the unit constructor.
    fn parse_brace_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::LBrace)?.span;
        match self.lexer.peek() {
            TokenKind::RBrace => {
                let end = self.lexer.bump().span;
                let span = start.join(end);
                return Ok(Expr::Constructor {
                    name: Ident::Name {
                        name: "()".into(),
                        span,
                    },
                    args: vec![],
                    span,
                });
            }
            _ => {}
        }

        let mut exprs = Vec::new();
        while !self.lexer.at(&TokenKind::RBrace) {
            match self.lexer.peek() {
                TokenKind::Let => exprs.push(self.parse_let_expr()?),
                _ => exprs.push(self.parse_expr()?),
            }

            if self.lexer.at(&TokenKind::RBrace) {
                break;
            }
            self.expect(&TokenKind::Semi)?;
        }
        let end = self.expect(&TokenKind::RBrace)?.span;
        Ok(Expr::Block {
            exprs,
            span: start.join(end),
        })
    }

    /// Parse fields inside a record expression after the opening `{`.
    fn parse_record_expr_fields(&mut self) -> Result<Vec<(Ident, Expr)>, ParseError> {
        let mut fields = Vec::new();
        while !self.lexer.at(&TokenKind::RBrace) {
            let name = self.parse_ident()?;
            self.expect(&TokenKind::Colon)?;
            let value = self.parse_expr()?;
            fields.push((name, value));
            if self.lexer.eat(&TokenKind::Comma).is_none() {
                break;
            }
        }
        Ok(fields)
    }

    /// Parse a pattern.
    ///
    /// Patterns intentionally mirror much of expression syntax, but produce a
    /// separate AST. This is used by function params, match arms, and local lets.
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.lexer.peek() {
            TokenKind::Ident(s) if s == "_" => {
                let span = self.lexer.bump().span;
                Ok(Pattern::Wildcard { span })
            }
            TokenKind::Ident(_) => {
                let name = self.parse_ident()?;
                let span = name.span();
                Ok(Pattern::Var { name, span })
            }
            TokenKind::Int(n) => {
                let n = n.to_string();
                let span = self.lexer.bump().span;
                Ok(Pattern::Literal {
                    value: Literal::Int(n),
                    span,
                })
            }
            TokenKind::Float(n) => {
                let n = n.to_string();
                let span = self.lexer.bump().span;
                Ok(Pattern::Literal {
                    value: Literal::Float(n),
                    span,
                })
            }
            TokenKind::String(s) => {
                let s = s.to_string();
                let span = self.lexer.bump().span;
                Ok(Pattern::Literal {
                    value: Literal::String(s),
                    span,
                })
            }
            TokenKind::True => {
                let span = self.lexer.bump().span;
                Ok(Pattern::Literal {
                    value: Literal::Bool(true),
                    span,
                })
            }
            TokenKind::False => {
                let span = self.lexer.bump().span;
                Ok(Pattern::Literal {
                    value: Literal::Bool(false),
                    span,
                })
            }
            TokenKind::Unit => {
                let span = self.lexer.bump().span;
                Ok(Pattern::Constructor {
                    name: Ident::Name {
                        name: "()".into(),
                        span,
                    },
                    args: vec![],
                    span,
                })
            }
            TokenKind::LParen => self.parse_paren_pattern(),
            TokenKind::Constructor(_) => {
                let name = self.parse_ident()?;
                match self.lexer.peek() {
                    TokenKind::LBrace => {
                        self.lexer.bump();
                        let fields = self.parse_record_pattern_fields()?;
                        let end = self.expect(&TokenKind::RBrace)?.span;
                        let span = name.span().join(end);
                        Ok(Pattern::Record { name, fields, span })
                    }
                    TokenKind::LParen => {
                        self.lexer.bump();
                        let mut args = Vec::new();
                        while !self.lexer.at(&TokenKind::RParen) {
                            args.push(self.parse_pattern()?);
                            if self.lexer.eat(&TokenKind::Comma).is_none() {
                                break;
                            }
                        }
                        let end = self.expect(&TokenKind::RParen)?.span;
                        let span = name.span().join(end);
                        Ok(Pattern::Constructor { name, args, span })
                    }
                    _ => {
                        let span = name.span();
                        Ok(Pattern::Constructor {
                            name,
                            args: Vec::new(),
                            span,
                        })
                    }
                }
            }
            _ => self.err("expected pattern"),
        }
    }

    /// Parse parenthesized and tuple patterns.
    ///
    /// Unit `()` is a dedicated token handled in `parse_pattern`. This function
    /// parses grouped patterns and tuple patterns.
    fn parse_paren_pattern(&mut self) -> Result<Pattern, ParseError> {
        let start = self.expect(&TokenKind::LParen)?.span;
        let first = self.parse_pattern()?;
        match self.lexer.peek() {
            TokenKind::Colon => {
                self.lexer.bump();
                let hint = self.parse_type_expr()?;
                let end = self.expect(&TokenKind::RParen)?.span;
                Ok(Pattern::TypeHint {
                    pattern: Box::new(first),
                    hint,
                    span: start.join(end),
                })
            }
            TokenKind::Comma => {
                self.lexer.bump();
                let mut items = vec![first];
                while !self.lexer.at(&TokenKind::RParen) {
                    items.push(self.parse_pattern()?);
                    if self.lexer.eat(&TokenKind::Comma).is_none() {
                        break;
                    }
                }
                let end = self.expect(&TokenKind::RParen)?.span;
                Ok(Pattern::Tuple {
                    items,
                    span: start.join(end),
                })
            }
            _ => {
                self.expect(&TokenKind::RParen)?;
                Ok(first)
            }
        }
    }

    /// Parse record pattern fields.
    ///
    /// Field punning is supported: `{ name }` means `{ name: name }`. Qualified
    /// fields use explicit patterns, as in `{ module.name: pattern }`.
    fn parse_record_pattern_fields(&mut self) -> Result<Vec<(Ident, Pattern)>, ParseError> {
        let mut fields = Vec::new();
        while !self.lexer.at(&TokenKind::RBrace) {
            let name = self.parse_ident()?;
            let pattern = match self.lexer.peek() {
                TokenKind::Colon => {
                    self.lexer.bump();
                    self.parse_pattern()?
                }
                _ => match &name {
                    Ident::Name { name, span } => Pattern::Var {
                        name: Ident::Name {
                            name: name.to_string(),
                            span: *span,
                        },
                        span: *span,
                    },
                    _ => return self.err("expected pattern after qualified record field"),
                },
            };
            fields.push((name, pattern));
            if self.lexer.eat(&TokenKind::Comma).is_none() {
                break;
            }
        }
        Ok(fields)
    }

    /// Parse optional type parameters: `<a, b, c>`.
    fn parse_type_params(&mut self) -> Result<Vec<TypeVar>, ParseError> {
        match self.lexer.peek() {
            TokenKind::LAngle => {
                self.lexer.bump();
                let mut params = Vec::new();
                loop {
                    params.push(self.parse_type_var()?);
                    if self.lexer.eat(&TokenKind::Comma).is_none() {
                        break;
                    }
                }
                self.expect(&TokenKind::RAngle)?;
                Ok(params)
            }
            _ => Ok(Vec::new()),
        }
    }

    /// Parse a type expression.
    ///
    /// Named types are represented as `TypeExpr::App { name, args }`, even when
    /// `args` is empty. Type variables use apostrophe syntax, such as `'a`.
    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        match self.lexer.peek() {
            TokenKind::Ident(_) | TokenKind::Constructor(_) => {
                let name = self.parse_ident()?;
                match self.lexer.peek() {
                    TokenKind::LBrace => {
                        self.lexer.bump();
                        let fields = self.parse_record_type_fields()?;
                        let span = name.span().join(self.lexer.prev_span());
                        return Ok(TypeExpr::Record { name, fields, span });
                    }
                    _ => {}
                }
                let args = match self.lexer.peek() {
                    TokenKind::LAngle => {
                        self.lexer.bump();
                        let args = self.parse_comma_types_until(TokenKind::RAngle)?;
                        self.expect(&TokenKind::RAngle)?;
                        args
                    }
                    _ => Vec::new(),
                };
                let span = args
                    .last()
                    .map(|arg| name.span().join(arg.span()))
                    .unwrap_or_else(|| name.span());
                Ok(TypeExpr::App { name, args, span })
            }
            TokenKind::TypeVar(_) => {
                let var = self.parse_type_var()?;
                let span = var.span;
                Ok(TypeExpr::Var { var, span })
            }
            TokenKind::LParen => {
                self.lexer.bump();
                let start = self.lexer.prev_span();
                let items = self.parse_comma_types_until(TokenKind::RParen)?;
                let end = self.expect(&TokenKind::RParen)?.span;
                Ok(TypeExpr::Tuple {
                    items,
                    span: start.join(end),
                })
            }
            TokenKind::LBrace => self.err("expected named record type"),
            _ => self.err("expected type"),
        }
    }

    /// Parse fields inside a named record type expression or declaration.
    fn parse_record_type_fields(&mut self) -> Result<Vec<(Ident, TypeExpr)>, ParseError> {
        let mut fields = Vec::new();
        while !self.lexer.at(&TokenKind::RBrace) {
            let name = self.parse_ident()?;
            self.expect(&TokenKind::Colon)?;
            let ty = self.parse_type_expr()?;
            fields.push((name, ty));
            if self.lexer.eat(&TokenKind::Comma).is_none() {
                break;
            }
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(fields)
    }

    /// Parse comma-separated type expressions until `end` is reached.
    fn parse_comma_types_until(&mut self, end: TokenKind) -> Result<Vec<TypeExpr>, ParseError> {
        let mut tys = Vec::new();
        while !self.lexer.at(&end) {
            tys.push(self.parse_type_expr()?);
            if self.lexer.eat(&TokenKind::Comma).is_none() {
                break;
            }
        }
        Ok(tys)
    }

    /// Return whether the current token can start a pattern.
    fn starts_pattern(&self) -> bool {
        matches!(
            self.lexer.peek(),
            TokenKind::Ident(_)
                | TokenKind::Constructor(_)
                | TokenKind::Unit
                | TokenKind::Int(_)
                | TokenKind::Float(_)
                | TokenKind::String(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::LParen
                | TokenKind::LBrace
        )
    }

    /// Return whether the current token can start a prefix expression.
    ///
    /// This is also used by the Pratt parser to detect juxtaposition-based
    /// function application.
    fn starts_prefix(&self) -> bool {
        matches!(
            self.lexer.peek(),
            TokenKind::Int(_)
                | TokenKind::Float(_)
                | TokenKind::String(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Ident(_)
                | TokenKind::Constructor(_)
                | TokenKind::Unit
                | TokenKind::Fn
                | TokenKind::Match
                | TokenKind::LParen
                | TokenKind::LBrace
        )
    }

    /// Parse a `use` declaration after the leading `use` token.
    ///
    /// Supported forms:
    /// - `use A.B`
    /// - `use A.B.c`
    /// - `use A.B.{c, d}`
    /// - `use A.B.*`
    fn parse_use_decl(&mut self) -> Result<UseDecl, ParseError> {
        let start = self.lexer.prev_span().start;
        let module = self.parse_ident()?;

        match self.lexer.peek() {
            TokenKind::Dot => {
                self.lexer.bump();
                match self.lexer.peek() {
                    TokenKind::Star => {
                        self.lexer.bump();
                        Ok(UseDecl::Wildcard {
                            module,
                            span: Span::new(start, self.lexer.prev_span().end),
                        })
                    }
                    TokenKind::LBrace => {
                        self.lexer.bump();
                        let mut members = Vec::new();
                        while !self.lexer.at(&TokenKind::RBrace) {
                            members.push(self.parse_ident()?);
                            if self.lexer.eat(&TokenKind::Comma).is_none() {
                                break;
                            }
                        }
                        self.expect(&TokenKind::RBrace)?;
                        Ok(UseDecl::Concrete {
                            module,
                            members,
                            span: Span::new(start, self.lexer.prev_span().end),
                        })
                    }
                    _ => {
                        let member = self.parse_ident()?;
                        Ok(UseDecl::Concrete {
                            module,
                            members: vec![member],
                            span: Span::new(start, self.lexer.prev_span().end),
                        })
                    }
                }
            }
            _ => Ok(UseDecl::Concrete {
                module,
                members: Vec::new(),
                span: Span::new(start, self.lexer.prev_span().end),
            }),
        }
    }

    /// Parse a possibly-qualified identifier.
    ///
    /// Stops before `.*` and `.{...}` so `parse_use_decl` can route those forms.
    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        let (head, head_span) = self.parse_ident_part()?;
        match self.lexer.peek() {
            TokenKind::Dot
                if !matches!(self.lexer.peek_n(1), TokenKind::Star | TokenKind::LBrace) =>
            {
                self.lexer.bump();
                let rest = self.parse_ident()?;
                let span = head_span.join(rest.span());
                Ok(Ident::Qualified {
                    name: head,
                    rest: Box::new(rest),
                    span,
                })
            }
            _ => Ok(Ident::Name {
                name: head,
                span: head_span,
            }),
        }
    }

    /// Parse a type variable.
    fn parse_type_var(&mut self) -> Result<TypeVar, ParseError> {
        match self.lexer.peek() {
            TokenKind::TypeVar(name) => {
                let name = name.to_string();
                let span = self.lexer.bump().span;
                Ok(TypeVar { name, span })
            }
            _ => self.err("expected type variable"),
        }
    }

    /// Parse one identifier segment, accepting lowercase identifiers and
    /// uppercase constructors.
    fn parse_ident_part(&mut self) -> Result<(String, Span), ParseError> {
        match self.lexer.peek() {
            TokenKind::Ident(name) | TokenKind::Constructor(name) => {
                let name = name.to_string();
                let span = self.lexer.bump().span;
                Ok((name, span))
            }
            _ => self.err("expected identifier"),
        }
    }

    /// Parse an uppercase constructor name or report a parser error.
    fn expect_constructor(&mut self) -> Result<Ident, ParseError> {
        match self.lexer.peek() {
            TokenKind::Constructor(name) => {
                let name = name.to_string();
                let span = self.lexer.bump().span;
                Ok(Ident::Name { name, span })
            }
            _ => self.err("expected constructor"),
        }
    }
    /// Consume `kind` or produce a diagnostic.
    fn expect(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        match self.lexer.eat(kind) {
            Some(tok) => Ok(tok),
            None => self.err(&format!("expected {:?}", kind)),
        }
    }
    /// Build and return a parser error at the current token.
    fn err<T>(&self, message: &str) -> Result<T, ParseError> {
        Err(self.make_err(message))
    }
    /// Build a parser error at the current token.
    fn make_err(&self, message: &str) -> ParseError {
        ParseError {
            src: self.src.to_string(),
            span: self.lexer.current_span().as_source_span(),
            message: message.to_string(),
        }
    }
}

/// Convert expression nodes that can act as record constructor names into `Ident`.
///
/// Record construction is parsed as a Pratt postfix form: after parsing `User`,
/// seeing `{ ... }` rewrites the left expression into `Expr::Record`. Other
/// expressions are returned to the Pratt parser unchanged.
fn expr_into_ident(expr: Expr) -> Result<Ident, Expr> {
    match expr {
        Expr::Var { name, .. } => Ok(name),
        Expr::Constructor { name, args, .. } if args.is_empty() => Ok(name),
        expr => Err(expr),
    }
}
