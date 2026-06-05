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

use once_cell::sync::Lazy;

use super::ast::*;
use super::error::{Expected, ParseError};
use super::ident::{Ident, IdentNameError};
use super::lexer::{Lexer, Span, Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ItemVisibility {
    Private,
    Public(Option<TypeCapability>),
}

impl ItemVisibility {
    fn is_private(&self) -> bool {
        matches!(self, Self::Private)
    }

    fn type_visibility(self) -> TypeVisibility {
        match self {
            Self::Private => TypeVisibility::Private,
            Self::Public(None) => TypeVisibility::Public(TypeCapability::Name),
            Self::Public(Some(capability)) => TypeVisibility::Public(capability),
        }
    }
}

const EXPR_PROJECTION_L_BP: u8 = 31;
const EXPR_APPLICATION_L_BP: u8 = 21;
const EXPR_APPLICATION_R_BP: u8 = 22;
const MATCH_SCRUTINEE_MIN_BP: u8 = EXPR_APPLICATION_L_BP;

static INFIX_BINDING_POWER: Lazy<HashMap<TokenKind, (&'static str, u8, u8)>> = Lazy::new(|| {
    HashMap::from([
        (TokenKind::Plus, ("+", 10, 11)),
        (TokenKind::Minus, ("-", 10, 11)),
        (TokenKind::Star, ("*", 12, 13)),
        (TokenKind::Slash, ("/", 12, 13)),
        (TokenKind::Percent, ("%", 12, 13)),
    ])
});

/// Stateful recursive-descent parser.
///
/// The lexer owns the source text, token stream, and cursor.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    /// Parse all top-level module items.
    ///
    /// TinyML module items appear as adjacent declarations. Each declaration
    /// parser consumes exactly the tokens belonging to that declaration.
    pub fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut items = Vec::new();
        while !self.lexer.at(&TokenKind::Eof) {
            items.push(self.parse_item()?);
        }
        Ok(Module { items })
    }

    /// Route a top-level declaration by its leading keyword.
    ///
    /// Item-level modifiers are parsed before dispatching to the concrete
    /// declaration parser. This keeps the declaration grammar uniform: `pub`,
    /// `pub(view)`, and `pub(make)` are recognized once, then each item parser
    /// decides which modifier forms it accepts.
    fn parse_item(&mut self) -> Result<ModuleItem, ParseError> {
        let visibility = self.parse_visibility_modifier()?;
        match self.lexer.next() {
            TokenKind::Use => self.parse_use_decl(visibility).map(ModuleItem::UseDecl),
            TokenKind::Type => self
                .parse_type_decl(visibility.type_visibility())
                .map(ModuleItem::TypeDecl),
            TokenKind::Let => self.parse_let_decl(visibility).map(ModuleItem::LetDecl),
            _ => self.err(Expected::Item),
        }
    }

    fn parse_visibility_modifier(&mut self) -> Result<ItemVisibility, ParseError> {
        if !self.lexer.at(&TokenKind::Pub) {
            return Ok(ItemVisibility::Private);
        }
        self.lexer.bump();
        let capability = if self.lexer.at(&TokenKind::LParen) {
            Some(self.parse_type_capability_modifier()?)
        } else {
            None
        };
        Ok(ItemVisibility::Public(capability))
    }

    fn parse_type_capability_modifier(&mut self) -> Result<TypeCapability, ParseError> {
        self.expect(&TokenKind::LParen)?;
        let modifier = self.parse_ident()?;
        let capability = match modifier.as_name() {
            Some("view") => TypeCapability::View,
            Some("make") => TypeCapability::Make,
            _ => return self.err(Expected::TypeCapability),
        };
        self.expect(&TokenKind::RParen)?;
        Ok(capability)
    }

    /// Parse a module-level `let` declaration.
    ///
    /// Module declarations are implicitly corecursive, so `let rec` is rejected.
    ///
    /// TinyML currently keeps module-level `let` syntax intentionally small:
    ///
    /// ```text
    /// let name [: type] = expr
    /// ```
    ///
    /// Functions must be written explicitly as values, e.g.
    /// `let f = fn x -> x`. We do not parse ML-style shorthand
    /// `let f x = x` yet, so a formatter can preserve the user's explicit
    /// function spelling instead of normalizing between two equivalent forms.
    fn parse_let_decl(&mut self, item_visibility: ItemVisibility) -> Result<LetDecl, ParseError> {
        let visibility = match item_visibility {
            ItemVisibility::Private => Visibility::Private,
            ItemVisibility::Public(None) => Visibility::Public,
            ItemVisibility::Public(Some(_)) => {
                return Err(ParseError::type_modifier_on_let(
                    self.lexer.source().to_string(),
                    self.lexer.current_span(),
                ));
            }
        };
        let start = self.lexer.prev_span().start;
        if self.lexer.at(&TokenKind::Rec) {
            return Err(ParseError::module_let_rec(
                self.lexer.source().to_string(),
                self.lexer.current_span(),
            ));
        }
        let name = self.parse_ident()?;
        self.validate_ident(name.as_value_name())?;

        let hint = if self.lexer.eat(&TokenKind::Colon).is_some() {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        self.expect(&TokenKind::Equal)?;
        let body = self.parse_expr()?;
        let span = Span::new(start, self.lexer.prev_span().end);
        Ok(LetDecl {
            visibility,
            name,
            hint,
            body,
            span,
        })
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

    /// Parse a module-level type declaration after `type` or `pub ... type`.
    ///
    /// The caller has already consumed the keyword that selected this parser:
    /// either bare `type`, which passes `TypeVisibility::Private`, or `pub`,
    /// which first parses an optional type visibility modifier and then consumes
    /// `type`. Bare `pub type` is intentionally opaque to consumers; `pub(view)`
    /// permits viewing/pattern matching, and `pub(make)` permits viewing plus
    /// construction.
    ///
    /// Type declarations are split into two syntactic parts:
    ///
    /// ```text
    /// type-decl   ::= visibility? type-header type-def?
    /// type-header ::= TypeName type-params?
    /// type-def    ::= = abstract-or-alias-or-record-or-variant
    /// ```
    ///
    /// The header accepts a type name plus optional generic parameters:
    ///
    /// ```text
    /// type Name
    /// type Name<'a, 'b>
    /// ```
    ///
    /// If no `=` follows the header, the declaration is fully abstract:
    ///
    /// ```text
    /// type Handle
    /// type Handle<'a>
    /// ```
    ///
    /// If `=` is present, `parse_type_def` classifies the definition by its
    /// first token:
    ///
    /// - `|` starts a variant definition. Variants must use this marker, so
    ///   `type X = | User` is a nullary constructor while `type X = User` is an
    ///   alias to a type named `User`.
    /// - `{` starts a record definition, e.g.
    ///   `type User = { name: string, age: i32 }`.
    /// - anything else is parsed as a normal type expression and becomes an
    ///   alias, e.g. `type Pair = (i32, i32)` or `type X = User`.
    ///
    /// Variant constructors accept three payload shapes:
    ///
    /// ```text
    /// | None                // nullary constructor
    /// | Some('a)            // tuple/payload constructor
    /// | User { name: Str }  // record constructor
    /// ```
    ///
    /// The parser validates only syntactic/name-shape invariants here:
    /// declaration names must be type-like, fields must be field-like, and type
    /// parameters must be type variables. Semantic checks such as duplicate
    /// declarations, unknown type references, constructor uniqueness, and arity
    /// correctness are left to the checker.
    fn parse_type_decl(&mut self, visibility: TypeVisibility) -> Result<TypeDecl, ParseError> {
        let (start, name, params) = self.parse_type_decl_header()?;
        let def = if self.lexer.eat(&TokenKind::Equal).is_some() {
            self.parse_type_def()?
        } else {
            TypeDef::Abstract {
                span: Span::new(start, self.lexer.prev_span().end),
            }
        };
        Ok(TypeDecl {
            visibility,
            name,
            params,
            def,
            span: Span::new(start, self.lexer.prev_span().end),
        })
    }

    fn parse_type_decl_header(&mut self) -> Result<(usize, Ident, Vec<TypeVar>), ParseError> {
        let start = self.lexer.prev_span().start;
        let name = self.parse_ident()?;
        self.validate_ident(name.as_type_name())?;
        let params = self.parse_type_params()?;
        Ok((start, name, params))
    }

    fn parse_type_def(&mut self) -> Result<TypeDef, ParseError> {
        match self.lexer.peek() {
            TokenKind::Pipe => self.parse_type_variant_def(),
            TokenKind::LBrace => {
                let start = self.lexer.bump().span;
                let fields = self.parse_type_record_fields()?;
                Ok(TypeDef::Record {
                    fields,
                    span: start.join(self.lexer.prev_span()),
                })
            }
            _ => {
                let manifest = self.parse_type_expr()?;
                let span = manifest.span();
                Ok(TypeDef::Alias { manifest, span })
            }
        }
    }

    fn parse_type_variant_def(&mut self) -> Result<TypeDef, ParseError> {
        let variants = self.parse_variants()?;
        let span = variants
            .first()
            .map(|first| first.span().join(variants.last().expect("variant").span()))
            .unwrap_or_else(|| self.lexer.prev_span());
        Ok(TypeDef::Variant { variants, span })
    }

    /// Parse a variant constructor list.
    fn parse_variants(&mut self) -> Result<Vec<Variant>, ParseError> {
        let mut variants = Vec::new();
        while self.lexer.eat(&TokenKind::Pipe).is_some() || variants.is_empty() {
            let ident = self.parse_ident_constructor()?;
            let variant = match self.lexer.peek() {
                TokenKind::LBrace => {
                    self.lexer.bump();
                    let fields = self.parse_type_record_fields()?;
                    let span = ident.span().join(self.lexer.prev_span());
                    Variant::RecordConstructor {
                        ident,
                        fields,
                        span,
                    }
                }
                TokenKind::LParen => {
                    self.lexer.bump();
                    let args = self.parse_type_comma_exprs_until(TokenKind::RParen)?;
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

    /// Pratt parser core.
    ///
    /// `min_bp` is the minimum left binding power accepted by this recursive
    /// call. Higher binding power means the syntax groups first, as if the
    /// source had implicit parentheses around the tighter expression:
    ///
    /// ```text
    /// f x.y        => f (x.y)
    /// f x + y      => (f x) + y
    /// f (g x) y    => (f (g x)) y
    /// ```
    ///
    /// The parser starts with one expression head (`parse_expr_one`) and then
    /// repeatedly extends it with projection, application, or infix operators
    /// while those operators bind tightly enough for this call.
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_expr_one()?;

        loop {
            // Field projection is the tightest expression extension, so `f x.y`
            // parses as `f (x.y)` rather than `(f x).y`.
            if self.lexer.at(&TokenKind::Dot) {
                if EXPR_PROJECTION_L_BP < min_bp {
                    break;
                }
                self.lexer.bump();
                let (name, span) = self.parse_ident_part()?;
                let field = Ident::Name { name, span };
                self.validate_ident(field.as_value_name())?;
                let span = lhs.span().join(field.span());
                lhs = Expr::Field {
                    base: Box::new(lhs),
                    field,
                    span,
                };
                continue;
            }

            // Juxtaposition application: `f x y`. Application binds tighter
            // than infix operators, so `f x + y` parses as `(f x) + y`.
            // A bare `{` is not accepted as a juxtaposition argument because it
            // is also used as a delimiter by forms such as `match scrutinee {`;
            // write `f ({ ... })` when passing a block explicitly.
            if self.lexer.peek().starts_application_arg() {
                if EXPR_APPLICATION_L_BP < min_bp {
                    break;
                }
                let r_bp = EXPR_APPLICATION_R_BP;
                let mut args = vec![self.parse_expr_bp(r_bp)?];
                while self.lexer.peek().starts_application_arg() {
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

            // Infix operators are table-driven. The recursive RHS parse uses
            // the operator's right binding power, which controls associativity.
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

    /// Parse one expression head before Pratt-loop extensions are applied.
    ///
    /// This handles literals, variables, constructors, unit, lambdas, matches,
    /// parenthesized/tuple expressions, and block expressions.
    fn parse_expr_one(&mut self) -> Result<Expr, ParseError> {
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
                if name.as_constructor_name().is_ok() {
                    self.parse_expr_constructor(name)
                } else {
                    self.parse_expr_var(name)
                }
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
            TokenKind::Fn => self.parse_expr_fn(),
            TokenKind::Match => self.parse_expr_match(),
            TokenKind::LParen => self.parse_expr_paren(),
            TokenKind::LBrace => self.parse_expr_block(),
            _ => self.err(Expected::Expression),
        }
    }

    fn parse_expr_var(&mut self, name: Ident) -> Result<Expr, ParseError> {
        self.validate_ident(name.as_value_name())?;
        let span = name.span();
        Ok(Expr::Var { name, span })
    }

    fn parse_expr_constructor(&mut self, name: Ident) -> Result<Expr, ParseError> {
        match self.lexer.peek() {
            TokenKind::LBrace => {
                self.lexer.bump();
                let fields = self.parse_record_expr_fields()?;
                let end = self.expect(&TokenKind::RBrace)?.span;
                let span = name.span().join(end);
                Ok(Expr::Record { name, fields, span })
            }
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
                let span = args
                    .last()
                    .map(|arg| name.span().join(arg.span()))
                    .unwrap_or_else(|| name.span());
                Ok(Expr::Constructor { name, args, span })
            }
            _ => {
                let span = name.span();
                Ok(Expr::Constructor {
                    name,
                    args: Vec::new(),
                    span,
                })
            }
        }
    }

    /// Parse lambda syntax: `fn <patterns...> -> <expr>`.
    fn parse_expr_fn(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::Fn)?.span;
        let mut params = Vec::new();
        while self.lexer.peek().starts_pattern() {
            params.push(self.parse_pattern_fn_param()?);
        }
        if params.is_empty() {
            return self.err(Expected::FunctionParameter);
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

    /// Parse match syntax: `match <expr> { | <pat> -> <expr> ... }`.
    fn parse_expr_match(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::Match)?.span;
        let scrutinee = self.parse_expr_bp(MATCH_SCRUTINEE_MIN_BP)?;
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
        }
        let end = self.expect(&TokenKind::RBrace)?.span;
        if arms.is_empty() {
            return self.err(Expected::MatchArm);
        }
        Ok(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            span: start.join(end),
        })
    }

    /// Parse parenthesized expressions and tuple expressions.
    ///
    /// Unit `()` is lexed as a dedicated token and handled by `parse_expr_one`.
    /// This function parses grouped expressions and tuple expressions.
    fn parse_expr_paren(&mut self) -> Result<Expr, ParseError> {
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
    /// Empty `{}` is an empty block; the checker decides its type.
    fn parse_expr_block(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(&TokenKind::LBrace)?.span;
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
            self.validate_ident(name.as_field_name())?;
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
                if name.as_constructor_name().is_ok() {
                    self.parse_pattern_constructor(name)
                } else {
                    self.validate_ident(name.as_value_name())?;
                    let span = name.span();
                    Ok(Pattern::Var { name, span })
                }
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
            TokenKind::LParen => self.parse_pattern_paren(),
            _ => self.err(Expected::Pattern),
        }
    }

    fn parse_pattern_fn_param(&mut self) -> Result<Pattern, ParseError> {
        match self.lexer.peek() {
            TokenKind::Ident(s) if s == "_" => {
                let span = self.lexer.bump().span;
                Ok(Pattern::Wildcard { span })
            }
            TokenKind::Ident(_) => {
                let name = self.parse_ident()?;
                if name.as_constructor_name().is_ok() {
                    let span = name.span();
                    Ok(Pattern::Constructor {
                        name,
                        args: Vec::new(),
                        span,
                    })
                } else {
                    self.validate_ident(name.as_value_name())?;
                    let span = name.span();
                    Ok(Pattern::Var { name, span })
                }
            }
            TokenKind::LParen => self.parse_pattern_paren(),
            TokenKind::Unit => self.parse_pattern(),
            _ => self.err(Expected::FunctionParameter),
        }
    }

    fn parse_pattern_constructor(&mut self, name: Ident) -> Result<Pattern, ParseError> {
        match self.lexer.peek() {
            TokenKind::LBrace => {
                self.lexer.bump();
                let fields = self.parse_pattern_record_fields()?;
                let end = self.expect(&TokenKind::RBrace)?.span;
                let span = name.span().join(end);
                Ok(Pattern::Record { name, fields, span })
            }
            TokenKind::LParen => {
                self.lexer.bump();
                let args = self.parse_pattern_constructor_tuple_args()?;
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

    /// Parse parenthesized and tuple patterns.
    ///
    /// Unit `()` is a dedicated token handled in `parse_pattern`. This function
    /// parses grouped patterns and tuple patterns.
    fn parse_pattern_paren(&mut self) -> Result<Pattern, ParseError> {
        let start = self.expect(&TokenKind::LParen)?.span;
        if let TokenKind::Ident(_) = self.lexer.peek() {
            let name = self.parse_ident()?;
            if name.as_constructor_name().is_ok() {
                return self.parse_pattern_grouped_constructor(start, name);
            }
            self.validate_ident(name.as_value_name())?;
            let span = name.span();
            let first = Pattern::Var { name, span };
            return self.finish_paren_pattern(start, first);
        }
        let first = self.parse_pattern()?;
        self.finish_paren_pattern(start, first)
    }

    fn finish_paren_pattern(&mut self, start: Span, first: Pattern) -> Result<Pattern, ParseError> {
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

    fn parse_pattern_grouped_constructor(
        &mut self,
        start: Span,
        name: Ident,
    ) -> Result<Pattern, ParseError> {
        if self.lexer.at(&TokenKind::RParen) {
            let end = self.expect(&TokenKind::RParen)?.span;
            return Ok(Pattern::Constructor {
                name,
                args: Vec::new(),
                span: start.join(end),
            });
        }

        if self.lexer.at(&TokenKind::LBrace) {
            self.lexer.bump();
            let fields = self.parse_pattern_record_fields()?;
            self.expect(&TokenKind::RBrace)?;
            let end = self.expect(&TokenKind::RParen)?.span;
            return Ok(Pattern::Record {
                name,
                fields,
                span: start.join(end),
            });
        }

        if self.lexer.at(&TokenKind::LParen) {
            self.lexer.bump();
            let args = self.parse_pattern_constructor_tuple_args()?;
            self.expect(&TokenKind::RParen)?;
            let end = self.expect(&TokenKind::RParen)?.span;
            return Ok(Pattern::Constructor {
                name,
                args,
                span: start.join(end),
            });
        }

        self.err(Expected::ConstructorPatternPayload)
    }

    fn parse_pattern_constructor_tuple_args(&mut self) -> Result<Vec<Pattern>, ParseError> {
        let mut args = Vec::new();
        while !self.lexer.at(&TokenKind::RParen) {
            args.push(self.parse_pattern()?);
            if self.lexer.eat(&TokenKind::Comma).is_none() {
                break;
            }
        }
        Ok(args)
    }

    /// Parse record pattern fields.
    ///
    /// Field punning is supported: `{ name }` means `{ name: name }`. Qualified
    /// fields use explicit patterns, as in `{ module.name: pattern }`.
    fn parse_pattern_record_fields(&mut self) -> Result<Vec<(Ident, Pattern)>, ParseError> {
        let mut fields = Vec::new();
        while !self.lexer.at(&TokenKind::RBrace) {
            let name = self.parse_ident()?;
            self.validate_ident(name.as_field_name())?;
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
                    _ => return self.err(Expected::RecordFieldPattern),
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

    /// Parse a type expression used in annotations, aliases, and payloads.
    ///
    /// Named types are represented as `TypeExpr::App { name, args }`, even when
    /// `args` is empty. Type variables use apostrophe syntax, such as `'a`.
    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        match self.lexer.peek() {
            TokenKind::Ident(_) => {
                let name = self.parse_ident()?;
                self.validate_ident(name.as_type_name())?;
                match self.lexer.peek() {
                    TokenKind::LBrace => {
                        self.lexer.bump();
                        let fields = self.parse_type_record_fields()?;
                        let span = name.span().join(self.lexer.prev_span());
                        return Ok(TypeExpr::Record { name, fields, span });
                    }
                    _ => {}
                }
                let args = match self.lexer.peek() {
                    TokenKind::LAngle => {
                        self.lexer.bump();
                        let args = self.parse_type_comma_exprs_until(TokenKind::RAngle)?;
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
                let items = self.parse_type_comma_exprs_until(TokenKind::RParen)?;
                let end = self.expect(&TokenKind::RParen)?.span;
                Ok(TypeExpr::Tuple {
                    items,
                    span: start.join(end),
                })
            }
            TokenKind::LBrace => self.err(Expected::NamedRecordType),
            _ => self.err(Expected::Type),
        }
    }

    /// Parse fields inside a named record type expression or declaration.
    fn parse_type_record_fields(&mut self) -> Result<Vec<(Ident, TypeExpr)>, ParseError> {
        let mut fields = Vec::new();
        while !self.lexer.at(&TokenKind::RBrace) {
            let name = self.parse_ident()?;
            self.validate_ident(name.as_field_name())?;
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

    /// Parse comma-separated non-variant type expressions until `end` is reached.
    fn parse_type_comma_exprs_until(
        &mut self,
        end: TokenKind,
    ) -> Result<Vec<TypeExpr>, ParseError> {
        let mut tys = Vec::new();
        while !self.lexer.at(&end) {
            tys.push(self.parse_type_expr()?);
            if self.lexer.eat(&TokenKind::Comma).is_none() {
                break;
            }
        }
        Ok(tys)
    }

    /// Parse a `use` declaration after the leading `use` token.
    ///
    /// Supported forms:
    /// - `use A.B`
    /// - `use A.B.c`
    /// - `use A.B.{c, d}`
    /// - `use A.B.*`
    /// - `use A.B.{C.d, E.F.g}`
    /// - `use A.{B.{C, d}}`
    fn parse_use_decl(&mut self, visibility: ItemVisibility) -> Result<UseDecl, ParseError> {
        if !visibility.is_private() {
            return Err(ParseError::visibility_on_use(
                self.lexer.source().to_string(),
                self.lexer.current_span(),
            ));
        }
        self.parse_use_tree(false, self.lexer.prev_span().start)
    }

    fn parse_use_tree(
        &mut self,
        allow_bare_value: bool,
        start: usize,
    ) -> Result<UseDecl, ParseError> {
        let target = self.parse_ident()?;
        let start = start.min(target.span().start);
        let (module, leading_member) = target.split_qualified_leaf();

        match leading_member {
            Some(member) => {
                self.validate_ident(module.as_module_name())?;
                self.validate_use_member(&member)?;
                Ok(UseDecl::Module {
                    name: module,
                    members: vec![UseDecl::Value {
                        span: member.span(),
                        name: member,
                    }],
                    span: Span::new(start, self.lexer.prev_span().end),
                })
            }
            None if allow_bare_value && module.as_value_name().is_ok() => Ok(UseDecl::Value {
                span: module.span(),
                name: module,
            }),
            None => {
                self.validate_ident(module.as_module_name())?;
                self.parse_use_module_suffix(module, start)
            }
        }
    }

    fn parse_use_module_suffix(
        &mut self,
        name: Ident,
        start: usize,
    ) -> Result<UseDecl, ParseError> {
        let members = match self.lexer.peek() {
            TokenKind::Dot => {
                self.lexer.bump();
                match self.lexer.peek() {
                    TokenKind::Star => {
                        self.lexer.bump();
                        vec![UseDecl::Wildcard {
                            span: self.lexer.prev_span(),
                        }]
                    }
                    TokenKind::LBrace => {
                        self.lexer.bump();
                        self.parse_use_group_members()?
                    }
                    _ => return self.err(Expected::UseSuffix),
                }
            }
            _ => Vec::new(),
        };

        Ok(UseDecl::Module {
            name,
            members,
            span: Span::new(start, self.lexer.prev_span().end),
        })
    }

    fn parse_use_group_members(&mut self) -> Result<Vec<UseDecl>, ParseError> {
        let mut members = Vec::new();
        while !self.lexer.at(&TokenKind::RBrace) {
            members.push(self.parse_use_tree(true, self.lexer.current_span().start)?);
            if self.lexer.eat(&TokenKind::Comma).is_none() {
                break;
            }
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(members)
    }

    /// Parse an identifier, including module-qualified identifiers.
    ///
    /// The lexer only knows about identifier *tokens*; this parser function owns
    /// identifier *structure*. A dotted name is considered qualified only when
    /// the head is module-like (uppercase). That means `Std.Option` becomes one
    /// `Ident::Qualified`, while `value.field` remains a bare `value` followed by
    /// a field-projection token for the Pratt loop to consume. The function also
    /// stops before `.*` and `.{...}` so `parse_use_decl` can route wildcard and
    /// grouped imports.
    fn parse_ident(&mut self) -> Result<Ident, ParseError> {
        let (head, head_span) = self.parse_ident_part()?;
        match self.lexer.peek() {
            TokenKind::Dot
                if starts_uppercase(&head)
                    && !matches!(self.lexer.peek_n(1), TokenKind::Star | TokenKind::LBrace) =>
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
        let span = self.lexer.current_span();
        match self.lexer.next() {
            TokenKind::TypeVar(name) => {
                if !starts_lowercase(&name) {
                    return Err(ParseError::invalid_type_variable_name(
                        self.lexer.source().to_string(),
                        span,
                    ));
                }
                Ok(TypeVar { name, span })
            }
            _ => Err(ParseError::expected(
                self.lexer.source().to_string(),
                span,
                Expected::TypeVariable,
            )),
        }
    }

    /// Parse one identifier segment, accepting lowercase identifiers and
    /// uppercase constructors.
    fn parse_ident_part(&mut self) -> Result<(String, Span), ParseError> {
        match self.lexer.peek() {
            TokenKind::Ident(name) => {
                let name = name.to_string();
                let span = self.lexer.bump().span;
                Ok((name, span))
            }
            _ => self.err(Expected::Identifier),
        }
    }

    /// Parse a constructor name or report a parser error.
    fn parse_ident_constructor(&mut self) -> Result<Ident, ParseError> {
        let ident = self.parse_ident()?;
        self.validate_ident(ident.as_constructor_name())?;
        Ok(ident)
    }
    /// Consume `kind` or produce a diagnostic.
    fn expect(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        match self.lexer.eat(kind) {
            Some(tok) => Ok(tok),
            None => self.err(Expected::Token(kind.clone())),
        }
    }

    fn err<T>(&self, expected: Expected) -> Result<T, ParseError> {
        Err(ParseError::expected(
            self.lexer.source().to_string(),
            self.lexer.current_span(),
            expected,
        ))
    }

    fn validate_ident(&self, result: Result<&Ident, IdentNameError>) -> Result<(), ParseError> {
        result.map(|_| ()).map_err(|err| {
            ParseError::invalid_identifier(self.lexer.source().to_string(), err.span, err.message)
        })
    }

    fn validate_use_member(&self, ident: &Ident) -> Result<(), ParseError> {
        if ident.as_value_name().is_ok() || ident.as_constructor_name().is_ok() {
            Ok(())
        } else {
            self.validate_ident(ident.as_value_name())
        }
    }
}

fn starts_uppercase(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

fn starts_lowercase(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_lowercase())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ident_from(src: &str) -> (Ident, TokenKind) {
        let lexer = Lexer::new(src).expect("lex ok");
        let mut parser = Parser::new(lexer);
        let ident = parser.parse_ident().expect("parse ident ok");
        (ident, parser.lexer.peek().clone())
    }

    fn ident_path(ident: &Ident) -> Vec<&str> {
        match ident {
            Ident::Name { name, .. } => vec![name.as_str()],
            Ident::Qualified { name, rest, .. } => {
                let mut path = vec![name.as_str()];
                path.extend(ident_path(rest));
                path
            }
        }
    }

    #[test]
    fn parse_ident_accepts_bare_names() {
        let (ident, next) = parse_ident_from("Value");
        assert!(matches!(ident, Ident::Name { name, .. } if name == "Value"));
        assert_eq!(next, TokenKind::Eof);

        let (ident, next) = parse_ident_from("value");
        assert!(matches!(ident, Ident::Name { name, .. } if name == "value"));
        assert_eq!(next, TokenKind::Eof);
    }

    #[test]
    fn parse_ident_accepts_qualified_module_chains() {
        let (ident, next) = parse_ident_from("A.B.C");
        assert_eq!(ident_path(&ident), ["A", "B", "C"]);
        assert_eq!(next, TokenKind::Eof);
    }

    #[test]
    fn parse_ident_accepts_qualified_module_then_value() {
        let (ident, next) = parse_ident_from("A.B.value");
        assert_eq!(ident_path(&ident), ["A", "B", "value"]);
        assert_eq!(next, TokenKind::Eof);
    }

    #[test]
    fn parse_ident_leaves_value_field_projection_for_expression_parser() {
        let (ident, next) = parse_ident_from("value.field");
        assert!(matches!(ident, Ident::Name { name, .. } if name == "value"));
        assert_eq!(next, TokenKind::Dot);
    }

    #[test]
    fn parse_ident_stops_before_use_wildcard_and_grouped_imports() {
        let (ident, next) = parse_ident_from("A.B.*");
        assert_eq!(ident_path(&ident), ["A", "B"]);
        assert_eq!(next, TokenKind::Dot);

        let (ident, next) = parse_ident_from("A.B.{value}");
        assert_eq!(ident_path(&ident), ["A", "B"]);
        assert_eq!(next, TokenKind::Dot);
    }

    #[test]
    fn parses_documented_use_forms() {
        let src = "use A.B\n\
                   use A.B.c\n\
                   use A.B.{c, D}\n\
                   use A.B.*\n\
                   use A.B.{C.d, E.F.g}\n\
                   use A.{B.{C, d}}";
        let lexer = Lexer::new(src).expect("lex ok");
        let parsed = Parser::new(lexer).parse_module().expect("parse ok");
        assert_eq!(parsed.items.len(), 6);

        let ModuleItem::UseDecl(UseDecl::Module { name, members, .. }) = &parsed.items[0] else {
            panic!("expected module use");
        };
        assert_eq!(ident_path(name), ["A", "B"]);
        assert!(members.is_empty());

        let ModuleItem::UseDecl(UseDecl::Module { name, members, .. }) = &parsed.items[1] else {
            panic!("expected concrete member use");
        };
        assert_eq!(ident_path(name), ["A", "B"]);
        assert!(matches!(&members[0], UseDecl::Value { name, .. } if ident_path(name) == ["c"]));

        let ModuleItem::UseDecl(UseDecl::Module { name, members, .. }) = &parsed.items[2] else {
            panic!("expected grouped member use");
        };
        assert_eq!(ident_path(name), ["A", "B"]);
        assert!(matches!(&members[0], UseDecl::Value { name, .. } if ident_path(name) == ["c"]));
        assert!(
            matches!(&members[1], UseDecl::Module { name, members, .. } if ident_path(name) == ["D"] && members.is_empty())
        );

        let ModuleItem::UseDecl(UseDecl::Module { name, members, .. }) = &parsed.items[3] else {
            panic!("expected wildcard module use");
        };
        assert_eq!(ident_path(name), ["A", "B"]);
        assert!(matches!(&members[0], UseDecl::Wildcard { .. }));

        let ModuleItem::UseDecl(UseDecl::Module { name, members, .. }) = &parsed.items[4] else {
            panic!("expected grouped prefixed member use");
        };
        assert_eq!(ident_path(name), ["A", "B"]);
        assert!(
            matches!(&members[0], UseDecl::Module { name, members, .. } if ident_path(name) == ["C"] && matches!(&members[0], UseDecl::Value { name, .. } if ident_path(name) == ["d"]))
        );
        assert!(
            matches!(&members[1], UseDecl::Module { name, members, .. } if ident_path(name) == ["E", "F"] && matches!(&members[0], UseDecl::Value { name, .. } if ident_path(name) == ["g"]))
        );

        let ModuleItem::UseDecl(UseDecl::Module { name, members, .. }) = &parsed.items[5] else {
            panic!("expected nested grouped use");
        };
        assert_eq!(ident_path(name), ["A"]);
        assert!(
            matches!(&members[0], UseDecl::Module { name, members, .. } if ident_path(name) == ["B"] && matches!(&members[0], UseDecl::Module { name, members, .. } if ident_path(name) == ["C"] && members.is_empty()) && matches!(&members[1], UseDecl::Value { name, .. } if ident_path(name) == ["d"]))
        );
    }

    #[test]
    fn rejects_grouping_after_concrete_use_member() {
        let src = "use A.B.c.*";
        let lexer = Lexer::new(src).expect("lex ok");
        assert!(Parser::new(lexer).parse_module().is_err());
    }

    #[test]
    fn parses_type_declaration_definitions_and_visibility() {
        let src = "type Abstract\n\
                   type Alias = User\n\
                   type Variant = | User\n\
                   pub type Public = User\n\
                   pub(view) type View = User\n\
                   pub(make) type Make = User\n\
                   pub let exposed = 1";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        assert_eq!(module.items.len(), 7);

        let ModuleItem::TypeDecl(TypeDecl { def, .. }) = &module.items[0] else {
            panic!("expected type decl");
        };
        assert!(matches!(def, TypeDef::Abstract { .. }));

        let ModuleItem::TypeDecl(TypeDecl { def, .. }) = &module.items[1] else {
            panic!("expected type decl");
        };
        assert!(
            matches!(def, TypeDef::Alias { manifest, .. } if matches!(manifest, TypeExpr::App { name, .. } if ident_path(name) == ["User"]))
        );

        let ModuleItem::TypeDecl(TypeDecl { def, .. }) = &module.items[2] else {
            panic!("expected type decl");
        };
        assert!(matches!(def, TypeDef::Variant { variants, .. } if variants.len() == 1));

        let visibilities = module.items[3..6]
            .iter()
            .map(|item| match item {
                ModuleItem::TypeDecl(TypeDecl { visibility, .. }) => visibility,
                _ => panic!("expected type decl"),
            })
            .collect::<Vec<_>>();
        assert_eq!(
            visibilities,
            [
                &TypeVisibility::Public(TypeCapability::Name),
                &TypeVisibility::Public(TypeCapability::View),
                &TypeVisibility::Public(TypeCapability::Make)
            ]
        );

        let ModuleItem::LetDecl(LetDecl { visibility, .. }) = &module.items[6] else {
            panic!("expected public let decl");
        };
        assert_eq!(visibility, &Visibility::Public);
    }

    #[test]
    fn rejects_type_visibility_modifier_on_let() {
        let lexer = Lexer::new("pub(view) let x = 1").expect("lex ok");
        assert!(Parser::new(lexer).parse_module().is_err());
    }

    #[test]
    fn rejects_module_function_shorthand() {
        let lexer = Lexer::new("let id x = x").expect("lex ok");
        assert!(Parser::new(lexer).parse_module().is_err());
    }

    #[test]
    fn parses_let_bound_fn_expression_as_const_decl() {
        let lexer = Lexer::new("let id = fn x -> x").expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let decl");
        };
        assert!(matches!(body, Expr::Fn { params, .. } if params.len() == 1));
    }

    #[test]
    fn accepts_valid_naming_invariants() {
        let src = "use App.Core.{make_user, User}\n\
             type Maybe<'a> = | None | Some('a)\n\
             let _value = 1\n\
             let read = fn (User { name }) -> name\n";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        insta::assert_snapshot!(format!("{module:#?}"));
    }

    #[test]
    fn accepts_top_level_constant_type_hint_syntax() {
        let src = "let x: u8 = 1";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { hint, body, .. }) = &module.items[0] else {
            panic!("expected let decl");
        };
        assert!(hint.is_some());
        assert!(!matches!(body, Expr::TypeHint { .. }));
    }

    #[test]
    fn accepts_parenthesized_expression_type_hint_syntax() {
        let src = "let x = (1: u8)";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let decl");
        };
        assert!(matches!(body, Expr::TypeHint { .. }));
    }

    #[test]
    fn rejects_unparenthesized_postfix_expression_type_hint_syntax() {
        let src = "let x = 1: u8";
        let lexer = Lexer::new(src).expect("lex ok");
        assert!(Parser::new(lexer).parse_module().is_err());
    }

    #[test]
    fn accepts_record_field_projection_syntax() {
        let src = "let get_id = fn x -> x.id";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let decl");
        };
        let Expr::Fn { body, .. } = body else {
            panic!("expected function expression");
        };
        assert!(matches!(&**body, Expr::Field { field, .. } if field.as_name() == Some("id")));
    }

    #[test]
    fn uppercase_dot_expression_remains_qualified_identifier() {
        let src = "let value = Module.value";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let decl");
        };
        assert!(matches!(
            body,
            Expr::Var { name: Ident::Qualified { name, .. }, .. } if name == "Module"
        ));
    }

    #[test]
    fn bare_constructor_function_params_are_nullary() {
        let src = "let f = fn Some x -> x";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let declaration");
        };
        let Expr::Fn { params, .. } = body else {
            panic!("expected function expression");
        };
        assert_eq!(params.len(), 2);
        assert!(matches!(
            &params[0],
            Pattern::Constructor { name, args, .. }
                if matches!(name.as_name(), Some("Some")) && args.is_empty()
        ));
        assert!(matches!(
            &params[1],
            Pattern::Var { name, .. } if matches!(name.as_name(), Some("x"))
        ));
    }

    #[test]
    fn parenthesized_constructor_function_params_carry_payloads() {
        let src = "let f = fn (Some(x)) (Pair(left, right)) -> x";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let declaration");
        };
        let Expr::Fn { params, .. } = body else {
            panic!("expected function expression");
        };
        assert_eq!(params.len(), 2);
        assert!(matches!(
            &params[0],
            Pattern::Constructor { name, args, .. }
                if matches!(name.as_name(), Some("Some")) && args.len() == 1
        ));
        assert!(matches!(
            &params[1],
            Pattern::Constructor { name, args, .. }
                if matches!(name.as_name(), Some("Pair")) && args.len() == 2
        ));
    }

    #[test]
    fn constructor_patterns_use_declared_payload_parentheses() {
        let src = "let value = match option { | Some(x) -> x }";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let declaration");
        };
        let Expr::Match { arms, .. } = body else {
            panic!("expected match expression");
        };
        assert!(matches!(
            &arms[0].pattern,
            Pattern::Constructor { name, args, .. }
                if matches!(name.as_name(), Some("Some")) && args.len() == 1
        ));
    }

    #[test]
    fn match_scrutinee_accepts_application_before_body_brace() {
        let src = "let value = match f a b { | Some(x) -> x }";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let declaration");
        };
        let Expr::Match { scrutinee, .. } = body else {
            panic!("expected match expression");
        };
        assert!(matches!(&**scrutinee, Expr::Apply { args, .. } if args.len() == 2));
    }

    #[test]
    fn rejects_match_arm_commas() {
        let src = "let value = match option { | Some(x) -> x, | None -> 0 }";
        let lexer = Lexer::new(src).expect("lex ok");
        assert!(Parser::new(lexer).parse_module().is_err());
    }

    #[test]
    fn empty_braces_parse_as_empty_block() {
        let src = "let value = {}";
        let lexer = Lexer::new(src).expect("lex ok");
        let module = Parser::new(lexer).parse_module().expect("parse ok");
        let ModuleItem::LetDecl(LetDecl { body, .. }) = &module.items[0] else {
            panic!("expected let declaration");
        };
        assert!(matches!(body, Expr::Block { exprs, .. } if exprs.is_empty()));
    }

    #[test]
    fn rejects_uppercase_value_binding() {
        let src = "let Foo = 1";
        let lexer = Lexer::new(src).expect("lex ok");
        let err = Parser::new(lexer)
            .parse_module()
            .expect_err("uppercase value name is rejected");
        insta::assert_snapshot!(err.message());
    }

    #[test]
    fn rejects_lowercase_module_name() {
        let src = "use app.Core.*";
        let lexer = Lexer::new(src).expect("lex ok");
        let err = Parser::new(lexer)
            .parse_module()
            .expect_err("lowercase module name is rejected");
        insta::assert_snapshot!(err.message());
    }

    #[test]
    fn rejects_lowercase_type_name() {
        let src = "type foo = u8";
        let lexer = Lexer::new(src).expect("lex ok");
        let err = Parser::new(lexer)
            .parse_module()
            .expect_err("lowercase type name is rejected");
        insta::assert_snapshot!(err.message());
    }

    #[test]
    fn rejects_lowercase_constructor_name() {
        let src = "type Foo = | some";
        let lexer = Lexer::new(src).expect("lex ok");
        let err = Parser::new(lexer)
            .parse_module()
            .expect_err("lowercase constructor name is rejected");
        insta::assert_snapshot!(err.message());
    }

    #[test]
    fn rejects_uppercase_field_name() {
        let src = "let user = User { Name: \"Ada\" }";
        let lexer = Lexer::new(src).expect("lex ok");
        let err = Parser::new(lexer)
            .parse_module()
            .expect_err("uppercase field name is rejected");
        insta::assert_snapshot!(err.message());
    }

    #[test]
    fn rejects_uppercase_type_variable() {
        let src = "type Box<'A> = | Box('A)";
        let lexer = Lexer::new(src).expect("lex ok");
        let err = Parser::new(lexer)
            .parse_module()
            .expect_err("uppercase type var is rejected");
        insta::assert_snapshot!(err.message());
    }

    #[test]
    fn rejects_wildcard_as_top_level_value_name() {
        let src = "let _ = 1";
        let lexer = Lexer::new(src).expect("lex ok");
        let err = Parser::new(lexer)
            .parse_module()
            .expect_err("wildcard is not a value declaration name");
        insta::assert_snapshot!(err.message());
    }
}
