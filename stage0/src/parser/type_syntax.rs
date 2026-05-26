use crate::ast::{AstPath, AstTypeExpr, TextSpan};
use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug, Clone)]
pub(crate) struct TypeSyntaxError {
    pub(crate) span: TextSpan,
    pub(crate) message: String,
}

impl TypeSyntaxError {
    pub(crate) fn into_parts(self) -> (TextSpan, String) {
        (self.span, self.message)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct TypeSyntaxParser;

impl TypeSyntaxParser {
    pub(crate) fn new() -> Self {
        Self
    }

    pub(crate) fn parse(&self, source: &str) -> Result<AstTypeExpr, TypeSyntaxError> {
        let tokens = Lexer::new().lex(source).map_err(|error| TypeSyntaxError {
            span: error.span,
            message: error.message,
        })?;
        let mut parser = TypeParser::new(source, tokens);
        let type_ = parser.parse_arrow_type()?;
        parser.expect(TokenKind::Eof, "expected end of type annotation")?;
        Ok(type_)
    }
}

struct TypeParser<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    cursor: usize,
}

impl<'src> TypeParser<'src> {
    fn new(source: &'src str, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            cursor: 0,
        }
    }

    fn parse_arrow_type(&mut self) -> Result<AstTypeExpr, TypeSyntaxError> {
        let parameter = self.parse_type_app()?;
        if self.match_kind(TokenKind::Arrow).is_none() {
            return Ok(parameter);
        }

        let result = self.parse_arrow_type()?;
        let span = parameter.span().join(result.span());
        Ok(AstTypeExpr::Arrow {
            parameter: Box::new(parameter),
            result: Box::new(result),
            span,
        })
    }

    fn parse_type_app(&mut self) -> Result<AstTypeExpr, TypeSyntaxError> {
        let type_ = self.parse_primary_type()?;
        if self.match_kind(TokenKind::Lt).is_none() {
            return Ok(type_);
        }

        let AstTypeExpr::Path {
            path: constructor,
            span: start,
        } = type_
        else {
            return Err(self.error_at_current("expected named type before type arguments"));
        };

        let mut args = Vec::new();
        if self.match_kind(TokenKind::Gt).is_none() {
            loop {
                args.push(self.parse_arrow_type()?);
                if self.match_kind(TokenKind::Comma).is_none() {
                    break;
                }
                if self.at(TokenKind::Gt) {
                    break;
                }
            }
            self.expect(TokenKind::Gt, "expected `>` after type arguments")?;
        }
        let end = self.previous_span();
        Ok(AstTypeExpr::Apply {
            constructor,
            args,
            span: start.join(end),
        })
    }

    fn parse_primary_type(&mut self) -> Result<AstTypeExpr, TypeSyntaxError> {
        match self.current().kind {
            TokenKind::Ident => self.parse_type_path(),
            TokenKind::LParen => self.parse_parenthesized_type(),
            _ => Err(self.error_at_current("expected type annotation")),
        }
    }

    fn parse_type_path(&mut self) -> Result<AstTypeExpr, TypeSyntaxError> {
        let head = self.expect(TokenKind::Ident, "expected type name")?;
        let name = self.text(head.span).to_owned();
        if name == "_" {
            return Ok(AstTypeExpr::Wildcard { span: head.span });
        }
        if name.starts_with('\'') {
            return Ok(AstTypeExpr::Var {
                name,
                span: head.span,
            });
        }

        let mut span = head.span;
        let mut segments = vec![name];
        while self.match_kind(TokenKind::Dot).is_some() {
            let segment = self.expect(TokenKind::Ident, "expected type path segment after `.`")?;
            segments.push(self.text(segment.span).to_owned());
            span = span.join(segment.span);
        }

        Ok(AstTypeExpr::Path {
            path: AstPath { segments },
            span,
        })
    }

    fn parse_parenthesized_type(&mut self) -> Result<AstTypeExpr, TypeSyntaxError> {
        let start = self.expect(TokenKind::LParen, "expected `(`")?;
        if let Some(end) = self.match_kind(TokenKind::RParen) {
            return Ok(AstTypeExpr::Path {
                path: AstPath {
                    segments: vec!["unit".to_owned()],
                },
                span: start.span.join(end),
            });
        }

        let first = self.parse_arrow_type()?;
        if self.match_kind(TokenKind::Comma).is_none() {
            self.expect(TokenKind::RParen, "expected `)` after type")?;
            return Ok(first);
        }

        let mut items = vec![first];
        loop {
            items.push(self.parse_arrow_type()?);
            if self.match_kind(TokenKind::Comma).is_none() {
                break;
            }
            if self.at(TokenKind::RParen) {
                break;
            }
        }
        let end = self.expect(TokenKind::RParen, "expected `)` after tuple type")?;
        Ok(AstTypeExpr::Tuple {
            items,
            span: start.span.join(end.span),
        })
    }

    fn match_kind(&mut self, kind: TokenKind) -> Option<TextSpan> {
        if self.at(kind) {
            Some(self.bump().span)
        } else {
            None
        }
    }

    fn expect(&mut self, kind: TokenKind, message: &'static str) -> Result<Token, TypeSyntaxError> {
        if self.at(kind) {
            Ok(self.bump())
        } else {
            Err(self.error_at_current(message))
        }
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current().kind == kind
    }

    fn current(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    fn bump(&mut self) -> Token {
        let token = self.tokens[self.cursor].clone();
        self.cursor += 1;
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

    fn error_at_current(&self, message: &'static str) -> TypeSyntaxError {
        TypeSyntaxError {
            span: self.current().span,
            message: message.to_owned(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TypeSyntaxParser;
    use crate::ast::{AstPath, AstTypeExpr};

    #[test]
    fn parses_curried_function_types() {
        let type_ = TypeSyntaxParser::new()
            .parse("(i64 -> i64) -> i64 -> i64")
            .expect("type parses");

        let AstTypeExpr::Arrow {
            parameter, result, ..
        } = type_
        else {
            panic!("expected outer arrow");
        };
        assert!(matches!(*parameter, AstTypeExpr::Arrow { .. }));
        assert!(matches!(*result, AstTypeExpr::Arrow { .. }));
    }

    #[test]
    fn parses_type_applications() {
        let type_ = TypeSyntaxParser::new()
            .parse("List<String>")
            .expect("type parses");

        let AstTypeExpr::Apply {
            constructor, args, ..
        } = type_
        else {
            panic!("expected type application");
        };
        assert_eq!(constructor.segments, ["List"]);
        assert_eq!(args.len(), 1);
        assert!(matches!(
            &args[0],
            AstTypeExpr::Path {
                path: AstPath { segments },
                ..
            } if segments == &["String"]
        ));
    }

    #[test]
    fn parses_actor_id_type_applications() {
        let type_ = TypeSyntaxParser::new()
            .parse("actor_id<'msg>")
            .expect("type parses");

        let AstTypeExpr::Apply {
            constructor, args, ..
        } = type_
        else {
            panic!("expected type application");
        };
        assert_eq!(constructor.segments, ["actor_id"]);
        assert!(matches!(&args[0], AstTypeExpr::Var { name, .. } if name == "'msg"));
    }

    #[test]
    fn parses_qualified_type_names() {
        let type_ = TypeSyntaxParser::new()
            .parse("Palette.color")
            .expect("type parses");

        let AstTypeExpr::Path { path, .. } = type_ else {
            panic!("expected type path");
        };
        assert_eq!(path.segments, ["Palette", "color"]);
    }

    #[test]
    fn parses_tuple_types() {
        let type_ = TypeSyntaxParser::new()
            .parse("(String, i64)")
            .expect("type parses");

        let AstTypeExpr::Tuple { items, .. } = type_ else {
            panic!("expected tuple type");
        };
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn parses_unit_syntax_as_type_constructor() {
        let type_ = TypeSyntaxParser::new().parse("()").expect("type parses");

        let AstTypeExpr::Path { path, .. } = type_ else {
            panic!("expected unit type constructor path");
        };
        assert_eq!(path.segments, ["unit"]);
    }

    #[test]
    fn rejects_postfix_list_spelling() {
        let error = TypeSyntaxParser::new()
            .parse("i64 list")
            .expect_err("postfix list spelling is not valid Riot type syntax");

        assert_eq!(error.message, "expected end of type annotation");
    }
}
