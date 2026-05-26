use std::collections::BTreeSet;

use crate::ast::AstTypeExpr;
use crate::parser::TypeSyntaxParser;
use crate::signature::{RsigType, TypeName};
use crate::type_lowerer::RsigTypeLowerer;

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
        parse_type_syntax(text)
            .as_ref()
            .map(|type_| RsigTypeLowerer::new().lower_signature(type_, variants))
            .unwrap_or_else(|| (Vec::new(), RsigType::Unknown))
    }
}

fn parse_type_syntax(text: &str) -> Option<AstTypeExpr> {
    match TypeSyntaxParser::new().parse(text) {
        Ok(type_) => Some(type_),
        Err(error) => {
            let _ = error.into_parts();
            None
        }
    }
}
