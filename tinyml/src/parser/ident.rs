use crate::prelude::PRIMITIVE_TYPES;

use super::lexer::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Ident {
    Name {
        name: String,
        span: Span,
    },
    Qualified {
        name: String,
        rest: Box<Ident>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdentNameError {
    pub message: &'static str,
    pub span: Span,
}

impl Ident {
    pub fn from_string(name: impl Into<String>) -> Self {
        Self::Name {
            name: name.into(),
            span: Span::new(0, 0),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Name { span, .. } | Self::Qualified { span, .. } => *span,
        }
    }

    pub fn as_name(&self) -> Option<&str> {
        match self {
            Self::Name { name, .. } => Some(name),
            Self::Qualified { .. } => None,
        }
    }

    pub fn as_value_name(&self) -> Result<&Self, IdentNameError> {
        self.validate_qualified(
            is_value_name,
            "expected value name starting with lowercase or _",
        )
    }

    pub fn as_module_name(&self) -> Result<&Self, IdentNameError> {
        self.validate_qualified(
            is_upper_name,
            "expected module name starting with uppercase",
        )
    }

    pub fn as_type_name(&self) -> Result<&Self, IdentNameError> {
        self.validate_qualified(
            is_type_name,
            "expected type name starting with uppercase or primitive type",
        )
    }

    pub fn as_constructor_name(&self) -> Result<&Self, IdentNameError> {
        self.validate_qualified(
            is_upper_name,
            "expected constructor name starting with uppercase",
        )
    }

    pub fn as_field_name(&self) -> Result<&Self, IdentNameError> {
        self.validate_qualified(
            is_value_name,
            "expected field name starting with lowercase or _",
        )
    }

    fn validate_qualified(
        &self,
        final_predicate: fn(&str) -> bool,
        final_message: &'static str,
    ) -> Result<&Self, IdentNameError> {
        match self {
            Self::Name { name, span } => {
                if final_predicate(name) {
                    Ok(self)
                } else {
                    Err(IdentNameError {
                        message: final_message,
                        span: *span,
                    })
                }
            }
            Self::Qualified { name, rest, span } => {
                if !is_upper_name(name) {
                    return Err(IdentNameError {
                        message: "expected module name starting with uppercase",
                        span: Span::new(span.start, span.start + name.len()),
                    });
                }
                rest.validate_qualified(final_predicate, final_message)?;
                Ok(self)
            }
        }
    }
}

fn is_value_name(name: &str) -> bool {
    name != "_"
        && name
            .chars()
            .next()
            .is_some_and(|ch| ch == '_' || ch.is_ascii_lowercase())
}

fn is_upper_name(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

fn is_type_name(name: &str) -> bool {
    is_upper_name(name) || PRIMITIVE_TYPES.contains(&name)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn name(name: &str) -> Ident {
        Ident::Name {
            name: name.into(),
            span: Span::new(0, name.len()),
        }
    }

    fn qual(head: &str, rest: Ident) -> Ident {
        Ident::Qualified {
            name: head.into(),
            rest: Box::new(rest),
            span: Span::new(0, 10),
        }
    }

    #[test]
    fn value_names_start_lowercase_or_underscore() {
        assert!(name("value").as_value_name().is_ok());
        assert!(name("_value").as_value_name().is_ok());
        assert!(name("Value").as_value_name().is_err());
        assert!(name("_").as_value_name().is_err());
    }

    #[test]
    fn module_type_and_constructor_names_start_uppercase() {
        assert!(name("Module").as_module_name().is_ok());
        assert!(name("Type").as_type_name().is_ok());
        assert!(name("Constructor").as_constructor_name().is_ok());
        assert!(name("module").as_module_name().is_err());
        assert!(name("constructor").as_constructor_name().is_err());
    }

    #[test]
    fn primitive_type_names_are_valid_type_names() {
        for primitive in PRIMITIVE_TYPES {
            assert!(name(primitive).as_type_name().is_ok());
        }
        assert!(name("custom").as_type_name().is_err());
    }

    #[test]
    fn qualified_prefixes_are_module_names() {
        assert!(qual("Module", name("value")).as_value_name().is_ok());
        assert!(
            qual("Module", qual("Nested", name("Type")))
                .as_type_name()
                .is_ok()
        );
        assert!(qual("module", name("value")).as_value_name().is_err());
        assert!(qual("Module", name("value")).as_constructor_name().is_err());
    }
}
