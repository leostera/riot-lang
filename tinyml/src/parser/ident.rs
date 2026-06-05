//! Identifier representation and naming validation.
//!
//! The lexer accepts broad identifier tokens, while parser contexts validate
//! whether names are values, modules, constructors, fields, or types.

use crate::prelude::PRIMITIVE_TYPES;

use super::lexer::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    pub fn qualified(head: impl Into<String>, leaf: impl Into<String>) -> Self {
        let leaf = Self::Name {
            name: leaf.into(),
            span: Span::new(0, 0),
        };
        Self::Qualified {
            name: head.into(),
            rest: Box::new(leaf),
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

    pub fn is_bare(&self) -> bool {
        matches!(self, Self::Name { .. })
    }

    pub fn is_qualified(&self) -> bool {
        matches!(self, Self::Qualified { .. })
    }

    pub fn is_value(&self) -> bool {
        self.as_value_name().is_ok()
    }

    pub fn is_module_or_constructor(&self) -> bool {
        self.as_module_name().is_ok() || self.as_constructor_name().is_ok()
    }

    /// Split a use-like target into `(module_path, optional_value_leaf)`.
    ///
    /// The final segment is treated as a value leaf only when it has value-name
    /// shape. For example, `A.B` remains `(A.B, None)`, while `A.B.value`
    /// becomes `(A.B, Some(value))`. Bare identifiers are returned unchanged as
    /// the module side and have no value leaf.
    pub fn split_qualified_leaf(self) -> (Self, Option<Self>) {
        match self {
            Self::Name { .. } => (self, None),
            Self::Qualified { name, rest, span } => match *rest {
                Self::Name {
                    name: leaf_name,
                    span: leaf_span,
                } if is_value_name(&leaf_name) => {
                    let module = Self::Name {
                        name,
                        span: Span::new(span.start, leaf_span.start.saturating_sub(1)),
                    };
                    let leaf = Self::Name {
                        name: leaf_name,
                        span: leaf_span,
                    };
                    (module, Some(leaf))
                }
                rest => {
                    let (tail_module, leaf) = rest.split_qualified_leaf();
                    let module_span = if leaf.is_some() {
                        Span::new(span.start, tail_module.span().end)
                    } else {
                        span
                    };
                    let module = Self::Qualified {
                        name,
                        rest: Box::new(tail_module),
                        span: module_span,
                    };
                    (module, leaf)
                }
            },
        }
    }

    pub fn leaf_name(&self) -> &str {
        match self {
            Self::Name { name, .. } => name,
            Self::Qualified { rest, .. } => rest.leaf_name(),
        }
    }

    pub fn is_qualified_as(&self, head: &str, leaf: &str) -> bool {
        match self {
            Self::Qualified { name, rest, .. } if name == head => {
                matches!(&**rest, Self::Name { name, .. } if name == leaf)
            }
            _ => false,
        }
    }

    pub fn same(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Name { name: left, .. }, Self::Name { name: right, .. }) => left == right,
            (
                Self::Qualified {
                    name: left,
                    rest: left_rest,
                    ..
                },
                Self::Qualified {
                    name: right,
                    rest: right_rest,
                    ..
                },
            ) => left == right && left_rest.same(right_rest),
            _ => false,
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

    fn path(ident: &Ident) -> Vec<&str> {
        match ident {
            Ident::Name { name, .. } => vec![name.as_str()],
            Ident::Qualified { name, rest, .. } => {
                let mut parts = vec![name.as_str()];
                parts.extend(path(rest));
                parts
            }
        }
    }

    #[test]
    fn split_qualified_leaf_returns_module_path_and_optional_value_leaf() {
        let (module, leaf) = name("Value").split_qualified_leaf();
        assert_eq!(path(&module), ["Value"]);
        assert!(leaf.is_none());

        let (module, leaf) = qual("A", name("B")).split_qualified_leaf();
        assert_eq!(path(&module), ["A", "B"]);
        assert!(leaf.is_none());

        let (module, leaf) = qual("A", qual("B", name("C"))).split_qualified_leaf();
        assert_eq!(path(&module), ["A", "B", "C"]);
        assert!(leaf.is_none());

        let (module, leaf) = qual("A", qual("B", name("value"))).split_qualified_leaf();
        assert_eq!(path(&module), ["A", "B"]);
        assert_eq!(path(&leaf.expect("value leaf")), ["value"]);
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
    fn same_ignores_spans_but_preserves_qualified_structure() {
        let a = Ident::Name {
            name: "User".into(),
            span: Span::new(0, 4),
        };
        let b = Ident::Name {
            name: "User".into(),
            span: Span::new(10, 14),
        };
        assert!(a.same(&b));
        assert!(qual("A", name("value")).same(&qual("A", name("value"))));
        assert!(!qual("A", name("value")).same(&qual("B", name("value"))));
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
