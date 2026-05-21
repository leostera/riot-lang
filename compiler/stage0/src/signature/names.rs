use std::borrow::Borrow;
use std::fmt;
use std::ops::Deref;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ModuleName(String);

impl ModuleName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for ModuleName {
    fn from(value: String) -> Self {
        ModuleName::new(value)
    }
}

impl From<&str> for ModuleName {
    fn from(value: &str) -> Self {
        ModuleName::new(value)
    }
}

impl AsRef<str> for ModuleName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Borrow<str> for ModuleName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl fmt::Debug for ModuleName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(formatter)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct AbiSymbol(String);

impl AbiSymbol {
    pub(crate) fn new(symbol: impl Into<String>) -> Self {
        Self(symbol.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for AbiSymbol {
    fn from(value: String) -> Self {
        AbiSymbol::new(value)
    }
}

impl From<&str> for AbiSymbol {
    fn from(value: &str) -> Self {
        AbiSymbol::new(value)
    }
}

impl AsRef<str> for AbiSymbol {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Deref for AbiSymbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl fmt::Display for AbiSymbol {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl fmt::Debug for AbiSymbol {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(formatter)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TypeName(String);

impl TypeName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for TypeName {
    fn from(value: String) -> Self {
        TypeName::new(value)
    }
}

impl From<&str> for TypeName {
    fn from(value: &str) -> Self {
        TypeName::new(value)
    }
}

impl AsRef<str> for TypeName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ConstructorName(String);

impl ConstructorName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for ConstructorName {
    fn from(value: String) -> Self {
        ConstructorName::new(value)
    }
}

impl From<&str> for ConstructorName {
    fn from(value: &str) -> Self {
        ConstructorName::new(value)
    }
}

impl AsRef<str> for ConstructorName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for ConstructorName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FieldName(String);

impl FieldName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for FieldName {
    fn from(value: String) -> Self {
        FieldName::new(value)
    }
}

impl From<&str> for FieldName {
    fn from(value: &str) -> Self {
        FieldName::new(value)
    }
}

impl AsRef<str> for FieldName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for FieldName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeParamName(String);

impl TypeParamName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for TypeParamName {
    fn from(value: String) -> Self {
        TypeParamName::new(value)
    }
}

impl From<&str> for TypeParamName {
    fn from(value: &str) -> Self {
        TypeParamName::new(value)
    }
}

impl AsRef<str> for TypeParamName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for TypeParamName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct TypeVarName(String);

impl TypeVarName {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for TypeVarName {
    fn from(value: String) -> Self {
        TypeVarName::new(value)
    }
}

impl From<&str> for TypeVarName {
    fn from(value: &str) -> Self {
        TypeVarName::new(value)
    }
}

impl AsRef<str> for TypeVarName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Display for TypeVarName {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}
