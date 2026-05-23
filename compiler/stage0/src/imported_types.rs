use crate::signature::{RsigType, TypeName};
use crate::stdlib::Stdlib;

pub(crate) fn imported_type_name(module_name: &str, type_name: &TypeName) -> TypeName {
    if type_name.as_str().contains('.') {
        return type_name.clone();
    }
    TypeName::new(format!("{module_name}.{}", type_name.as_str()))
}

pub(crate) fn qualify_imported_type(module_name: &str, type_: &RsigType) -> RsigType {
    match type_ {
        RsigType::ActorId(message) => {
            RsigType::ActorId(Box::new(qualify_imported_type(module_name, message)))
        }
        RsigType::Arrow { parameter, result } => RsigType::Arrow {
            parameter: Box::new(qualify_imported_type(module_name, parameter)),
            result: Box::new(qualify_imported_type(module_name, result)),
        },
        RsigType::List(element) => {
            RsigType::List(Box::new(qualify_imported_type(module_name, element)))
        }
        RsigType::Tuple(items) => RsigType::Tuple(
            items
                .iter()
                .map(|item| qualify_imported_type(module_name, item))
                .collect(),
        ),
        RsigType::Record(name) => RsigType::Record(imported_type_name(module_name, name)),
        RsigType::RecordApp { name, args } => RsigType::RecordApp {
            name: imported_type_name(module_name, name),
            args: args
                .iter()
                .map(|arg| qualify_imported_type(module_name, arg))
                .collect(),
        },
        RsigType::Variant(name) if Stdlib::new().is_prelude_type_name(name) => {
            RsigType::Variant(name.clone())
        }
        RsigType::Variant(name) => RsigType::Variant(imported_type_name(module_name, name)),
        RsigType::VariantApp { name, args } if Stdlib::new().is_prelude_type_name(name) => {
            RsigType::VariantApp {
                name: name.clone(),
                args: args
                    .iter()
                    .map(|arg| qualify_imported_type(module_name, arg))
                    .collect(),
            }
        }
        RsigType::VariantApp { name, args } => RsigType::VariantApp {
            name: imported_type_name(module_name, name),
            args: args
                .iter()
                .map(|arg| qualify_imported_type(module_name, arg))
                .collect(),
        },
        other => other.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn qualify_imported_type_preserves_dependency_qualified_names() {
        let qualified = qualify_imported_type(
            "Analyze",
            &RsigType::Arrow {
                parameter: Box::new(RsigType::Variant(TypeName::new("Syntax.token"))),
                result: Box::new(RsigType::Record(TypeName::new("Syntax.span"))),
            },
        );

        assert_eq!(
            qualified,
            RsigType::Arrow {
                parameter: Box::new(RsigType::Variant(TypeName::new("Syntax.token"))),
                result: Box::new(RsigType::Record(TypeName::new("Syntax.span"))),
            }
        );
    }
}
