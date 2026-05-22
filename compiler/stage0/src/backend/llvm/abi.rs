use crate::actor::air::ActorSlotType;
use crate::signature::RsigType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AbiType {
    Unknown,
    Unit,
    I64,
    Bool,
    ActorId,
    Value,
}

impl AbiType {
    pub(crate) fn from_actor_slot(type_: ActorSlotType) -> Self {
        match type_ {
            ActorSlotType::I64 => AbiType::I64,
            ActorSlotType::Bool => AbiType::Bool,
            ActorSlotType::ActorId => AbiType::ActorId,
            ActorSlotType::Value => AbiType::Value,
        }
    }

    pub(crate) fn from_rsig(type_: &RsigType) -> Self {
        match type_ {
            RsigType::Bool => AbiType::Bool,
            RsigType::I32 | RsigType::I64 => AbiType::I64,
            RsigType::ActorId(_) => AbiType::ActorId,
            RsigType::Unit => AbiType::Unit,
            RsigType::String
            | RsigType::Tuple(_)
            | RsigType::List(_)
            | RsigType::Record(_)
            | RsigType::RecordApp { .. }
            | RsigType::Variant(_)
            | RsigType::VariantApp { .. }
            | RsigType::Arrow { .. } => AbiType::Value,
            _ => AbiType::Unknown,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunctionAbi {
    pub(crate) params: Vec<AbiType>,
    pub(crate) result: AbiType,
}

impl FunctionAbi {
    pub(crate) fn is_supported_local(&self) -> bool {
        self.result != AbiType::Unknown
            && self
                .params
                .iter()
                .all(|param| *param != AbiType::Unknown && *param != AbiType::Unit)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ExternalAbi<'a> {
    symbol: &'a str,
}

impl<'a> ExternalAbi<'a> {
    pub(crate) fn new(symbol: &'a str) -> Self {
        Self { symbol }
    }

    pub(crate) fn param_is_boxed(&self, type_: &RsigType) -> bool {
        matches!(type_, RsigType::String) && self.symbol.starts_with("riot_rt_value_")
            || external_type_is_boxed(type_)
    }

    pub(crate) fn result_abi(&self, type_: &RsigType) -> AbiType {
        if external_type_is_boxed_with_string(type_) {
            AbiType::Value
        } else {
            AbiType::from_rsig(type_)
        }
    }
}

fn external_type_is_boxed(type_: &RsigType) -> bool {
    matches!(
        type_,
        RsigType::Arrow { .. }
            | RsigType::List(_)
            | RsigType::Record(_)
            | RsigType::RecordApp { .. }
            | RsigType::Tuple(_)
            | RsigType::Unknown
            | RsigType::Var(_)
            | RsigType::Variant(_)
            | RsigType::VariantApp { .. }
    )
}

fn external_type_is_boxed_with_string(type_: &RsigType) -> bool {
    matches!(type_, RsigType::String) || external_type_is_boxed(type_)
}

#[cfg(test)]
mod tests {
    use crate::signature::{RsigType, TypeName};

    use super::{AbiType, ExternalAbi};

    #[test]
    fn runtime_value_abi_takes_string_as_boxed_value() {
        let abi = ExternalAbi::new("riot_rt_value_string_len");

        assert!(abi.param_is_boxed(&RsigType::String));
    }

    #[test]
    fn raw_println_abi_takes_string_as_pointer_len() {
        let abi = ExternalAbi::new("riot_rt_println");

        assert!(!abi.param_is_boxed(&RsigType::String));
    }

    #[test]
    fn external_string_results_are_boxed_runtime_values() {
        let abi = ExternalAbi::new("riot_rt_value_string_concat");

        assert_eq!(abi.result_abi(&RsigType::String), AbiType::Value);
    }

    #[test]
    fn generic_applications_use_boxed_runtime_abi() {
        let record = RsigType::RecordApp {
            name: TypeName::new("box"),
            args: vec![RsigType::I64],
        };
        let variant = RsigType::VariantApp {
            name: TypeName::new("option"),
            args: vec![RsigType::String],
        };
        let abi = ExternalAbi::new("riot_rt_value_identity");

        assert_eq!(AbiType::from_rsig(&record), AbiType::Value);
        assert_eq!(AbiType::from_rsig(&variant), AbiType::Value);
        assert!(abi.param_is_boxed(&record));
        assert!(abi.param_is_boxed(&variant));
        assert_eq!(abi.result_abi(&record), AbiType::Value);
        assert_eq!(abi.result_abi(&variant), AbiType::Value);
    }
}
