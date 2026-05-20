use crate::ir::ActorSlotType;
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
            RsigType::I64 => AbiType::I64,
            RsigType::ActorId(_) => AbiType::ActorId,
            RsigType::Unit => AbiType::Unit,
            RsigType::String
            | RsigType::Tuple(_)
            | RsigType::List(_)
            | RsigType::Record(_)
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
