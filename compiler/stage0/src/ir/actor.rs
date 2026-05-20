use crate::signature::{ModuleName, RsigType};

use super::RirExpr;

#[derive(Debug, Clone)]
pub(crate) struct ActorIrProgram {
    pub(crate) module_name: ModuleName,
    pub(crate) actors: Vec<ActorIrActor>,
}

#[derive(Debug, Clone)]
pub(crate) struct ActorIrActor {
    pub(crate) id: usize,
    pub(crate) owner: String,
    pub(crate) frame: ActorFrameLayout,
    pub(crate) states: Vec<ActorFrameState>,
}

#[derive(Debug, Clone)]
pub(crate) struct ActorFrameLayout {
    pub(crate) size_bytes: usize,
    pub(crate) align: usize,
    pub(crate) slots: Vec<ActorFrameSlot>,
    pub(crate) captures: Vec<ActorFrameSlot>,
}

#[derive(Debug, Clone)]
pub(crate) struct ActorFrameSlot {
    pub(crate) name: String,
    pub(crate) type_: ActorSlotType,
    pub(crate) field_index: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ActorSlotType {
    I64,
    Bool,
    ActorId,
    Value,
}

impl ActorSlotType {
    pub(crate) fn from_rsig(type_: &RsigType) -> Option<Self> {
        match type_ {
            RsigType::I64 => Some(ActorSlotType::I64),
            RsigType::Bool => Some(ActorSlotType::Bool),
            RsigType::ActorId(_) => Some(ActorSlotType::ActorId),
            RsigType::String
            | RsigType::Tuple(_)
            | RsigType::List(_)
            | RsigType::Record(_)
            | RsigType::Variant(_)
            | RsigType::Arrow { .. } => Some(ActorSlotType::Value),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ActorFrameState {
    pub(crate) index: usize,
    pub(crate) op: ActorFrameOp,
    pub(crate) next: ActorStateNext,
}

#[derive(Debug, Clone)]
pub(crate) enum ActorStateNext {
    State(usize),
    Done,
}

#[derive(Debug, Clone)]
pub(crate) enum ActorFrameOp {
    Let { name: String, value: RirExpr },
    Receive { binder: String, body: RirExpr },
    Expr(RirExpr),
}
