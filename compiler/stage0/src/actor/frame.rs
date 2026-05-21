use std::collections::BTreeMap;

use crate::lambda::ir::RirPattern;

use super::air::ActorSlotType;

pub(crate) fn bind_pattern_actor_slot_types(
    pattern: &RirPattern,
    type_: Option<ActorSlotType>,
    locals: &mut BTreeMap<String, Option<ActorSlotType>>,
) {
    match pattern {
        RirPattern::Bind {
            binding,
            type_: binding_type,
        } => {
            locals.insert(
                binding.as_str().to_owned(),
                ActorSlotType::from_rsig(binding_type).or(type_),
            );
        }
        RirPattern::Constructor { payload, .. } | RirPattern::Tuple(payload) => {
            for pattern in payload {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
        }
        RirPattern::List { prefix, tail } => {
            for pattern in prefix {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
            if let Some(tail) = tail {
                bind_pattern_actor_slot_types(tail, Some(ActorSlotType::Value), locals);
            }
        }
        RirPattern::Record { fields, .. } => {
            for (_, pattern) in fields {
                bind_pattern_actor_slot_types(pattern, Some(ActorSlotType::Value), locals);
            }
        }
        RirPattern::Wildcard
        | RirPattern::Unit
        | RirPattern::Bool(_)
        | RirPattern::Int(_)
        | RirPattern::String(_) => {}
    }
}
