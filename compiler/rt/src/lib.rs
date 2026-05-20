#![deny(unsafe_op_in_unsafe_fn)]

mod abi;
mod actor;
mod actor_id;
mod frame;
mod io;
mod scheduler;
#[cfg(test)]
mod tests;
mod value;

pub use actor::RtMessage;
pub use actor_id::ActorId;
pub use frame::RtFrameLayout;
pub use value::RtValue;
