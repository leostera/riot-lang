//! Backend-specific compiler stages.
//!
//! Stage 0 currently retains its JavaScript backend and is gaining a C source
//! backend for the C-first bootstrap. Neither backend is the owned Riotc ARM
//! pipeline.

pub mod c;
pub mod js;
