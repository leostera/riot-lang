//! TypeScript/JavaScript backend.
//!
//! This backend lowers Lambda plus checker interface information into a single
//! JS-backend IR, then emits TypeScript source containing runtime values and type
//! declarations.

pub mod emit;
pub mod ir;
pub mod lower;

pub use ir::Module as Js;

/// Lower Lambda plus checker interface information into JS backend IR.
pub fn lower(interface: &crate::checker::ModuleInterface, lambda: &crate::lambda::Lambda) -> Js {
    lower::Lowerer::new(interface).lower_module(lambda)
}

/// Emit JS backend IR as TypeScript source.
pub fn emit_ts(js: &Js, sink: &mut impl std::fmt::Write) -> std::fmt::Result {
    emit::Emitter::new(js).emit_ts(sink)
}

/// Emit JS backend IR as a TypeScript string.
pub fn emit_ts_string(js: &Js) -> String {
    emit::Emitter::new(js).to_ts_string()
}

/// Emit runtime JavaScript, omitting TypeScript-only annotations and aliases.
pub fn emit_js_string(js: &Js) -> String {
    let mut output = String::new();
    emit::Emitter::new(js)
        .emit_js(&mut output)
        .expect("writing JavaScript to a String cannot fail");
    output
}
