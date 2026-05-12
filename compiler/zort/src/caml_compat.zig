// Explicit namespace for all OCaml-shaped interop shim code.
// The semantic kernel should never need to import these modules directly.
pub const Codec = @import("caml_compat/codec.zig");
pub const Api = @import("caml_compat/api.zig");
pub const Runtime = @import("caml_compat/runtime.zig");
