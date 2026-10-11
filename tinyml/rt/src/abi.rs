//! Runtime helper ABI symbol names used by native code generation.
//!
//! The compiler emits calls to these symbols for runtime-managed TinyML values.
//! This crate documents the contract; the actual runtime bodies can be supplied
//! by a future runtime library or test harness.

pub const BLOCK_NEW: &str = "tinyml_rt_block_new";
pub const BLOCK_GET: &str = "tinyml_rt_block_get";
pub const BLOCK_TAG: &str = "tinyml_rt_block_tag";

pub const STRING_NEW: &str = "tinyml_rt_string_new";
pub const FLOAT_NEW: &str = "tinyml_rt_float_new";

pub const MATCH_FAILURE: &str = "tinyml_rt_match_failure";

#[cfg(test)]
mod tests {
    #[test]
    fn helper_symbols_are_stable() {
        assert_eq!(super::BLOCK_NEW, "tinyml_rt_block_new");
        assert_eq!(super::BLOCK_GET, "tinyml_rt_block_get");
        assert_eq!(super::BLOCK_TAG, "tinyml_rt_block_tag");
    }
}
