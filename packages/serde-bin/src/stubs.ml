external write_uint32_le: bytes -> int -> int -> unit = "serde_bin_write_u32_le" [@@noalloc]

external write_int32_le: bytes -> int -> int32 -> unit = "serde_bin_write_i32_le" [@@noalloc]

external write_int64_le: bytes -> int -> int64 -> unit = "serde_bin_write_i64_le" [@@noalloc]

external read_uint32_le_from_string: string -> int -> int = "serde_bin_read_u32_le_string"

external read_uint32_le_from_bytes: bytes -> int -> int = "serde_bin_read_u32_le_bytes"

external read_int32_le_from_string: string -> int -> int32 = "serde_bin_read_i32_le_string"

external read_int32_le_from_bytes: bytes -> int -> int32 = "serde_bin_read_i32_le_bytes"

external read_int64_le_from_string: string -> int -> int64 = "serde_bin_read_i64_le_string"

external read_int64_le_from_bytes: bytes -> int -> int64 = "serde_bin_read_i64_le_bytes"
