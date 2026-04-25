type t =
  | Negative_size of int
  | Negative_offset of int
  | Negative_length of int
  | Invalid_count of int
  | Index_out_of_bounds of { buffer_length: int; at: int }
  | Range_out_of_bounds of { buffer_length: int; offset: int; len: int }
  | Shift_out_of_bounds of { buffer_length: int; by: int }
  | Split_out_of_bounds of { buffer_length: int; at: int }
  | Commit_out_of_bounds of { writable_bytes: int; requested: int }
  | Consume_out_of_bounds of { readable_bytes: int; requested: int }

val message: t -> string
