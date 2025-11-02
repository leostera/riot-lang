open Kernel

type error = Kernel.IO.error

(** Convert error to human-readable message *)
let error_message err = Kernel.IO.error_message err

(** Convert kernel result (currently a no-op since types match) *)
let convert_kernel_result result = result
