(** FFI bindings for host triplet detection *)
external get_arch: unit -> string = "kernel_host_arch"

external get_vendor: unit -> string = "kernel_host_vendor"

external get_os: unit -> string = "kernel_host_os"

external get_abi: unit -> string = "kernel_host_abi"
