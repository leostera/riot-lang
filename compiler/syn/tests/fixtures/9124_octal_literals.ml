(* Octal integer literals *)

let perms = 0o755

let mask = 0o777

let zero = 0o000

(* Octal in patterns *)

let check_perms p =
  match p with
  | 0o644 -> "read/write"
  | 0o755 -> "executable"
  | _ -> "custom"
