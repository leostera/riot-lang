type package = {
  name : string;
  path : string;
  relative_path : string;
  dependencies : string list;
}

type workspace = {
  root : string;
  target_dir_root : string;
  packages : package list;
}

val parse_package_toml : string -> string * string list
val find_tusk_toml : string -> string option
val scan : root:string -> workspace

