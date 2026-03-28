let should_include =
  String.ends_with ~suffix:".hpp" entry_name
  || entry_name = "Cargo.toml"
  || entry_name = "Cargo.lock"
  || entry_name = "build.rs"
