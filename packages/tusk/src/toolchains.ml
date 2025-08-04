(** Toolchain management for tusk build system *)

(* TODO: Implement toolchain management system
   
   1. Create and manage ~/.tusk/toolchains directory structure
      - Each toolchain version gets its own subdirectory
      - Store OCaml compiler, tools, and libraries
      - Handle platform-specific paths and configurations
   
   2. Create toolchain type and parse ocaml-toolchain.toml
      - Define a toolchain record type with:
        * version: string (e.g., "5.3.0")
        * channel: string (e.g., "stable", "nightly")
        * tools: list of required tools (ocamlc, ocamlopt, ocamldep, etc.)
        * env_vars: environment variable settings
      - Use Toml module to parse ocaml-toolchain.toml files
      - Support both global and project-specific toolchain configs
   
   3. Create ready_toolchains function
      - Check if requested toolchain is already installed
      - Download and install missing toolchains
      - Compile OCaml from source if needed
      - Set up proper directory structure in ~/.tusk/toolchains/<version>
      - Return paths to toolchain binaries
      - Cache toolchain information for faster lookups
   
   Additional TODOs:
   - Support multiple OCaml versions side-by-side
   - Handle toolchain switching (like rustup)
   - Provide toolchain validation and health checks
   - Support custom toolchain locations
   - Add toolchain metadata caching
   - Implement toolchain garbage collection
   - Support cross-compilation toolchains
*)

type toolchain = {
  version : string;
  channel : string;
  (* TODO: Add more fields as needed *)
}

let toolchain_base_dir = 
  Filename.concat (Sys.getenv "HOME") ".tusk/toolchains"

let get_toolchain_path version =
  Filename.concat toolchain_base_dir version

let ocamlc_path version =
  Filename.concat (get_toolchain_path version) "bin/ocamlc"

let ocamlopt_path version =
  Filename.concat (get_toolchain_path version) "bin/ocamlopt"

let ocamldep_path version =
  Filename.concat (get_toolchain_path version) "bin/ocamldep"

(* TODO: Implement these functions *)
let parse_toolchain_file _path = 
  failwith "TODO: parse_toolchain_file not implemented"

let install_toolchain _version =
  failwith "TODO: install_toolchain not implemented"

let ready_toolchains _workspace =
  failwith "TODO: ready_toolchains not implemented"

let validate_toolchain _toolchain =
  failwith "TODO: validate_toolchain not implemented"

let list_installed_toolchains () =
  failwith "TODO: list_installed_toolchains not implemented"

let switch_toolchain _version =
  failwith "TODO: switch_toolchain not implemented"