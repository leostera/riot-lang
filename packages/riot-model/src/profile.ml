open Std

(** Override behavior - either inherit from base or override with new value *)
type 'a override =
  | Inherit
  (** Keep the value from the base profile *)
  | Override of 'a

(** Replace with this value *)
(** Profile override - partially specified profile fields *)
type profile_override = {
  kind: Ocaml_compiler.compilation_kind override;
  inline: int option override;
  no_assert: bool override;
  compact: bool override;
  unsafe: bool override;
  no_alias_deps: bool override;
  open_modules: string list override;
  warnings: Ocaml_compiler.warning list override;
  errors: Ocaml_compiler.warning list override;
  cc_flags: string list override;
  ld_flags: string list override;
  ocamlc_flags: string list override;
}

(** Build profile configuration with individual flag fields *)
type t = {
  name: string;  (** Profile name: "debug", "release", etc. *)
  kind: Ocaml_compiler.compilation_kind;
  (* Optimization flags *)
  inline: int option;  (** -inline N: inlining threshold *)
  no_assert: bool;  (** -noassert: remove assertions *)
  compact: bool;  (** -compact: optimize for code size *)
  unsafe: bool;  (** -unsafe: disable bounds checking *)
  (* Module handling *)
  no_alias_deps: bool;  (** -no-alias-deps: no deps for module aliases *)
  open_modules: string list;  (** -open Module: auto-open modules *)
  (* Warnings configuration *)
  warnings: Ocaml_compiler.warning list;  (** Warnings to enable *)
  errors: Ocaml_compiler.warning list;  (** Warnings to treat as errors *)
  (* Additional flags *)
  cc_flags: string list;  (** C compiler flags (passed with -ccopt) *)
  ld_flags: string list;  (** Linker flags (passed with -cclib) *)
  ocamlc_flags: string list;  (** Additional raw ocamlc/ocamlopt flags *)
}

(** Default debug profile - native code with debug symbols and minimal optimization *)
let debug = {
  name = "debug";
  kind = Ocaml_compiler.Native;
  inline = Some 0;
  no_assert = false;
  compact = false;
  unsafe = false;
  no_alias_deps = false;
  open_modules = [];
  warnings = [];
  errors = [
    Ocaml_compiler.PartialMatch;
    Ocaml_compiler.UnusedVariable;
    Ocaml_compiler.UnusedOpen;
    Ocaml_compiler.UnusedMatch;
  ];
  cc_flags = [];
  ld_flags = [];
  ocamlc_flags = [ "-g" ];
}

(** Default release profile - optimized, strict *)
let release = {
  name = "release";
  kind = Ocaml_compiler.Native;
  inline = Some 100;
  no_assert = true;
  compact = true;
  unsafe = false;
  no_alias_deps = false;
  open_modules = [];
  warnings = [];
  errors = [ Ocaml_compiler.All ];
  cc_flags = [];
  ld_flags = [];
  ocamlc_flags = [];
}

(** Merge two profiles - override takes precedence per-field *)
let merge = fun base override ->
  {
    name = override.name;
    kind = override.kind;
    inline =
      (
        match override.inline with
        | None -> base.inline
        | some -> some
      );
    no_assert = override.no_assert;
    compact = override.compact;
    unsafe = override.unsafe;
    no_alias_deps = override.no_alias_deps;
    open_modules = override.open_modules;
    warnings = override.warnings;
    errors = override.errors;
    cc_flags = base.cc_flags @ override.cc_flags;
    ld_flags = base.ld_flags @ override.ld_flags;
    ocamlc_flags = base.ocamlc_flags @ override.ocamlc_flags;
  }

(** Apply a profile override to a base profile *)
let apply_override = fun base (override: profile_override) ->
  {
    name = base.name;
    kind =
      (
        match override.kind with
        | Inherit -> base.kind
        | Override k -> k
      );
    inline =
      (
        match override.inline with
        | Inherit -> base.inline
        | Override opt -> opt
      );
    no_assert =
      (
        match override.no_assert with
        | Inherit -> base.no_assert
        | Override b -> b
      );
    compact =
      (
        match override.compact with
        | Inherit -> base.compact
        | Override b -> b
      );
    unsafe =
      (
        match override.unsafe with
        | Inherit -> base.unsafe
        | Override b -> b
      );
    no_alias_deps =
      (
        match override.no_alias_deps with
        | Inherit -> base.no_alias_deps
        | Override b -> b
      );
    open_modules =
      (
        match override.open_modules with
        | Inherit -> base.open_modules
        | Override l -> l
      );
    warnings =
      (
        match override.warnings with
        | Inherit -> base.warnings
        | Override l -> l
      );
    errors =
      (
        match override.errors with
        | Inherit -> base.errors
        | Override l -> l
      );
    cc_flags =
      (
        match override.cc_flags with
        | Inherit -> base.cc_flags
        | Override l -> base.cc_flags @ l
      );
    ld_flags =
      (
        match override.ld_flags with
        | Inherit -> base.ld_flags
        | Override l -> base.ld_flags @ l
      );
    ocamlc_flags =
      (
        match override.ocamlc_flags with
        | Inherit -> base.ocamlc_flags
        | Override l -> base.ocamlc_flags @ l
      );
  }

(** Apply overrides from a list by looking up the profile's name *)
let apply_overrides = fun base overrides ->
  Log.debug ("[PROFILE] apply_overrides: looking for profile '" ^ base.name ^ "' in overrides");
  Log.debug ("[PROFILE] available overrides: " ^ String.concat ", " (List.map fst overrides));
  match List.assoc_opt base.name overrides with
  | None ->
      Log.debug ("[PROFILE] No override found for '" ^ base.name ^ "'");
      base
  | Some override ->
      Log.debug ("[PROFILE] Found override for '" ^ base.name ^ "', applying");
      apply_override base override

(** Parse profile_override from TOML table *)
let override_from_toml: (string * Std.Data.Toml.value) list -> profile_override = fun table_items ->
  let open Std.Data.Toml in
    let get_string_list key =
      match List.assoc_opt key table_items with
      | Some (Array arr) ->
          Override (
            List.filter_map
              (
                function
                | String s -> Some s
                | _ -> None
              )
              arr
          )
      | _ -> Inherit
    in
    let get_int_opt key =
      match List.assoc_opt key table_items with
      | Some (String s) -> (
          try
            let i = int_of_string s in
            Override (Some i)
          with
          | _ -> Inherit
        )
      | _ -> Inherit
    in
    let get_bool key =
      match List.assoc_opt key table_items with
      | Some (Bool b) -> Override b
      | _ -> Inherit
    in
    let get_compilation_kind () =
      match List.assoc_opt "kind" table_items with
      | Some (String "bytecode") -> Override Ocaml_compiler.Bytecode
      | Some (String "native") -> Override Ocaml_compiler.Native
      | _ -> Inherit
    in
    {
      kind = get_compilation_kind ();
      inline = get_int_opt "inline";
      no_assert = get_bool "no_assert";
      compact = get_bool "compact";
      unsafe = get_bool "unsafe";
      no_alias_deps = get_bool "no_alias_deps";
      open_modules = get_string_list "open_modules";
      warnings = Inherit;
      errors = Inherit;
      cc_flags = get_string_list "cc_flags";
      ld_flags = get_string_list "ld_flags";
      ocamlc_flags = get_string_list "ocamlc_flags";
    }

(** Parse profile from TOML table *)
let from_toml: (string * Std.Data.Toml.value) list -> base:t -> t = fun table_items ~base ->
  let open Std.Data.Toml in
    let get_string_list key =
      match List.assoc_opt key table_items with
      | Some (Array arr) ->
          List.filter_map
            (
              function
              | String s -> Some s
              | _ -> None
            )
            arr
      | _ -> base.open_modules
    in
    let get_int_opt key =
      match List.assoc_opt key table_items with
      | Some (String s) -> (
          try Some (int_of_string s) with
          | _ -> base.inline
        )
      | _ -> base.inline
    in
    let get_bool key default =
      match List.assoc_opt key table_items with
      | Some (Bool b) -> b
      | _ -> default
    in
    let get_compilation_kind () =
      match List.assoc_opt "kind" table_items with
      | Some (String "bytecode") -> Ocaml_compiler.Bytecode
      | Some (String "native") -> Ocaml_compiler.Native
      | _ -> base.kind
    in
    {
      name = base.name;
      kind = get_compilation_kind ();
      inline = get_int_opt "inline";
      no_assert = get_bool "no_assert" base.no_assert;
      compact = get_bool "compact" base.compact;
      unsafe = get_bool "unsafe" base.unsafe;
      no_alias_deps = get_bool "no_alias_deps" base.no_alias_deps;
      open_modules = get_string_list "open_modules";
      warnings = base.warnings;
      errors = base.errors;
      cc_flags =
        (
          match List.assoc_opt "cc_flags" table_items with
          | Some (Array arr) ->
              List.filter_map
                (
                  function
                  | String s -> Some s
                  | _ -> None
                )
                arr
          | _ -> base.cc_flags
        );
      ld_flags =
        (
          match List.assoc_opt "ld_flags" table_items with
          | Some (Array arr) ->
              List.filter_map
                (
                  function
                  | String s -> Some s
                  | _ -> None
                )
                arr
          | _ -> base.ld_flags
        );
      ocamlc_flags =
        (
          match List.assoc_opt "ocamlc_flags" table_items with
          | Some (Array arr) ->
              List.filter_map
                (
                  function
                  | String s -> Some s
                  | _ -> None
                )
                arr
          | _ -> base.ocamlc_flags
        );
    }

(** Convert profile to OCaml compiler flags *)
let to_compiler_flags = fun profile ->
  let flags = [] in
  let flags =
    match profile.inline with
    | Some n -> Ocaml_compiler.Inline n :: flags
    | None -> flags
  in
  let flags =
    if profile.no_assert then
      Ocaml_compiler.NoAssert :: flags
    else
      flags
  in
  let flags =
    if profile.compact then
      Ocaml_compiler.Compact :: flags
    else
      flags
  in
  let flags =
    if profile.unsafe then
      Ocaml_compiler.Unsafe :: flags
    else
      flags
  in
  let flags =
    if profile.no_alias_deps then
      Ocaml_compiler.NoAliasDeps :: flags
    else
      flags
  in
  let flags =
    List.fold_left (fun acc m -> Ocaml_compiler.Open m :: acc) flags profile.open_modules
  in
  let flags =
    if List.is_empty profile.warnings then
      flags
    else
      Ocaml_compiler.Warning profile.warnings :: flags
  in
  let flags =
    if List.is_empty profile.errors then
      flags
    else
      Ocaml_compiler.WarnError profile.errors :: flags
  in
  let flags =
    List.rev_append (List.map (fun flag -> Ocaml_compiler.Raw flag) profile.ocamlc_flags) flags
  in
  Ocaml_compiler.flags_to_string (List.rev flags)

(** Hash profile into a Sha256 hasher state *)
let hash = fun state profile ->
  let module H = Crypto.Sha256 in
  H.write_string state profile.name;
  H.write_string state
    (
      match profile.kind with
      | Ocaml_compiler.Bytecode -> "bytecode"
      | Native -> "native"
    );
  (
    match profile.inline with
    | Some n -> H.write_string state (Int.to_string n)
    | None -> H.write_string state "none"
  );
  H.write_string state (Bool.to_string profile.no_assert);
  H.write_string state (Bool.to_string profile.compact);
  H.write_string state (Bool.to_string profile.unsafe);
  H.write_string state (Bool.to_string profile.no_alias_deps);
  List.iter (H.write_string state) profile.open_modules;
  List.iter
    (fun w ->
      H.write_string state (Ocaml_compiler.warning_to_string w))
    profile.warnings;
  List.iter
    (fun w ->
      H.write_string state (Ocaml_compiler.warning_to_string w))
    profile.errors;
  List.iter (H.write_string state) profile.cc_flags;
  List.iter (H.write_string state) profile.ld_flags;
  List.iter (H.write_string state) profile.ocamlc_flags

(** Convert profile to JSON *)
let to_json = fun profile ->
  let open Data.Json in
    obj
      [
        ("name", string profile.name);
        (
          "kind",
          string
            (
              match profile.kind with
              | Ocaml_compiler.Bytecode -> "bytecode"
              | Native -> "native"
            )
        );
        (
          "inline",
          match profile.inline with
          | Some n -> int n
          | None -> Null
        );
        ("no_assert", bool profile.no_assert);
        ("compact", bool profile.compact);
        ("unsafe", bool profile.unsafe);
        ("no_alias_deps", bool profile.no_alias_deps);
        ("open_modules", array (List.map string profile.open_modules));
        ("cc_flags", array (List.map string profile.cc_flags));
        ("ld_flags", array (List.map string profile.ld_flags));
        ("ocamlc_flags", array (List.map string profile.ocamlc_flags));
      ]
