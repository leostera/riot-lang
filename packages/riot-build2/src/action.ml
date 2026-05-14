open Std

module De = Serde.De
module Ser = Serde.Ser
module Vector = Collections.Vector

type compile_library_source_kind =
  | LibraryInterface
  | LibraryImplementation

type compile_library_source = {
  source: Path.t;
  kind: compile_library_source_kind;
  content: string option;
}

type t =
  | CompileC of {
      source: Path.t;
      outputs: Path.t list;
      ccflags: string list;
    }
  | CompileSource of {
      source: compile_library_source;
      outputs: Path.t list;
      output: Path.t;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileInterface of {
      source: compile_library_source;
      outputs: Path.t list;
      output: Path.t;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileByteImplementation of {
      source: compile_library_source;
      outputs: Path.t list;
      output: Path.t;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileNativeImplementation of {
      source: compile_library_source;
      outputs: Path.t list;
      output: Path.t;
      cmi_file: Path.t option;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileSources of {
      sources: compile_library_source list;
      outputs: Path.t list;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileLibrary of {
      sources: compile_library_source list;
      objects: Path.t list;
      outputs: Path.t list;
      output: Path.t;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CopyFile of {
      source: Path.t;
      destination: Path.t;
    }
  | WriteFile of {
      destination: Path.t;
      content: string;
    }

let outputs = fun __tmp1 ->
  match __tmp1 with
  | CompileC { outputs; _ }
  | CompileSource { outputs; _ }
  | CompileInterface { outputs; _ }
  | CompileByteImplementation { outputs; _ }
  | CompileNativeImplementation { outputs; _ }
  | CompileSources { outputs; _ }
  | CompileLibrary { outputs; _ } -> outputs
  | CopyFile { destination; _ } -> [ destination ]
  | WriteFile { destination; _ } -> [ destination ]

let export_outputs = fun __tmp1 ->
  match __tmp1 with
  | CompileLibrary { outputs; _ } ->
      List.filter
        outputs
        ~fn:(fun output ->
          match Path.extension output with
          | Some ".cmxa"
          | Some ".a" -> true
          | _ -> false)
  | CompileC _
  | CompileSource _
  | CompileInterface _
  | CompileByteImplementation _
  | CompileNativeImplementation _
  | CompileSources _
  | CopyFile _
  | WriteFile _ -> []

let requires_toolchain = fun __tmp1 ->
  match __tmp1 with
  | CompileC _
  | CompileSource _
  | CompileInterface _
  | CompileByteImplementation _
  | CompileNativeImplementation _
  | CompileSources _
  | CompileLibrary _ -> true
  | CopyFile _
  | WriteFile _ -> false

let kind = fun __tmp1 ->
  match __tmp1 with
  | CompileC _ -> "CompileC"
  | CompileSource _ -> "CompileSource"
  | CompileInterface _ -> "CompileInterface"
  | CompileByteImplementation _ -> "CompileByteImplementation"
  | CompileNativeImplementation _ -> "CompileNativeImplementation"
  | CompileSources _ -> "CompileSources"
  | CompileLibrary _ -> "CompileLibrary"
  | CopyFile _ -> "CopyFile"
  | WriteFile _ -> "WriteFile"

let resolve_source_for_hash = fun ~(package:Riot_model.Package.t) ~src_path ->
  if Path.is_absolute src_path then
    src_path
  else
    Path.join package.path src_path

let hash_file = fun ~(package:Riot_model.Package.t) path ->
  let path = resolve_source_for_hash ~package ~src_path:path in
  match Fs.File.open_read path with
  | Error _ -> Crypto.hash_string (Path.to_string path)
  | Ok file ->
      let state = Crypto.Sha256.create () in
      let reader = Fs.File.to_reader file in
      let buffer = IO.Buffer.create ~size:16_384 in
      let rec loop () =
        IO.Buffer.clear buffer;
        match IO.Reader.read reader ~into:buffer with
        | Ok 0 -> true
        | Ok _ ->
            Crypto.Sha256.write_iovec state (IO.Buffer.to_iovec buffer);
            loop ()
        | Error _ -> false
      in
      let success = loop () in
      let _ = Fs.File.close file in
      if success then
        Crypto.Sha256.finish state
      else
        Crypto.hash_string (Path.to_string path)

let write_path = fun hasher path -> Crypto.Sha256.write hasher (Path.to_string path)

let write_paths = fun hasher paths -> List.for_each paths ~fn:(write_path hasher)

let write_optional_path = fun hasher path ->
  match path with
  | Some path ->
      Crypto.Sha256.write_bool hasher true;
      write_path hasher path
  | None -> Crypto.Sha256.write_bool hasher false

let write_strings = fun hasher values -> List.for_each values ~fn:(Crypto.Sha256.write hasher)

let write_flags = fun hasher flags ->
  flags
  |> Riot_toolchain.Ocamlc.flags_to_string
  |> write_strings hasher

let write_source_content = fun hasher ~package source content ->
  match content with
  | Some content ->
      Crypto.Sha256.write hasher "generated";
      Crypto.Sha256.write hasher content
  | None ->
      Crypto.Sha256.write hasher "concrete";
      Crypto.Sha256.write_hash hasher (hash_file ~package source)

let hash = fun ~(package:Riot_model.Package.t) ~toolchain action ->
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-build2-action:v4";
  Crypto.Sha256.write hasher (Riot_model.Package_name.to_string package.name);
  Crypto.Sha256.write_hash hasher (Riot_toolchain.hash toolchain);
  (
    match action with
    | CompileC { source; outputs; ccflags } ->
        Crypto.Sha256.write hasher "CompileC";
        write_path hasher source;
        write_source_content hasher ~package source None;
        write_paths hasher outputs;
        write_strings hasher ccflags
    | CompileSource {
        source;
        outputs;
        output;
        includes;
        flags;
      } ->
        Crypto.Sha256.write hasher "CompileSource";
        (
          match source.kind with
          | LibraryInterface -> Crypto.Sha256.write hasher "interface"
          | LibraryImplementation -> Crypto.Sha256.write hasher "implementation"
        );
        write_path hasher source.source;
        write_source_content hasher ~package source.source source.content;
        write_paths hasher outputs;
        write_path hasher output;
        write_paths hasher includes;
        write_flags hasher flags
    | CompileInterface {
        source;
        outputs;
        output;
        includes;
        flags;
      } ->
        Crypto.Sha256.write hasher "CompileInterface";
        write_path hasher source.source;
        write_source_content hasher ~package source.source source.content;
        write_paths hasher outputs;
        write_path hasher output;
        write_paths hasher includes;
        write_flags hasher flags
    | CompileByteImplementation {
        source;
        outputs;
        output;
        includes;
        flags;
      } ->
        Crypto.Sha256.write hasher "CompileByteImplementation";
        write_path hasher source.source;
        write_source_content hasher ~package source.source source.content;
        write_paths hasher outputs;
        write_path hasher output;
        write_paths hasher includes;
        write_flags hasher flags
    | CompileNativeImplementation {
        source;
        outputs;
        output;
        cmi_file;
        includes;
        flags;
      } ->
        Crypto.Sha256.write hasher "CompileNativeImplementation";
        write_path hasher source.source;
        write_source_content hasher ~package source.source source.content;
        write_paths hasher outputs;
        write_path hasher output;
        write_optional_path hasher cmi_file;
        write_paths hasher includes;
        write_flags hasher flags
    | CompileSources {
        sources;
        outputs;
        includes;
        flags;
      } ->
        Crypto.Sha256.write hasher "CompileSources";
        List.for_each
          sources
          ~fn:(fun source ->
            (
              match source.kind with
              | LibraryInterface -> Crypto.Sha256.write hasher "interface"
              | LibraryImplementation -> Crypto.Sha256.write hasher "implementation"
            );
            write_path hasher source.source;
            write_source_content hasher ~package source.source source.content;
            ());
        write_paths hasher outputs;
        write_paths hasher includes;
        write_flags hasher flags
    | CompileLibrary {
        sources;
        objects;
        outputs;
        output;
        includes;
        flags;
      } ->
        Crypto.Sha256.write hasher "CompileLibrary";
        List.for_each
          sources
          ~fn:(fun source ->
            (
              match source.kind with
              | LibraryInterface -> Crypto.Sha256.write hasher "interface"
              | LibraryImplementation -> Crypto.Sha256.write hasher "implementation"
            );
            write_path hasher source.source;
            write_source_content hasher ~package source.source source.content;
            ());
        write_paths hasher objects;
        write_paths hasher outputs;
        write_path hasher output;
        write_paths hasher includes;
        write_flags hasher flags
    | CopyFile { source; destination } ->
        Crypto.Sha256.write hasher "CopyFile";
        write_path hasher source;
        write_source_content hasher ~package source None;
        write_path hasher destination
    | WriteFile { destination; content } ->
        Crypto.Sha256.write hasher "WriteFile";
        write_path hasher destination;
        Crypto.Sha256.write hasher content
  );
  Crypto.Sha256.finish hasher

let vector_to_list = fun values ->
  let rec loop index items =
    if index < 0 then
      items
    else
      loop (Int.sub index 1) (Vector.get_unchecked values ~at:index :: items)
  in
  loop (Int.sub (Vector.length values) 1) []

let de_list = fun decode -> De.map (De.list decode) vector_to_list

let ser_list = fun encode -> Ser.contramap Vector.from_list (Ser.list encode)

let path_deserialize = De.map De.string Path.v

let path_serialize = Ser.contramap Path.to_string Ser.string

let flags_deserialize = De.map (de_list De.string) Riot_toolchain.Ocamlc.flags_of_string

let flags_serialize = Ser.contramap Riot_toolchain.Ocamlc.flags_to_string (ser_list Ser.string)

let source_kind_deserialize =
  De.variant
    [
      De.Variant.unit "LibraryInterface" LibraryInterface;
      De.Variant.unit "LibraryImplementation" LibraryImplementation;
    ]

let source_kind_serialize =
  Ser.variant
    [
      Ser.Variant.unit
        "LibraryInterface"
        (fun __tmp1 ->
          match __tmp1 with
          | LibraryInterface -> true
          | LibraryImplementation -> false);
      Ser.Variant.unit
        "LibraryImplementation"
        (fun __tmp1 ->
          match __tmp1 with
          | LibraryImplementation -> true
          | LibraryInterface -> false);
    ]

type source_field =
  | Source_path
  | Source_kind
  | Source_content

type source_builder = {
  mutable source: Path.t option;
  mutable kind: compile_library_source_kind option;
  mutable content: string option option;
}

let source_fields =
  De.fields
    [
      De.field "source" Source_path;
      De.field "kind" Source_kind;
      De.field "content" Source_content;
    ]

let source_deserialize =
  De.record_mut
    ~fields:source_fields
    ~create:(fun () -> { source = None; kind = None; content = Some None })
    ~step:(fun reader builder field ->
      match field with
      | Some Source_path -> builder.source <- Some (De.read reader path_deserialize)
      | Some Source_kind -> builder.kind <- Some (De.read reader source_kind_deserialize)
      | Some Source_content -> builder.content <- Some (De.read reader (De.option De.string))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.source, builder.kind, builder.content) with
      | (Some source, Some kind, Some content) -> ({ source; kind; content }: compile_library_source)
      | _ -> De.missing_field ())

let source_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "source" path_serialize (fun (value: compile_library_source) -> value.source);
          Ser.field "kind" source_kind_serialize (fun (value: compile_library_source) -> value.kind);
          Ser.field
            "content"
            (Ser.option Ser.string)
            (fun (value: compile_library_source) -> value.content);
        ]
    )

type compile_c_payload = {
  c_source: Path.t;
  c_outputs: Path.t list;
  c_ccflags: string list;
}

type compile_library_payload = {
  library_sources: compile_library_source list;
  library_objects: Path.t list;
  library_outputs: Path.t list;
  library_output: Path.t;
  library_includes: Path.t list;
  library_flags: Riot_toolchain.Ocamlc.compiler_flag list;
}

type compile_source_payload = {
  source_source: compile_library_source;
  source_outputs: Path.t list;
  source_output: Path.t;
  source_includes: Path.t list;
  source_flags: Riot_toolchain.Ocamlc.compiler_flag list;
}

type compile_native_source_payload = {
  native_source: compile_library_source;
  native_outputs: Path.t list;
  native_output: Path.t;
  native_cmi_file: Path.t option;
  native_includes: Path.t list;
  native_flags: Riot_toolchain.Ocamlc.compiler_flag list;
}

type compile_sources_payload = {
  sources_sources: compile_library_source list;
  sources_outputs: Path.t list;
  sources_includes: Path.t list;
  sources_flags: Riot_toolchain.Ocamlc.compiler_flag list;
}

type copy_file_payload = {
  copy_source: Path.t;
  copy_destination: Path.t;
}

type write_file_payload = {
  write_destination: Path.t;
  write_content: string;
}

type compile_c_field =
  | Compile_c_source
  | Compile_c_outputs
  | Compile_c_ccflags

type compile_c_builder = {
  mutable compile_c_source: Path.t option;
  mutable compile_c_outputs: Path.t list option;
  mutable compile_c_ccflags: string list option;
}

let compile_c_fields =
  De.fields
    [
      De.field "source" Compile_c_source;
      De.field "outputs" Compile_c_outputs;
      De.field "ccflags" Compile_c_ccflags;
    ]

let compile_c_deserialize =
  De.record_mut
    ~fields:compile_c_fields
    ~create:(fun () -> {
      compile_c_source = None;
      compile_c_outputs = Some [];
      compile_c_ccflags = Some [];
    })
    ~step:(fun reader builder field ->
      match field with
      | Some Compile_c_source -> builder.compile_c_source <- Some (De.read reader path_deserialize)
      | Some Compile_c_outputs ->
          builder.compile_c_outputs <- Some (De.read reader (de_list path_deserialize))
      | Some Compile_c_ccflags ->
          builder.compile_c_ccflags <- Some (De.read reader (de_list De.string))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.compile_c_source, builder.compile_c_outputs, builder.compile_c_ccflags) with
      | (Some c_source, Some c_outputs, Some c_ccflags) ->
          ({ c_source; c_outputs; c_ccflags }: compile_c_payload)
      | _ -> De.missing_field ())

let compile_c_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "source" path_serialize (fun (value: compile_c_payload) -> value.c_source);
          Ser.field
            "outputs"
            (ser_list path_serialize)
            (fun (value: compile_c_payload) -> value.c_outputs);
          Ser.field
            "ccflags"
            (ser_list Ser.string)
            (fun (value: compile_c_payload) -> value.c_ccflags);
        ]
    )

type compile_source_field =
  | Source_source
  | Source_outputs
  | Source_output
  | Source_includes
  | Source_flags

type compile_source_builder = {
  mutable source_source: compile_library_source option;
  mutable source_outputs: Path.t list option;
  mutable source_output: Path.t option;
  mutable source_includes: Path.t list option;
  mutable source_flags: Riot_toolchain.Ocamlc.compiler_flag list option;
}

let compile_source_fields =
  De.fields
    [
      De.field "source" Source_source;
      De.field "outputs" Source_outputs;
      De.field "output" Source_output;
      De.field "includes" Source_includes;
      De.field "flags" Source_flags;
    ]

let compile_source_deserialize =
  De.record_mut
    ~fields:compile_source_fields
    ~create:(fun () ->
      {
        source_source = None;
        source_outputs = Some [];
        source_output = None;
        source_includes = Some [];
        source_flags = Some [];
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Source_source -> builder.source_source <- Some (De.read reader source_deserialize)
      | Some Source_outputs ->
          builder.source_outputs <- Some (De.read reader (de_list path_deserialize))
      | Some Source_output -> builder.source_output <- Some (De.read reader path_deserialize)
      | Some Source_includes ->
          builder.source_includes <- Some (De.read reader (de_list path_deserialize))
      | Some Source_flags -> builder.source_flags <- Some (De.read reader flags_deserialize)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.source_source,
        builder.source_outputs,
        builder.source_output,
        builder.source_includes,
        builder.source_flags
      ) with
      | (
          Some source_source,
          Some source_outputs,
          Some source_output,
          Some source_includes,
          Some source_flags
        ) ->
          ({
            source_source;
            source_outputs;
            source_output;
            source_includes;
            source_flags;
          }: compile_source_payload)
      | _ -> De.missing_field ())

let compile_source_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field
            "source"
            source_serialize
            (fun (value: compile_source_payload) -> value.source_source);
          Ser.field
            "outputs"
            (ser_list path_serialize)
            (fun (value: compile_source_payload) -> value.source_outputs);
          Ser.field
            "output"
            path_serialize
            (fun (value: compile_source_payload) -> value.source_output);
          Ser.field
            "includes"
            (ser_list path_serialize)
            (fun (value: compile_source_payload) -> value.source_includes);
          Ser.field
            "flags"
            flags_serialize
            (fun (value: compile_source_payload) -> value.source_flags);
        ]
    )

type compile_native_source_field =
  | Native_source
  | Native_outputs
  | Native_output
  | Native_cmi_file
  | Native_includes
  | Native_flags

type compile_native_source_builder = {
  mutable native_source: compile_library_source option;
  mutable native_outputs: Path.t list option;
  mutable native_output: Path.t option;
  mutable native_cmi_file: Path.t option option;
  mutable native_includes: Path.t list option;
  mutable native_flags: Riot_toolchain.Ocamlc.compiler_flag list option;
}

let compile_native_source_fields =
  De.fields
    [
      De.field "source" Native_source;
      De.field "outputs" Native_outputs;
      De.field "output" Native_output;
      De.field "cmi_file" Native_cmi_file;
      De.field "includes" Native_includes;
      De.field "flags" Native_flags;
    ]

let compile_native_source_deserialize =
  De.record_mut
    ~fields:compile_native_source_fields
    ~create:(fun () ->
      {
        native_source = None;
        native_outputs = Some [];
        native_output = None;
        native_cmi_file = Some None;
        native_includes = Some [];
        native_flags = Some [];
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Native_source -> builder.native_source <- Some (De.read reader source_deserialize)
      | Some Native_outputs ->
          builder.native_outputs <- Some (De.read reader (de_list path_deserialize))
      | Some Native_output -> builder.native_output <- Some (De.read reader path_deserialize)
      | Some Native_cmi_file ->
          builder.native_cmi_file <- Some (De.read reader (De.option path_deserialize))
      | Some Native_includes ->
          builder.native_includes <- Some (De.read reader (de_list path_deserialize))
      | Some Native_flags -> builder.native_flags <- Some (De.read reader flags_deserialize)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.native_source,
        builder.native_outputs,
        builder.native_output,
        builder.native_cmi_file,
        builder.native_includes,
        builder.native_flags
      ) with
      | (
          Some native_source,
          Some native_outputs,
          Some native_output,
          Some native_cmi_file,
          Some native_includes,
          Some native_flags
        ) ->
          ({
            native_source;
            native_outputs;
            native_output;
            native_cmi_file;
            native_includes;
            native_flags;
          }: compile_native_source_payload)
      | _ -> De.missing_field ())

let compile_native_source_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field
            "source"
            source_serialize
            (fun (value: compile_native_source_payload) -> value.native_source);
          Ser.field
            "outputs"
            (ser_list path_serialize)
            (fun (value: compile_native_source_payload) -> value.native_outputs);
          Ser.field
            "output"
            path_serialize
            (fun (value: compile_native_source_payload) -> value.native_output);
          Ser.field
            "cmi_file"
            (Ser.option path_serialize)
            (fun (value: compile_native_source_payload) -> value.native_cmi_file);
          Ser.field
            "includes"
            (ser_list path_serialize)
            (fun (value: compile_native_source_payload) -> value.native_includes);
          Ser.field
            "flags"
            flags_serialize
            (fun (value: compile_native_source_payload) -> value.native_flags);
        ]
    )

type compile_sources_field =
  | Sources_sources
  | Sources_outputs
  | Sources_includes
  | Sources_flags

type compile_sources_builder = {
  mutable sources_sources: compile_library_source list option;
  mutable sources_outputs: Path.t list option;
  mutable sources_includes: Path.t list option;
  mutable sources_flags: Riot_toolchain.Ocamlc.compiler_flag list option;
}

let compile_sources_fields =
  De.fields
    [
      De.field "sources" Sources_sources;
      De.field "outputs" Sources_outputs;
      De.field "includes" Sources_includes;
      De.field "flags" Sources_flags;
    ]

let compile_sources_deserialize =
  De.record_mut
    ~fields:compile_sources_fields
    ~create:(fun () ->
      {
        sources_sources = Some [];
        sources_outputs = Some [];
        sources_includes = Some [];
        sources_flags = Some [];
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Sources_sources ->
          builder.sources_sources <- Some (De.read reader (de_list source_deserialize))
      | Some Sources_outputs ->
          builder.sources_outputs <- Some (De.read reader (de_list path_deserialize))
      | Some Sources_includes ->
          builder.sources_includes <- Some (De.read reader (de_list path_deserialize))
      | Some Sources_flags -> builder.sources_flags <- Some (De.read reader flags_deserialize)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.sources_sources,
        builder.sources_outputs,
        builder.sources_includes,
        builder.sources_flags
      ) with
      | (Some sources_sources, Some sources_outputs, Some sources_includes, Some sources_flags) ->
          ({
            sources_sources;
            sources_outputs;
            sources_includes;
            sources_flags;
          }: compile_sources_payload)
      | _ -> De.missing_field ())

let compile_sources_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field
            "sources"
            (ser_list source_serialize)
            (fun (value: compile_sources_payload) -> value.sources_sources);
          Ser.field
            "outputs"
            (ser_list path_serialize)
            (fun (value: compile_sources_payload) -> value.sources_outputs);
          Ser.field
            "includes"
            (ser_list path_serialize)
            (fun (value: compile_sources_payload) -> value.sources_includes);
          Ser.field
            "flags"
            flags_serialize
            (fun (value: compile_sources_payload) -> value.sources_flags);
        ]
    )

type compile_library_field =
  | Library_sources
  | Library_objects
  | Library_outputs
  | Library_output
  | Library_includes
  | Library_flags

type compile_library_builder = {
  mutable library_sources: compile_library_source list option;
  mutable library_objects: Path.t list option;
  mutable library_outputs: Path.t list option;
  mutable library_output: Path.t option;
  mutable library_includes: Path.t list option;
  mutable library_flags: Riot_toolchain.Ocamlc.compiler_flag list option;
}

let compile_library_fields =
  De.fields
    [
      De.field "sources" Library_sources;
      De.field "objects" Library_objects;
      De.field "outputs" Library_outputs;
      De.field "output" Library_output;
      De.field "includes" Library_includes;
      De.field "flags" Library_flags;
    ]

let compile_library_deserialize =
  De.record_mut
    ~fields:compile_library_fields
    ~create:(fun () ->
      {
        library_sources = Some [];
        library_objects = Some [];
        library_outputs = Some [];
        library_output = None;
        library_includes = Some [];
        library_flags = Some [];
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Library_sources ->
          builder.library_sources <- Some (De.read reader (de_list source_deserialize))
      | Some Library_objects ->
          builder.library_objects <- Some (De.read reader (de_list path_deserialize))
      | Some Library_outputs ->
          builder.library_outputs <- Some (De.read reader (de_list path_deserialize))
      | Some Library_output -> builder.library_output <- Some (De.read reader path_deserialize)
      | Some Library_includes ->
          builder.library_includes <- Some (De.read reader (de_list path_deserialize))
      | Some Library_flags -> builder.library_flags <- Some (De.read reader flags_deserialize)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.library_sources,
        builder.library_objects,
        builder.library_outputs,
        builder.library_output,
        builder.library_includes,
        builder.library_flags
      ) with
      | (
          Some library_sources,
          Some library_objects,
          Some library_outputs,
          Some library_output,
          Some library_includes,
          Some library_flags
        ) ->
          ({
            library_sources;
            library_objects;
            library_outputs;
            library_output;
            library_includes;
            library_flags;
          }: compile_library_payload)
      | _ -> De.missing_field ())

let compile_library_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field
            "sources"
            (ser_list source_serialize)
            (fun (value: compile_library_payload) -> value.library_sources);
          Ser.field
            "objects"
            (ser_list path_serialize)
            (fun (value: compile_library_payload) -> value.library_objects);
          Ser.field
            "outputs"
            (ser_list path_serialize)
            (fun (value: compile_library_payload) -> value.library_outputs);
          Ser.field
            "output"
            path_serialize
            (fun (value: compile_library_payload) -> value.library_output);
          Ser.field
            "includes"
            (ser_list path_serialize)
            (fun (value: compile_library_payload) -> value.library_includes);
          Ser.field
            "flags"
            flags_serialize
            (fun (value: compile_library_payload) -> value.library_flags);
        ]
    )

type copy_file_field =
  | Copy_source
  | Copy_destination

type copy_file_builder = {
  mutable copy_source: Path.t option;
  mutable copy_destination: Path.t option;
}

let copy_file_fields =
  De.fields [ De.field "source" Copy_source; De.field "destination" Copy_destination; ]

let copy_file_deserialize =
  De.record_mut
    ~fields:copy_file_fields
    ~create:(fun () -> { copy_source = None; copy_destination = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Copy_source -> builder.copy_source <- Some (De.read reader path_deserialize)
      | Some Copy_destination -> builder.copy_destination <- Some (De.read reader path_deserialize)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.copy_source, builder.copy_destination) with
      | (Some copy_source, Some copy_destination) ->
          ({ copy_source; copy_destination }: copy_file_payload)
      | _ -> De.missing_field ())

let copy_file_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "source" path_serialize (fun (value: copy_file_payload) -> value.copy_source);
          Ser.field
            "destination"
            path_serialize
            (fun (value: copy_file_payload) -> value.copy_destination);
        ]
    )

type write_file_field =
  | Write_destination
  | Write_content

type write_file_builder = {
  mutable write_destination: Path.t option;
  mutable write_content: string option;
}

let write_file_fields =
  De.fields [ De.field "destination" Write_destination; De.field "content" Write_content; ]

let write_file_deserialize =
  De.record_mut
    ~fields:write_file_fields
    ~create:(fun () -> { write_destination = None; write_content = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Write_destination ->
          builder.write_destination <- Some (De.read reader path_deserialize)
      | Some Write_content -> builder.write_content <- Some (De.read reader De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.write_destination, builder.write_content) with
      | (Some write_destination, Some write_content) ->
          ({ write_destination; write_content }: write_file_payload)
      | _ -> De.missing_field ())

let write_file_serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field
            "destination"
            path_serialize
            (fun (value: write_file_payload) -> value.write_destination);
          Ser.field "content" Ser.string (fun (value: write_file_payload) -> value.write_content);
        ]
    )

let deserialize =
  De.variant
    [
      De.Variant.newtype
        "CompileC"
        compile_c_deserialize
        (fun payload ->
          CompileC {
            source = payload.c_source;
            outputs = payload.c_outputs;
            ccflags = payload.c_ccflags;
          });
      De.Variant.newtype
        "CompileLibrary"
        compile_library_deserialize
        (fun payload ->
          CompileLibrary {
            sources = payload.library_sources;
            objects = payload.library_objects;
            outputs = payload.library_outputs;
            output = payload.library_output;
            includes = payload.library_includes;
            flags = payload.library_flags;
          });
      De.Variant.newtype
        "CompileSource"
        compile_source_deserialize
        (fun payload ->
          CompileSource {
            source = payload.source_source;
            outputs = payload.source_outputs;
            output = payload.source_output;
            includes = payload.source_includes;
            flags = payload.source_flags;
          });
      De.Variant.newtype
        "CompileInterface"
        compile_source_deserialize
        (fun payload ->
          CompileInterface {
            source = payload.source_source;
            outputs = payload.source_outputs;
            output = payload.source_output;
            includes = payload.source_includes;
            flags = payload.source_flags;
          });
      De.Variant.newtype
        "CompileByteImplementation"
        compile_source_deserialize
        (fun payload ->
          CompileByteImplementation {
            source = payload.source_source;
            outputs = payload.source_outputs;
            output = payload.source_output;
            includes = payload.source_includes;
            flags = payload.source_flags;
          });
      De.Variant.newtype
        "CompileNativeImplementation"
        compile_native_source_deserialize
        (fun payload ->
          CompileNativeImplementation {
            source = payload.native_source;
            outputs = payload.native_outputs;
            output = payload.native_output;
            cmi_file = payload.native_cmi_file;
            includes = payload.native_includes;
            flags = payload.native_flags;
          });
      De.Variant.newtype
        "CompileSources"
        compile_sources_deserialize
        (fun payload ->
          CompileSources {
            sources = payload.sources_sources;
            outputs = payload.sources_outputs;
            includes = payload.sources_includes;
            flags = payload.sources_flags;
          });
      De.Variant.newtype
        "CopyFile"
        copy_file_deserialize
        (fun payload ->
          CopyFile { source = payload.copy_source; destination = payload.copy_destination });
      De.Variant.newtype
        "WriteFile"
        write_file_deserialize
        (fun payload ->
          WriteFile { destination = payload.write_destination; content = payload.write_content });
    ]

let serialize =
  Ser.variant
    [
      Ser.Variant.newtype
        "CompileC"
        compile_c_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CompileC { source; outputs; ccflags } ->
              Some { c_source = source; c_outputs = outputs; c_ccflags = ccflags }
          | _ -> None);
      Ser.Variant.newtype
        "CompileSource"
        compile_source_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CompileSource {
              source;
              outputs;
              output;
              includes;
              flags;
            } ->
              Some {
                source_source = source;
                source_outputs = outputs;
                source_output = output;
                source_includes = includes;
                source_flags = flags;
              }
          | _ -> None);
      Ser.Variant.newtype
        "CompileInterface"
        compile_source_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CompileInterface {
              source;
              outputs;
              output;
              includes;
              flags;
            } ->
              Some {
                source_source = source;
                source_outputs = outputs;
                source_output = output;
                source_includes = includes;
                source_flags = flags;
              }
          | _ -> None);
      Ser.Variant.newtype
        "CompileByteImplementation"
        compile_source_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CompileByteImplementation {
              source;
              outputs;
              output;
              includes;
              flags;
            } ->
              Some {
                source_source = source;
                source_outputs = outputs;
                source_output = output;
                source_includes = includes;
                source_flags = flags;
              }
          | _ -> None);
      Ser.Variant.newtype
        "CompileNativeImplementation"
        compile_native_source_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CompileNativeImplementation {
              source;
              outputs;
              output;
              cmi_file;
              includes;
              flags;
            } ->
              Some {
                native_source = source;
                native_outputs = outputs;
                native_output = output;
                native_cmi_file = cmi_file;
                native_includes = includes;
                native_flags = flags;
              }
          | _ -> None);
      Ser.Variant.newtype
        "CompileSources"
        compile_sources_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CompileSources {
              sources;
              outputs;
              includes;
              flags;
            } ->
              Some {
                sources_sources = sources;
                sources_outputs = outputs;
                sources_includes = includes;
                sources_flags = flags;
              }
          | _ -> None);
      Ser.Variant.newtype
        "CompileLibrary"
        compile_library_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CompileLibrary {
              sources;
              objects;
              outputs;
              output;
              includes;
              flags;
            } ->
              Some {
                library_sources = sources;
                library_objects = objects;
                library_outputs = outputs;
                library_output = output;
                library_includes = includes;
                library_flags = flags;
              }
          | _ -> None);
      Ser.Variant.newtype
        "CopyFile"
        copy_file_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | CopyFile { source; destination } ->
              Some { copy_source = source; copy_destination = destination }
          | _ -> None);
      Ser.Variant.newtype
        "WriteFile"
        write_file_serialize
        (fun __tmp1 ->
          match __tmp1 with
          | WriteFile { destination; content } ->
              Some { write_destination = destination; write_content = content }
          | _ -> None);
    ]
