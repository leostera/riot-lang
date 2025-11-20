open Std

(** CodeDB Schema - Attribute URIs for facts *)
module Codedb = struct
  (** Symbol Attributes *)
  
  (** Attribute: Symbol kind (module, value, type, interface) *)
  let kind = Poneglyph.Uri.of_string "codedb:kind"
  
  (** Attribute: Module belongs to package (value is package URI) *)
  let package = Poneglyph.Uri.of_string "codedb:package"
  
  (** Attribute: Package name as string *)
  let package_name = Poneglyph.Uri.of_string "codedb:package_name"
  
  (** File Attributes *)
  
  (** Attribute: File path (e.g., "packages/std/src/list.ml") *)
  let path = Poneglyph.Uri.of_string "codedb:path"
  
  (** Attribute: SHA256 hash of file content *)
  let sha256 = Poneglyph.Uri.of_string "codedb:sha256"
  
  (** Attribute: File size in bytes *)
  let size = Poneglyph.Uri.of_string "codedb:size"
  
  (** Attribute: Last modified timestamp *)
  let modified_at = Poneglyph.Uri.of_string "codedb:modified_at"
  
  (** Attribute: Deletion timestamp (when file was deleted) *)
  let deleted_at = Poneglyph.Uri.of_string "codedb:deleted_at"
  
  (** Relationships *)
  
  (** Attribute: Symbol is provided by a File (value is File entity URI) *)
  let provided_by = Poneglyph.Uri.of_string "codedb:provided_by"
  
  (** Analysis Attributes *)
  
  (** Attribute: Analysis is of a specific file (value is File entity URI) *)
  let analysis_of = Poneglyph.Uri.of_string "codedb:analysis_of"
  
  (** Attribute: When the analysis was performed *)
  let analyzed_at = Poneglyph.Uri.of_string "codedb:analyzed_at"
  
  (** Source URI for all CodeDB facts *)
  let source = Poneglyph.Uri.of_string "poneglyph:source:codedb"
  
  (** URI Constructors *)
  
  (** File-specific helpers *)
  module File = struct
    (** Create a file entity URI with SHA256 *)
    let uri ~path ~sha256 =
      Poneglyph.Uri.of_string ("codedb:file:" ^ path ^ "#" ^ sha256)
  end
  
  (** Create an analysis entity URI *)
  module Analysis = struct
    let uri ~sha256 =
      Poneglyph.Uri.of_string ("codedb:analysis:" ^ sha256)
  end
end

(** OCaml-specific schema attributes *)

(* TODO: create and use Poneglyph.field with common metadata like

   module Schema.OCaml = struct
     let ns = Poneglyph.Schema.ns "ocaml"

     module Module = struct
       let uri = Poneglyph.Schema.kind ns "module" <- becomes a function that returns Poneglyph.Uri.t

       let provides = Poneglyph.Schema.field 
         ~name:"provides"
         ~hint:"a symbol provided by this module"
         ~value_type:Poneglyph.Types.uri

       ...
     end
   end

   this way we end up with a series of typed values that represent the whole
   schema we're gonna be working with.

   other example:

   let simple_name = Poneglyph.field 
      ~name:"Simple Name"
      ~hint:"A direct, short, simple name of in OCaml"
      ~desc:"A simple name in OCaml is a name that is not namespaced, and may be reused across modules, values, etc"
      ~examples:[
        "t", "hello", "Hola"
      ]
      ~value_type:Poneglyph.Types.string
      <uri>

  so we can later on have a 

  let ocaml_schema = Poneglyph.schema ~ns:(Poneglyph.namespace "ocaml:v1")
  [
    simple_name;
    canonical_name;
  ]

  and then later on when we start Poneglyph in the Codedb server we can do

  let schemas = [
    ocaml_schema;
    codedb_schema;
    ...
  ] in
  Poneglyph.open <path to db> schemas

  and this schemas will then be used to filter/verify data at read/write time:
  * you can only see data that is specified in the schemas, so otherwise its filtered
  * you can only state facts that match the schema

*)
module OCaml = struct
  (** Attributes *)
  let simple_name = Poneglyph.Uri.of_string "ocaml:simple_name"
  let canonical_name = Poneglyph.Uri.of_string "ocaml:canonical_name"
  let qualified_name = Poneglyph.Uri.of_string "ocaml:qualified_name"
  let namespace = Poneglyph.Uri.of_string "ocaml:namespace"
  let is_module = Poneglyph.Uri.of_string "ocaml:is_module"
  let implementation_file = Poneglyph.Uri.of_string "ocaml:implementation_file"
  let interface_file = Poneglyph.Uri.of_string "ocaml:interface_file"
  
  (** Module-specific helpers *)
  module Module = struct
    (** Create a module entity URI from qualified name *)
    let uri qualified_name =
      Poneglyph.Uri.of_string ("ocaml:module:" ^ qualified_name)
  end

  (** Module-specific helpers *)
  module Symbol = struct
    (** Create a module entity URI from qualified name *)
    let uri name =
      Poneglyph.Uri.of_string ("ocaml:symbol:" ^ name)

    let kind = Poneglyph.Uri.of_string "ocaml:symbol:kind"
  end
end

module Tusk = struct
  (** Package-specific helpers *)
  module Package = struct
    (** Create a package entity URI from package name *)
    let uri package_name =
      Poneglyph.Uri.of_string ("tusk:package:" ^ package_name)
  end
end
