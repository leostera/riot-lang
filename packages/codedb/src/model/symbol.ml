open Std

type reference = 
  | Module of Module_name.t
  | Value of Value_name.t
  | Type of Type_name.t
  | Interface of Module_name.t

type kind = 
  | Module
  | Value
  | Type
  | Interface

type files = {
  implementation : File.t option;
  interface : File.t option;
}

type t = {
  kind : kind;
  name : Module_name.t;
  package : Package_info.t;
  files : files;
}

let reference_name (ref : reference) =
  match ref with
  | Module n -> Module_name.to_string n
  | Value n -> Value_name.to_string n
  | Type n -> Type_name.to_string n
  | Interface n -> Module_name.to_string n

let reference_kind (ref : reference) = 
  match ref with
  | Module _ -> Module
  | Value _ -> Value
  | Type _ -> Type
  | Interface _ -> Interface

let kind_to_string = function
  | Module -> "Module"
  | Value -> "Value"
  | Type -> "Type"
  | Interface -> "Interface"

let kind_from_string str = 
  match String.lowercase_ascii str with
  | "module" -> Some Module
  | "value" -> Some Value
  | "type" -> Some Type
  | "interface" -> Some Interface
  | _ -> None

let make ~kind ~name ~package ~files =
  { kind; name; package; files }

(** Convert symbol kind to lowercase string for facts *)
let kind_to_fact_string = function
  | Module -> "module"
  | Value -> "value"
  | Type -> "type"
  | Interface -> "interface"

(** Generate entity URI for this symbol 
    Format: codedb:<kind>:<package>/<name>
    Examples: 
      codedb:module:std/List
      codedb:value:std/List/map
      codedb:type:std/List/t
*)
let entity_uri t =
  let kind_str = kind_to_fact_string t.kind in
  let pkg = Package_name.to_string t.package.name in
  let name = Module_name.simple_name t.name in
  Poneglyph.Uri.of_string (String.concat "" ["codedb:"; kind_str; ":"; pkg; "/"; name])

(** Convert symbol to Poneglyph facts 
    Creates 4 symbol facts + file facts + relationship fact:
    
    Symbol facts (4):
    - codedb:attr:kind: "module" | "value" | "type" | "interface"
    - codedb:attr:name: simple name (e.g., "List")
    - codedb:attr:qualified_name: compilation unit name (e.g., "Std__List")
    - codedb:attr:package: package name (e.g., "std")
    
    Relationship fact (1):
    - codedb:attr:provided_by: File entity URI
    
    File facts (2-4 from File.to_facts):
    - codedb:attr:path
    - codedb:attr:sha256
    - codedb:attr:size (optional)
    - codedb:attr:modified_at (optional)
    
    @param tx_id Transaction ID to group all facts together
    @param stated_at Optional timestamp for when facts are stated (defaults to now)
*)
let to_facts ~tx_id ?(stated_at = Datetime.now ()) t =
  let entity = entity_uri t in
  
  let symbol_facts = [
    Poneglyph.fact ~source:Schema.Codedb.source ~entity 
      ~attribute:Schema.OCaml.Symbol.kind
      ~value:(Poneglyph.Fact.String (kind_to_fact_string t.kind))
      ~stated_at ~tx_id;
      
    Poneglyph.fact ~source:Schema.Codedb.source ~entity 
      ~attribute:Schema.OCaml.simple_name
      ~value:(Poneglyph.Fact.String (Module_name.simple_name t.name))
      ~stated_at ~tx_id;
      
    Poneglyph.fact ~source:Schema.Codedb.source ~entity 
      ~attribute:Schema.Codedb.package
      ~value:(Poneglyph.Fact.Uri (Schema.Tusk.Package.uri (Package_name.to_string t.package.name)))
      ~stated_at ~tx_id;
  ] in
  
  let ocaml_facts = [
    Poneglyph.fact ~source:Schema.Codedb.source ~entity
      ~attribute:Schema.OCaml.simple_name
      ~value:(Poneglyph.Fact.String (Module_name.simple_name t.name))
      ~stated_at ~tx_id;
    Poneglyph.fact ~source:Schema.Codedb.source ~entity
      ~attribute:Schema.OCaml.qualified_name
      ~value:(Poneglyph.Fact.String (Module_name.qualified_name t.name))
      ~stated_at ~tx_id;
    Poneglyph.fact ~source:Schema.Codedb.source ~entity
      ~attribute:Schema.OCaml.namespace
      ~value:(Poneglyph.Fact.String (String.concat "." (Module_name.namespace_list t.name)))
      ~stated_at ~tx_id;
    Poneglyph.fact ~source:Schema.Codedb.source ~entity
      ~attribute:Schema.OCaml.is_module
      ~value:(Poneglyph.Fact.Bool (t.kind = Module))
      ~stated_at ~tx_id;
  ] in
  
  (* Add file relationship facts for both implementation and interface *)
  let file_relationship_facts =
    let impl_facts = match t.files.implementation with
      | None -> []
      | Some file -> [
          Poneglyph.fact ~source:Schema.Codedb.source ~entity
            ~attribute:Schema.Codedb.provided_by
            ~value:(Poneglyph.Fact.Uri (File.entity_uri file))
            ~stated_at ~tx_id
        ]
    in
    let intf_facts = match t.files.interface with
      | None -> []
      | Some file -> [
          Poneglyph.fact ~source:Schema.Codedb.source ~entity
            ~attribute:Schema.Codedb.provided_by
            ~value:(Poneglyph.Fact.Uri (File.entity_uri file))
            ~stated_at ~tx_id
        ]
    in
    impl_facts @ intf_facts
  in
  
  (* Add file facts for both implementation and interface *)
  let all_file_facts =
    let impl_facts = match t.files.implementation with
      | None -> []
      | Some file -> File.to_facts ~tx_id ~stated_at file
    in
    let intf_facts = match t.files.interface with
      | None -> []
      | Some file -> File.to_facts ~tx_id ~stated_at file
    in
    impl_facts @ intf_facts
  in
  
  symbol_facts @ ocaml_facts @ file_relationship_facts @ all_file_facts

let kind_to_json = function
  | Module -> Data.Json.String "Module"
  | Value -> Data.Json.String "Value"
  | Type -> Data.Json.String "Type"
  | Interface -> Data.Json.String "Interface"

let kind_from_json json =
  match json with
  | Data.Json.String "Module" -> Ok Module
  | Data.Json.String "Value" -> Ok Value
  | Data.Json.String "Type" -> Ok Type
  | Data.Json.String "Interface" -> Ok Interface
  | Data.Json.String s -> Error ("Unknown kind: " ^ s)
  | _ -> Error "Expected string for symbol kind"

let files_to_json files =
  Data.Json.Object [
    ("implementation", match files.implementation with
      | None -> Data.Json.Null
      | Some f -> File.to_json f);
    ("interface", match files.interface with
      | None -> Data.Json.Null
      | Some f -> File.to_json f);
  ]

let to_json t =
  Data.Json.Object [
    ("kind", kind_to_json t.kind);
    ("name", Module_name.to_json t.name);
    ("package", Package_info.to_json t.package);
    ("files", files_to_json t.files);
  ]

let files_from_json json =
  match json with
  | Data.Json.Object fields ->
      let impl = match List.assoc_opt "implementation" fields with
        | Some Data.Json.Null | None -> Ok None
        | Some f -> (match File.from_json f with
                    | Ok file -> Ok (Some file)
                    | Error e -> Error e)
      in
      let intf = match List.assoc_opt "interface" fields with
        | Some Data.Json.Null | None -> Ok None
        | Some f -> (match File.from_json f with
                    | Ok file -> Ok (Some file)
                    | Error e -> Error e)
      in
      (match (impl, intf) with
       | Ok implementation, Ok interface -> Ok { implementation; interface }
       | Error e, _ | _, Error e -> Error e)
  | _ -> Error "Expected object for files"

let from_json json =
  match json with
  | Data.Json.Object fields ->
      (match 
        ( List.assoc_opt "kind" fields,
          List.assoc_opt "name" fields,
          List.assoc_opt "package" fields,
          List.assoc_opt "files" fields )
       with
       | Some kind_json, Some name_json, Some package_json, Some files_json ->
           (match kind_from_json kind_json with
            | Error e -> Error e
            | Ok kind ->
                (match Module_name.from_json name_json with
                 | Error e -> Error e
                 | Ok name ->
                     (match Package_info.from_json package_json with
                      | Error e -> Error e
                      | Ok package ->
                          (match files_from_json files_json with
                           | Error e -> Error e
                           | Ok files ->
                               Ok { kind; name; package; files }))))
       | _ -> Error "Missing required fields in symbol JSON")
  | _ -> Error "Expected object for symbol"

let reference_to_json (ref : reference) =
  match ref with
  | Module n -> 
      Data.Json.Object [
        ("kind", Data.Json.String "Module");
        ("name", Module_name.to_json n);
      ]
  | Value n ->
      Data.Json.Object [
        ("kind", Data.Json.String "Value");
        ("name", Value_name.to_json n);
      ]
  | Type n ->
      Data.Json.Object [
        ("kind", Data.Json.String "Type");
        ("name", Type_name.to_json n);
      ]
  | Interface n ->
      Data.Json.Object [
        ("kind", Data.Json.String "Interface");
        ("name", Module_name.to_json n);
      ]

let reference_from_json json =
  match json with
  | Data.Json.Object fields ->
      (match List.assoc_opt "kind" fields, List.assoc_opt "name" fields with
       | Some (Data.Json.String "Module"), Some name_json ->
           (match Module_name.from_json name_json with
            | Ok n -> Ok (Module n : reference)
            | Error e -> Error e)
       | Some (Data.Json.String "Value"), Some name_json ->
           (match Value_name.from_json name_json with
            | Ok n -> Ok (Value n : reference)
            | Error e -> Error e)
       | Some (Data.Json.String "Type"), Some name_json ->
           (match Type_name.from_json name_json with
            | Ok n -> Ok (Type n : reference)
            | Error e -> Error e)
       | Some (Data.Json.String "Interface"), Some name_json ->
           (match Module_name.from_json name_json with
            | Ok n -> Ok (Interface n : reference)
            | Error e -> Error e)
       | Some (Data.Json.String s), _ -> Error ("Unknown reference kind: " ^ s)
       | _ -> Error "Missing or invalid fields in reference JSON")
  | _ -> Error "Expected object for symbol reference"
