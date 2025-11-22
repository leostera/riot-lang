open Global

type value =
  | String of string
  | Char of char
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Bool of bool
  | Float of float
  | Uri of Net.Uri.t
  | Datetime of Datetime.t
  | Path of Path.t
  | Uuid of Uuid.t
  | List of value list
  | DiscriminatedUnion of {
      discriminant : string;
      variant : string;
      fields : (string * value) list;
    }
  | Map of (string * value) list

type field_type =
  | String of { default : string option }
  | Char of { default : char option }
  | Int of { default : int option }
  | Int32 of { default : int32 option }
  | Int64 of { default : int64 option }
  | Bool of { default : bool option }
  | Float of { default : float option }
  | Uri of { default : Net.Uri.t option }
  | Datetime of { default : Datetime.t option }
  | Path of { default : Path.t option }
  | Uuid of { default : Uuid.t option }
  | List of { item_spec : field; default : value list option }
  | DiscriminatedUnion of {
      discriminant : string;
      cases : (string * field list) list;
    }
  | Map of field list

and field = {
  name : string;
  field_type : field_type;
  required : bool;
  help : string option;
  allowed_values : value list option;
}

type field_spec = field

type t = {
  app : string;
  fields : field list;
}

(* Global registry of specs - mutable! *)
let registered_specs : t list Sync.Cell.t = cell []

let for_app ~app fields = 
  let spec = { app; fields } in
  (* Register the spec globally *)
  let current = !registered_specs in
  registered_specs := spec :: current;
  spec

let all_specs () = !registered_specs

let string ?default ?(required = false) ?help name =
  { name; field_type = String { default }; required; help; allowed_values = None }

let char ?default ?(required = false) ?help name =
  { name; field_type = Char { default }; required; help; allowed_values = None }

let int ?default ?(required = false) ?help name =
  { name; field_type = Int { default }; required; help; allowed_values = None }

let int32 ?default ?(required = false) ?help name =
  { name; field_type = Int32 { default }; required; help; allowed_values = None }

let int64 ?default ?(required = false) ?help name =
  { name; field_type = Int64 { default }; required; help; allowed_values = None }

let bool ?default ?(required = false) ?help name =
  { name; field_type = Bool { default }; required; help; allowed_values = None }

let float ?default ?(required = false) ?help name =
  { name; field_type = Float { default }; required; help; allowed_values = None }

let uri ?default ?(required = false) ?help name =
  { name; field_type = Uri { default }; required; help; allowed_values = None }

let datetime ?default ?(required = false) ?help name =
  { name; field_type = Datetime { default }; required; help; allowed_values = None }

let path ?default ?(required = false) ?help name =
  { name; field_type = Path { default }; required; help; allowed_values = None }

let uuid ?default ?(required = false) ?help name =
  { name; field_type = Uuid { default }; required; help; allowed_values = None }

let enum field_spec choices =
  { field_spec with allowed_values = Some choices }

let list item_spec ?default ?(required = false) ?help name =
  { name; field_type = List { item_spec; default }; required; help; allowed_values = None }

let discriminated_union ~discriminant ~cases =
  { name = ""; 
    field_type = DiscriminatedUnion { discriminant; cases }; 
    required = false; 
    help = None; 
    allowed_values = None }

let map fields =
  { name = ""; field_type = Map fields; required = false; help = None; allowed_values = None }

let key name field_spec =
  { field_spec with name }

let app_name spec = spec.app

let get_fields spec = spec.fields

let field_name field = field.name

let field_type field = field.field_type
