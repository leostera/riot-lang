open Global

(* Convert a value to string for error messages *)
let value_to_string (v : Spec.value) = match v with
  | Spec.String s -> "\"" ^ s ^ "\""
  | Spec.Char c -> "'" ^ String.make 1 c ^ "'"
  | Spec.Int i -> string_of_int i
  | Spec.Int32 i -> Int32.to_string i
  | Spec.Int64 i -> Int64.to_string i
  | Spec.Bool b -> if b then "true" else "false"
  | Spec.Float f -> string_of_float f
  | Spec.Uri uri -> Net.Uri.to_string uri
  | Spec.Datetime dt -> Datetime.to_iso8601 dt
  | Spec.Path p -> Path.to_string p
  | Spec.Uuid uuid -> Uuid.to_string uuid
  | Spec.Map _ -> "<map>"

(* Custom equality for Spec.value *)
let rec value_equal (v1 : Spec.value) (v2 : Spec.value) =
  match (v1, v2) with
  | Spec.String s1, Spec.String s2 -> String.equal s1 s2
  | Spec.Char c1, Spec.Char c2 -> c1 = c2
  | Spec.Int i1, Spec.Int i2 -> i1 = i2
  | Spec.Int32 i1, Spec.Int32 i2 -> i1 = i2
  | Spec.Int64 i1, Spec.Int64 i2 -> i1 = i2
  | Spec.Bool b1, Spec.Bool b2 -> b1 = b2
  | Spec.Float f1, Spec.Float f2 -> f1 = f2
  | Spec.Uri u1, Spec.Uri u2 -> Net.Uri.equal u1 u2
  | Spec.Datetime d1, Spec.Datetime d2 -> Datetime.equal d1 d2
  | Spec.Path p1, Spec.Path p2 -> Path.equal p1 p2
  | Spec.Uuid u1, Spec.Uuid u2 -> Uuid.equal u1 u2
  | Spec.Map m1, Spec.Map m2 ->
      let rec map_equal l1 l2 =
        match (l1, l2) with
        | [], [] -> true
        | (k1, v1) :: rest1, (k2, v2) :: rest2 ->
            String.equal k1 k2 && value_equal v1 v2 && map_equal rest1 rest2
        | _ -> false
      in
      map_equal m1 m2
  | _ -> false

(* Check if a value is in a list using custom equality *)
let mem_value value choices =
  Collections.List.exists (value_equal value) choices

(* Check if a value is in the allowed values list *)
let check_allowed_values field_name (value : Spec.value) allowed_values =
  match allowed_values with
  | None -> Ok value
  | Some choices ->
      if mem_value value choices then
        Ok value
      else
        let value_str = value_to_string value in
        let choices_str = String.concat ", " (Collections.List.map value_to_string choices) in
        Error (field_name ^ ": invalid value " ^ value_str ^ ", must be one of: " ^ choices_str)

(* Validate a TOML value against a field spec and apply defaults, converting to Spec.value *)
let rec validate_field (field : Spec.field) toml_opt : (Spec.value, string) result =
  let field_name = field.name in
  let field_type = field.field_type in
  let required = field.required in
  let allowed_values = field.allowed_values in
  
  (* First validate the type and get a Spec.value *)
  let type_result : (Spec.value, string) result = match field_type with
  | String { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> Ok (Spec.String s)
      | Some _ -> Error (field_name ^ ": expected string")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.String d)
          | None -> Error (field_name ^ ": no default and not required"))
  
  | Int { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match int_of_string_opt s with
          | Some i -> Ok (Spec.Int i)
          | None -> Error (field_name ^ ": invalid integer"))
      | Some _ -> Error (field_name ^ ": expected integer")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Int d)
          | None -> Error (field_name ^ ": no default"))
  
  | Int32 { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match Int32.of_string_opt s with
          | Some i -> Ok (Spec.Int32 i)
          | None -> Error (field_name ^ ": invalid int32"))
      | Some _ -> Error (field_name ^ ": expected int32")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Int32 d)
          | None -> Error (field_name ^ ": no default"))
  
  | Int64 { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match Int64.of_string_opt s with
          | Some i -> Ok (Spec.Int64 i)
          | None -> Error (field_name ^ ": invalid int64"))
      | Some _ -> Error (field_name ^ ": expected int64")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Int64 d)
          | None -> Error (field_name ^ ": no default"))
  
  | Bool { default } -> (
      match toml_opt with
      | Some (Data.Toml.String "true") -> Ok (Spec.Bool true)
      | Some (Data.Toml.String "false") -> Ok (Spec.Bool false)
      | Some _ -> Error (field_name ^ ": expected boolean")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Bool d)
          | None -> Error (field_name ^ ": no default"))
  
  | Float { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match float_of_string_opt s with
          | Some f -> Ok (Spec.Float f)
          | None -> Error (field_name ^ ": invalid float"))
      | Some _ -> Error (field_name ^ ": expected float")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Float d)
          | None -> Error (field_name ^ ": no default"))
  
  | Char { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          if String.length s = 1 then
            Ok (Spec.Char (String.get s 0))
          else
            Error (field_name ^ ": expected single character, got: " ^ s))
      | Some _ -> Error (field_name ^ ": expected character")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Char d)
          | None -> Error (field_name ^ ": no default"))
  
  | Uri { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match Net.Uri.of_string s with
          | Ok uri -> Ok (Spec.Uri uri)
          | Error _ -> Error (field_name ^ ": invalid URI: " ^ s))
      | Some _ -> Error (field_name ^ ": expected URI string")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Uri d)
          | None -> Error (field_name ^ ": no default"))
  
  | Datetime { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match Datetime.parse s with
          | Ok dt -> Ok (Spec.Datetime dt)
          | Error _ -> Error (field_name ^ ": invalid datetime: " ^ s))
      | Some _ -> Error (field_name ^ ": expected datetime string")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Datetime d)
          | None -> Error (field_name ^ ": no default"))
  
  | Path { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match Path.of_string s with
          | Ok path -> Ok (Spec.Path path)
          | Error _ -> Error (field_name ^ ": invalid path: " ^ s))
      | Some _ -> Error (field_name ^ ": expected path string")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Path d)
          | None -> Error (field_name ^ ": no default"))
  
  | Uuid { default } -> (
      match toml_opt with
      | Some (Data.Toml.String s) -> (
          match Uuid.of_string s with
          | Ok uuid -> Ok (Spec.Uuid uuid)
          | Error _ -> Error (field_name ^ ": invalid UUID: " ^ s))
      | Some _ -> Error (field_name ^ ": expected UUID string")
      | None ->
          if required then Error (field_name ^ ": required field missing")
          else match default with
          | Some d -> Ok (Spec.Uuid d)
          | None -> Error (field_name ^ ": no default"))
  
  | Map fields -> (
      match toml_opt with
      | Some (Data.Toml.Table table) ->
          validate_fields fields (Some (Data.Toml.Table table))
      | Some _ -> Error (field_name ^ ": expected table/map")
      | None -> validate_fields fields None)
  in
  
  (* Then check if value is in allowed_values *)
  match type_result with
  | Error err -> Error err
  | Ok validated_value -> check_allowed_values field_name validated_value allowed_values

and validate_fields (fields : Spec.field list) toml_opt : (Spec.value, string) result =
  let table = match toml_opt with
    | Some (Data.Toml.Table t) -> t
    | _ -> []
  in
  
  let rec process_fields acc = function
    | [] -> Ok (Collections.List.rev acc)
    | field :: rest ->
        let name = field.Spec.name in
        let field_value = Collections.List.assoc_opt name table in
        match validate_field field field_value with
        | Ok validated -> process_fields ((name, validated) :: acc) rest
        | Error err -> Error err
  in
  
  match process_fields [] fields with
  | Ok validated_fields -> Ok (Spec.Map validated_fields)
  | Error err -> Error err

let validate spec toml : (Spec.value, string) result =
  validate_fields (Spec.get_fields spec) (Some toml)
