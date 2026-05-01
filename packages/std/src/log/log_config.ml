open Global
open Collections
open Config

type format_style =
  | Full
  | Compact

type handler_config =
  | Stdout of {
      format: format_style;
    }
  | File of {
      path: string;
      format: format_style;
    }

type t = {
  handlers: handler_config list;
}

let spec =
  Spec.(for_app
    ~app:"log"
    [
      list
        (discriminated_union
          ~discriminant:"type"
          ~cases:[
            ("stdout", [ string "format" ~default:"full" ]);
            ("file", [ string "path" ~required:true; string "format" ~default:"full" ]);
          ])
        "handler"
        ~default:[];
    ])

let parse_format = fun __tmp1 ->
  match __tmp1 with
  | "full" -> Full
  | "compact" -> Compact
  | _ -> Full

let find_field = fun key fields ->
  List.find fields ~fn:(fun (field_name, _value) -> String.equal field_name key)
  |> Option.map ~fn:(fun (_field_name, value) -> value)

let get = fun conf ->
  let handlers_list = get_list conf "handler" in
  let handlers =
    List.map
      handlers_list
      ~fn:(fun handler_value ->
        let (_disc, variant, fields) = as_discriminated_union handler_value in
        match variant with
        | "stdout" ->
            let format =
              match find_field "format" fields with
              | Some (Spec.String s) -> parse_format s
              | _ -> Full
            in
            Stdout { format }
        | "file" ->
            let path =
              match find_field "path" fields with
              | Some (Spec.String p) -> p
              | _ ->
                  panic
                    "file handler requires path (this should not happen - spec should enforce it)"
            in
            let format =
              match find_field "format" fields with
              | Some (Spec.String s) -> parse_format s
              | _ -> Full
            in
            File { path; format }
        | _ ->
            panic
              ("Unknown handler type: "
              ^ variant
              ^ " (this should not happen - spec should enforce it)"))
  in
  Ok { handlers }
