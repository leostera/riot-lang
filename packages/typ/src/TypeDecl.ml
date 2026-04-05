open Std

type constructor = {
  name: string;
  scheme: TypeScheme.t;
}

type t = {
  type_name: string;
  constructors: constructor list;
}

let constructor_entries = fun decl ->
  decl.constructors |> List.map (fun (constructor: constructor) -> (constructor.name, constructor.scheme))

let constructor_to_json = fun (constructor: constructor) ->
  Data.Json.Object [
    ("name", Data.Json.String constructor.name);
    ("scheme", Data.Json.String (TypePrinter.scheme_to_string constructor.scheme));
  ]

let to_json = fun decl ->
  Data.Json.Object [
    ("type_name", Data.Json.String decl.type_name);
    ("constructors", Data.Json.Array (List.map constructor_to_json decl.constructors));
  ]

let to_string = fun decl ->
  let constructors =
    match decl.constructors with
    | [] -> "none"
    | constructors ->
        constructors
        |> List.map
          (fun (constructor: constructor) ->
            constructor.name ^ " : " ^ TypePrinter.scheme_to_string constructor.scheme)
        |> String.concat ", "
  in
  decl.type_name ^ " { " ^ constructors ^ " }"
