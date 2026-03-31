open Std

type 'msg attr =
[
  `event of string * (string -> 'msg)
  | `attr of string * string
]

let event = fun name fn -> `event (name, fn)

let on_click = fun fn -> event "click" fn

let attr = fun name value -> `attr (name, value)

let attr_id = fun v -> attr "id" v

let attr_type = fun v -> attr "type" v

let attr_src = fun v -> attr "src" v

type 'msg t =
  | El of { tag: string; attrs: 'msg attr list; children: 'msg t list; }
  | Text of string
  | Splat of 'msg t list

let list = fun els -> Splat els

let button = fun ~on_click ?(children = []) () -> El {tag = "button"; attrs = [ on_click ]; children}

let html = fun ?(children = []) () -> El {tag = "html"; attrs = []; children}

let body = fun ?(children = []) () -> El {tag = "body"; attrs = []; children}

let div = fun ?(attrs = []) ?id ?(children = []) () -> El {
  tag = "div";
  attrs = List.filter_map (fun x -> x) [ Option.map attr_id id ]
  @ List.map (fun ((k, v)) -> `attr (k, v)) attrs;
  children;

}

let h1 = fun ?(children = []) () -> El {tag = "h1"; attrs = []; children}

let h2 = fun ?(children = []) () -> El {tag = "h2"; attrs = []; children}

let h3 = fun ?(children = []) () -> El {tag = "h3"; attrs = []; children}

let h4 = fun ?(children = []) () -> El {tag = "h4"; attrs = []; children}

let h5 = fun ?(children = []) () -> El {tag = "h5"; attrs = []; children}

let h6 = fun ?(children = []) () -> El {tag = "h6"; attrs = []; children}

let span = fun ?(children = []) () -> El {tag = "span"; attrs = []; children}

let p = fun ?(children = []) () -> El {tag = "p"; attrs = []; children}

let script = fun ?src ?id ?type_ ?(children = []) () -> El {
  tag = "script";
  attrs = [ Option.map attr_id id; Option.map attr_type type_; Option.map attr_src src;  ]
  |> List.filter_map (fun x -> x);
  children;

}

let string = fun (str:string) -> Text str

let int = fun (x:int) -> Text (Int.to_string x)

let rec to_string = fun (t:'msg t) ->
  match t with
  | Text str -> str
  | Splat els -> String.concat "\n" (List.map to_string els)
  | El { tag; children; attrs } -> "<"
  ^ tag
  ^ " "
  ^ attrs_to_string attrs
  ^ ">"
  ^ (List.map to_string children |> String.concat "\n")
  ^ "</"
  ^ tag
  ^ ">"
and attrs_to_string = fun attrs ->
  List.map
    (
      function
      | `attr (k, v) -> k ^ "=" ^ "\"" ^ v ^ "\""
      | _ -> ""
    )
    attrs |> String.concat " "

let event_handlers = fun attrs ->
  List.filter_map
    (fun attr ->
      match attr with
      | `event (name, fn) -> Some (name, fn)
      | _ -> None)
    attrs

let rec map_action = fun fn t ->
  match t with
  | Text string ->
      Text string
  | Splat els ->
      Splat (List.map (map_action fn) els)
  | El { tag; children; attrs } ->
      let children = List.map (map_action fn) children in
      let attrs =
        List.map
          (fun attr ->
            match attr with
            | `event (name, handler) -> `event (name, fun ev -> fn (handler ev))
            | `attr (k, v) -> `attr (k, v))
          attrs
      in
      El {tag; children; attrs}
