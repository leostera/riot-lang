open Std

(** Core Types *)
type 'msg attr =
  | Attr of string * string
  | Event of string * (string -> 'msg)

type 'msg t =
  | El of { tag: string; attrs: 'msg attr list; children: 'msg t list }
  | Text of string
  | Fragment of 'msg t list

(** Attribute Constructors *)
let attr = fun name value -> Attr (name, value)

let class_ = fun value -> Attr ("class", value)

let style_ = fun value -> Attr ("style", value)

let id = fun value -> Attr ("id", value)

let title_ = fun value -> Attr ("title", value)

let name = fun value -> Attr ("name", value)

let value = fun value -> Attr ("value", value)

let placeholder = fun value -> Attr ("placeholder", value)

let type_ = fun value -> Attr ("type", value)

let href = fun value -> Attr ("href", value)

let src = fun value -> Attr ("src", value)

let alt = fun value -> Attr ("alt", value)

let target = fun value -> Attr ("target", value)

let rel = fun value -> Attr ("rel", value)

let for_ = fun value -> Attr ("for", value)

let action = fun value -> Attr ("action", value)

let method_ = fun value -> Attr ("method", value)

let disabled = Attr ("disabled", "disabled")

let readonly = Attr ("readonly", "readonly")

let required = Attr ("required", "required")

let checked = Attr ("checked", "checked")

let selected = Attr ("selected", "selected")

let autofocus = Attr ("autofocus", "autofocus")

let autocomplete = fun value -> Attr ("autocomplete", value)

let data = fun name value -> Attr ("data-" ^ name, value)

(** Event Constructors *)
let on = fun event_name handler -> Event (event_name, handler)

let on_click = fun handler -> Event ("click", handler)

let on_dblclick = fun handler -> Event ("dblclick", handler)

let on_submit = fun handler -> Event ("submit", handler)

let on_input = fun handler -> Event ("input", handler)

let on_change = fun handler -> Event ("change", handler)

let on_focus = fun handler -> Event ("focus", handler)

let on_blur = fun handler -> Event ("blur", handler)

let on_keydown = fun handler -> Event ("keydown", handler)

let on_keyup = fun handler -> Event ("keyup", handler)

let on_keypress = fun handler -> Event ("keypress", handler)

let on_mouseenter = fun handler -> Event ("mouseenter", handler)

let on_mouseleave = fun handler -> Event ("mouseleave", handler)

let on_mouseover = fun handler -> Event ("mouseover", handler)

let on_mouseout = fun handler -> Event ("mouseout", handler)

(** HTML Element Constructors *)
(* Generic element builder *)

let el = fun tag ?(attrs = []) children -> El { tag; attrs; children }

(* Document structure *)

let html = fun ?(attrs = []) children -> El { tag = "html"; attrs; children }

let head = fun ?(attrs = []) children -> El { tag = "head"; attrs; children }

let body = fun ?(attrs = []) children -> El { tag = "body"; attrs; children }

let title = fun ?(attrs = []) children -> El { tag = "title"; attrs; children }

let base = fun ?(attrs = []) () -> El { tag = "base"; attrs; children = [] }

let meta = fun ?(attrs = []) () -> El { tag = "meta"; attrs; children = [] }

let link = fun ?(attrs = []) () -> El { tag = "link"; attrs; children = [] }

let style = fun ?(attrs = []) content -> El { tag = "style"; attrs; children = [ Text content ] }

let script = fun ?(attrs = []) content -> El { tag = "script"; attrs; children = [ Text content ] }

(* Content sectioning *)

let header = fun ?(attrs = []) children -> El { tag = "header"; attrs; children }

let nav = fun ?(attrs = []) children -> El { tag = "nav"; attrs; children }

let main = fun ?(attrs = []) children -> El { tag = "main"; attrs; children }

let section = fun ?(attrs = []) children -> El { tag = "section"; attrs; children }

let article = fun ?(attrs = []) children -> El { tag = "article"; attrs; children }

let aside = fun ?(attrs = []) children -> El { tag = "aside"; attrs; children }

let footer = fun ?(attrs = []) children -> El { tag = "footer"; attrs; children }

let address = fun ?(attrs = []) children -> El { tag = "address"; attrs; children }

let hgroup = fun ?(attrs = []) children -> El { tag = "hgroup"; attrs; children }

let search = fun ?(attrs = []) children -> El { tag = "search"; attrs; children }

(* Text content *)

let div = fun ?(attrs = []) children -> El { tag = "div"; attrs; children }

let p = fun ?(attrs = []) children -> El { tag = "p"; attrs; children }

let span = fun ?(attrs = []) children -> El { tag = "span"; attrs; children }

let h1 = fun ?(attrs = []) children -> El { tag = "h1"; attrs; children }

let h2 = fun ?(attrs = []) children -> El { tag = "h2"; attrs; children }

let h3 = fun ?(attrs = []) children -> El { tag = "h3"; attrs; children }

let h4 = fun ?(attrs = []) children -> El { tag = "h4"; attrs; children }

let h5 = fun ?(attrs = []) children -> El { tag = "h5"; attrs; children }

let h6 = fun ?(attrs = []) children -> El { tag = "h6"; attrs; children }

let pre = fun ?(attrs = []) children -> El { tag = "pre"; attrs; children }

let blockquote = fun ?(attrs = []) children -> El { tag = "blockquote"; attrs; children }

let hr = fun ?(attrs = []) () -> El { tag = "hr"; attrs; children = [] }

let br = fun () -> El { tag = "br"; attrs = []; children = [] }

let figure = fun ?(attrs = []) children -> El { tag = "figure"; attrs; children }

let figcaption = fun ?(attrs = []) children -> El { tag = "figcaption"; attrs; children }

let menu = fun ?(attrs = []) children -> El { tag = "menu"; attrs; children }

(* Inline text semantics *)

let a = fun ?(attrs = []) children -> El { tag = "a"; attrs; children }

let abbr = fun ?(attrs = []) children -> El { tag = "abbr"; attrs; children }

let b = fun ?(attrs = []) children -> El { tag = "b"; attrs; children }

let bdi = fun ?(attrs = []) children -> El { tag = "bdi"; attrs; children }

let bdo = fun ?(attrs = []) children -> El { tag = "bdo"; attrs; children }

let cite = fun ?(attrs = []) children -> El { tag = "cite"; attrs; children }

let code = fun ?(attrs = []) children -> El { tag = "code"; attrs; children }

let data = fun ?(attrs = []) children -> El { tag = "data"; attrs; children }

let dfn = fun ?(attrs = []) children -> El { tag = "dfn"; attrs; children }

let em = fun ?(attrs = []) children -> El { tag = "em"; attrs; children }

let i = fun ?(attrs = []) children -> El { tag = "i"; attrs; children }

let kbd = fun ?(attrs = []) children -> El { tag = "kbd"; attrs; children }

let mark = fun ?(attrs = []) children -> El { tag = "mark"; attrs; children }

let q = fun ?(attrs = []) children -> El { tag = "q"; attrs; children }

let rp = fun ?(attrs = []) children -> El { tag = "rp"; attrs; children }

let rt = fun ?(attrs = []) children -> El { tag = "rt"; attrs; children }

let ruby = fun ?(attrs = []) children -> El { tag = "ruby"; attrs; children }

let s = fun ?(attrs = []) children -> El { tag = "s"; attrs; children }

let samp = fun ?(attrs = []) children -> El { tag = "samp"; attrs; children }

let small = fun ?(attrs = []) children -> El { tag = "small"; attrs; children }

let strong = fun ?(attrs = []) children -> El { tag = "strong"; attrs; children }

let sub = fun ?(attrs = []) children -> El { tag = "sub"; attrs; children }

let sup = fun ?(attrs = []) children -> El { tag = "sup"; attrs; children }

let time = fun ?(attrs = []) children -> El { tag = "time"; attrs; children }

let u = fun ?(attrs = []) children -> El { tag = "u"; attrs; children }

let var = fun ?(attrs = []) children -> El { tag = "var"; attrs; children }

let wbr = fun ?(attrs = []) () -> El { tag = "wbr"; attrs; children = [] }

let del = fun ?(attrs = []) children -> El { tag = "del"; attrs; children }

let ins = fun ?(attrs = []) children -> El { tag = "ins"; attrs; children }

(* Lists *)

let ul = fun ?(attrs = []) children -> El { tag = "ul"; attrs; children }

let ol = fun ?(attrs = []) children -> El { tag = "ol"; attrs; children }

let li = fun ?(attrs = []) children -> El { tag = "li"; attrs; children }

let dl = fun ?(attrs = []) children -> El { tag = "dl"; attrs; children }

let dt = fun ?(attrs = []) children -> El { tag = "dt"; attrs; children }

let dd = fun ?(attrs = []) children -> El { tag = "dd"; attrs; children }

(* Tables *)

let table = fun ?(attrs = []) children -> El { tag = "table"; attrs; children }

let caption = fun ?(attrs = []) children -> El { tag = "caption"; attrs; children }

let thead = fun ?(attrs = []) children -> El { tag = "thead"; attrs; children }

let tbody = fun ?(attrs = []) children -> El { tag = "tbody"; attrs; children }

let tfoot = fun ?(attrs = []) children -> El { tag = "tfoot"; attrs; children }

let tr = fun ?(attrs = []) children -> El { tag = "tr"; attrs; children }

let th = fun ?(attrs = []) children -> El { tag = "th"; attrs; children }

let td = fun ?(attrs = []) children -> El { tag = "td"; attrs; children }

let col = fun ?(attrs = []) () -> El { tag = "col"; attrs; children = [] }

let colgroup = fun ?(attrs = []) children -> El { tag = "colgroup"; attrs; children }

(* Forms *)

let form = fun ?(attrs = []) children -> El { tag = "form"; attrs; children }

let fieldset = fun ?(attrs = []) children -> El { tag = "fieldset"; attrs; children }

let legend = fun ?(attrs = []) children -> El { tag = "legend"; attrs; children }

let label = fun ?(attrs = []) children -> El { tag = "label"; attrs; children }

let input = fun ?(attrs = []) () -> El { tag = "input"; attrs; children = [] }

let button = fun ?(attrs = []) children -> El { tag = "button"; attrs; children }

let select = fun ?(attrs = []) children -> El { tag = "select"; attrs; children }

let datalist = fun ?(attrs = []) children -> El { tag = "datalist"; attrs; children }

let optgroup = fun ?(attrs = []) children -> El { tag = "optgroup"; attrs; children }

let option = fun ?(attrs = []) children -> El { tag = "option"; attrs; children }

let textarea = fun ?(attrs = []) children -> El { tag = "textarea"; attrs; children }

let output = fun ?(attrs = []) children -> El { tag = "output"; attrs; children }

let progress = fun ?(attrs = []) children -> El { tag = "progress"; attrs; children }

let meter = fun ?(attrs = []) children -> El { tag = "meter"; attrs; children }

(* Interactive *)

let details = fun ?(attrs = []) children -> El { tag = "details"; attrs; children }

let summary = fun ?(attrs = []) children -> El { tag = "summary"; attrs; children }

let dialog = fun ?(attrs = []) children -> El { tag = "dialog"; attrs; children }

(* Image and multimedia *)

let area = fun ?(attrs = []) () -> El { tag = "area"; attrs; children = [] }

let audio = fun ?(attrs = []) children -> El { tag = "audio"; attrs; children }

let img = fun ?(attrs = []) () -> El { tag = "img"; attrs; children = [] }

let map = fun ?(attrs = []) children -> El { tag = "map"; attrs; children }

let track = fun ?(attrs = []) () -> El { tag = "track"; attrs; children = [] }

let video = fun ?(attrs = []) children -> El { tag = "video"; attrs; children }

(* Embedded content *)

let embed = fun ?(attrs = []) () -> El { tag = "embed"; attrs; children = [] }

let iframe = fun ?(attrs = []) children -> El { tag = "iframe"; attrs; children }

let object_ = fun ?(attrs = []) children -> El { tag = "object"; attrs; children }

let picture = fun ?(attrs = []) children -> El { tag = "picture"; attrs; children }

let source = fun ?(attrs = []) () -> El { tag = "source"; attrs; children = [] }

(* Scripting *)

let canvas = fun ?(attrs = []) children -> El { tag = "canvas"; attrs; children }

let noscript = fun ?(attrs = []) children -> El { tag = "noscript"; attrs; children }

(* SVG and MathML *)

let svg = fun ?(attrs = []) children -> El { tag = "svg"; attrs; children }

let math = fun ?(attrs = []) children -> El { tag = "math"; attrs; children }

(** Content Helpers *)
let text = fun str -> Text str

let int = fun n -> Text (Int.to_string n)

let float = fun f -> Text (Float.to_string f)

let fragment = fun children -> Fragment children

let empty = Fragment []

(** Conditional Rendering *)
let when_ = fun condition element ->
  if condition then
    element
  else
    empty

let unless = fun condition element ->
  if not condition then
    element
  else
    empty

let maybe = fun opt f ->
  match opt with
  | Some x -> f x
  | None -> empty

(** Rendering *)
(* Web Components *)

let slot = fun ?(attrs = []) children -> El { tag = "slot"; attrs; children }

let template = fun ?(attrs = []) children -> El { tag = "template"; attrs; children }

(** Rendering *)
(* Self-closing tags per HTML5 spec *)

let self_closing_tags = [
  "area";
  "base";
  "br";
  "col";
  "embed";
  "hr";
  "img";
  "input";
  "link";
  "meta";
  "source";
  "track";
  "wbr";
]

let is_self_closing = fun tag ->
  List.mem tag self_closing_tags

let rec to_html = fun t ->
  match t with
  | Text str ->
      str
  | Fragment children ->
      String.concat "" (List.map ~fn:to_html children)
  | El { tag; attrs; children } ->
      let attrs_str = attrs_to_string attrs in
      let attrs_part =
        if attrs_str = "" then
          ""
        else
          " " ^ attrs_str
      in
      if is_self_closing tag then
        "<" ^ tag ^ attrs_part ^ " />"
      else
        let children_html = String.concat "" (List.map ~fn:to_html children) in
        "<" ^ tag ^ attrs_part ^ ">" ^ children_html ^ "</" ^ tag ^ ">"

and attrs_to_string = fun attrs ->
  attrs |> List.filter_map
    (
      function
      | Attr (k, v) -> Some (k ^ "=\"" ^ escape_attr v ^ "\"")
      | Event _ -> None
    ) |> String.concat " "

and escape_attr = fun str ->
  (* HTML attribute escaping *)
  let buf = IO.Buffer.create (String.length str) in
  String.iter
    (
      function
      | '"' -> IO.Buffer.add_string buf "&quot;"
      | '&' -> IO.Buffer.add_string buf "&amp;"
      | '<' -> IO.Buffer.add_string buf "&lt;"
      | '>' -> IO.Buffer.add_string buf "&gt;"
      | c -> IO.Buffer.add_char buf c
    )
    str;
  IO.Buffer.contents buf

(** Advanced *)
let rec map = fun f t ->
  match t with
  | Text str ->
      Text str
  | Fragment children ->
      Fragment (List.map ~fn:(map f) children)
  | El { tag; attrs; children } ->
      let attrs' = List.map ~fn:(map_attr f) attrs in
      let children' = List.map ~fn:(map f) children in
      El { tag; attrs = attrs'; children = children' }

and map_attr = fun f attr ->
  match attr with
  | Attr (k, v) -> Attr (k, v)
  | Event (name, handler) -> Event (name, fun ev -> f (handler ev))

let extract_handlers = fun t ->
  let rec go = fun acc ->
    function
    | Text _ ->
        acc
    | Fragment children ->
        List.fold_left go acc children
    | El { attrs; children; _ } ->
        let attr_handlers =
          List.filter_map
            (
              function
              | Event (name, handler) -> Some (name, handler)
              | Attr _ -> None
            )
            attrs
        in
        List.fold_left go (attr_handlers @ acc) children
  in
  go [] t
