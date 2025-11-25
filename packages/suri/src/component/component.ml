open Std

(** Core Types *)

type 'msg attr =
  | Attr of string * string
  | Event of string * (string -> 'msg)

type 'msg t =
  | El of { tag : string; attrs : 'msg attr list; children : 'msg t list }
  | Text of string
  | Fragment of 'msg t list

(** Attribute Constructors *)

let attr name value = Attr (name, value)
let class_ value = Attr ("class", value)
let style_ value = Attr ("style", value)
let id value = Attr ("id", value)
let title_ value = Attr ("title", value)
let name value = Attr ("name", value)
let value value = Attr ("value", value)
let placeholder value = Attr ("placeholder", value)
let type_ value = Attr ("type", value)
let href value = Attr ("href", value)
let src value = Attr ("src", value)
let alt value = Attr ("alt", value)
let target value = Attr ("target", value)
let rel value = Attr ("rel", value)
let for_ value = Attr ("for", value)
let action value = Attr ("action", value)
let method_ value = Attr ("method", value)
let disabled = Attr ("disabled", "disabled")
let readonly = Attr ("readonly", "readonly")
let required = Attr ("required", "required")
let checked = Attr ("checked", "checked")
let selected = Attr ("selected", "selected")
let autofocus = Attr ("autofocus", "autofocus")
let autocomplete value = Attr ("autocomplete", value)
let data name value = Attr ("data-" ^ name, value)

(** Event Constructors *)

let on event_name handler = Event (event_name, handler)
let on_click handler = Event ("click", handler)
let on_dblclick handler = Event ("dblclick", handler)
let on_submit handler = Event ("submit", handler)
let on_input handler = Event ("input", handler)
let on_change handler = Event ("change", handler)
let on_focus handler = Event ("focus", handler)
let on_blur handler = Event ("blur", handler)
let on_keydown handler = Event ("keydown", handler)
let on_keyup handler = Event ("keyup", handler)
let on_keypress handler = Event ("keypress", handler)
let on_mouseenter handler = Event ("mouseenter", handler)
let on_mouseleave handler = Event ("mouseleave", handler)
let on_mouseover handler = Event ("mouseover", handler)
let on_mouseout handler = Event ("mouseout", handler)

(** HTML Element Constructors *)

(* Generic element builder *)
let el tag ?(attrs = []) children = El { tag; attrs; children }

(* Document structure *)
let html ?(attrs = []) children = El { tag = "html"; attrs; children }
let head ?(attrs = []) children = El { tag = "head"; attrs; children }
let body ?(attrs = []) children = El { tag = "body"; attrs; children }
let title ?(attrs = []) children = El { tag = "title"; attrs; children }
let base ?(attrs = []) () = El { tag = "base"; attrs; children = [] }
let meta ?(attrs = []) () = El { tag = "meta"; attrs; children = [] }
let link ?(attrs = []) () = El { tag = "link"; attrs; children = [] }
let style ?(attrs = []) content = El { tag = "style"; attrs; children = [Text content] }
let script ?(attrs = []) content = El { tag = "script"; attrs; children = [Text content] }

(* Content sectioning *)
let header ?(attrs = []) children = El { tag = "header"; attrs; children }
let nav ?(attrs = []) children = El { tag = "nav"; attrs; children }
let main ?(attrs = []) children = El { tag = "main"; attrs; children }
let section ?(attrs = []) children = El { tag = "section"; attrs; children }
let article ?(attrs = []) children = El { tag = "article"; attrs; children }
let aside ?(attrs = []) children = El { tag = "aside"; attrs; children }
let footer ?(attrs = []) children = El { tag = "footer"; attrs; children }
let address ?(attrs = []) children = El { tag = "address"; attrs; children }
let hgroup ?(attrs = []) children = El { tag = "hgroup"; attrs; children }
let search ?(attrs = []) children = El { tag = "search"; attrs; children }

(* Text content *)
let div ?(attrs = []) children = El { tag = "div"; attrs; children }
let p ?(attrs = []) children = El { tag = "p"; attrs; children }
let span ?(attrs = []) children = El { tag = "span"; attrs; children }
let h1 ?(attrs = []) children = El { tag = "h1"; attrs; children }
let h2 ?(attrs = []) children = El { tag = "h2"; attrs; children }
let h3 ?(attrs = []) children = El { tag = "h3"; attrs; children }
let h4 ?(attrs = []) children = El { tag = "h4"; attrs; children }
let h5 ?(attrs = []) children = El { tag = "h5"; attrs; children }
let h6 ?(attrs = []) children = El { tag = "h6"; attrs; children }
let pre ?(attrs = []) children = El { tag = "pre"; attrs; children }
let blockquote ?(attrs = []) children = El { tag = "blockquote"; attrs; children }
let hr ?(attrs = []) () = El { tag = "hr"; attrs; children = [] }
let br () = El { tag = "br"; attrs = []; children = [] }
let figure ?(attrs = []) children = El { tag = "figure"; attrs; children }
let figcaption ?(attrs = []) children = El { tag = "figcaption"; attrs; children }
let menu ?(attrs = []) children = El { tag = "menu"; attrs; children }

(* Inline text semantics *)
let a ?(attrs = []) children = El { tag = "a"; attrs; children }
let abbr ?(attrs = []) children = El { tag = "abbr"; attrs; children }
let b ?(attrs = []) children = El { tag = "b"; attrs; children }
let bdi ?(attrs = []) children = El { tag = "bdi"; attrs; children }
let bdo ?(attrs = []) children = El { tag = "bdo"; attrs; children }
let cite ?(attrs = []) children = El { tag = "cite"; attrs; children }
let code ?(attrs = []) children = El { tag = "code"; attrs; children }
let data ?(attrs = []) children = El { tag = "data"; attrs; children }
let dfn ?(attrs = []) children = El { tag = "dfn"; attrs; children }
let em ?(attrs = []) children = El { tag = "em"; attrs; children }
let i ?(attrs = []) children = El { tag = "i"; attrs; children }
let kbd ?(attrs = []) children = El { tag = "kbd"; attrs; children }
let mark ?(attrs = []) children = El { tag = "mark"; attrs; children }
let q ?(attrs = []) children = El { tag = "q"; attrs; children }
let rp ?(attrs = []) children = El { tag = "rp"; attrs; children }
let rt ?(attrs = []) children = El { tag = "rt"; attrs; children }
let ruby ?(attrs = []) children = El { tag = "ruby"; attrs; children }
let s ?(attrs = []) children = El { tag = "s"; attrs; children }
let samp ?(attrs = []) children = El { tag = "samp"; attrs; children }
let small ?(attrs = []) children = El { tag = "small"; attrs; children }
let strong ?(attrs = []) children = El { tag = "strong"; attrs; children }
let sub ?(attrs = []) children = El { tag = "sub"; attrs; children }
let sup ?(attrs = []) children = El { tag = "sup"; attrs; children }
let time ?(attrs = []) children = El { tag = "time"; attrs; children }
let u ?(attrs = []) children = El { tag = "u"; attrs; children }
let var ?(attrs = []) children = El { tag = "var"; attrs; children }
let wbr ?(attrs = []) () = El { tag = "wbr"; attrs; children = [] }
let del ?(attrs = []) children = El { tag = "del"; attrs; children }
let ins ?(attrs = []) children = El { tag = "ins"; attrs; children }

(* Lists *)
let ul ?(attrs = []) children = El { tag = "ul"; attrs; children }
let ol ?(attrs = []) children = El { tag = "ol"; attrs; children }
let li ?(attrs = []) children = El { tag = "li"; attrs; children }
let dl ?(attrs = []) children = El { tag = "dl"; attrs; children }
let dt ?(attrs = []) children = El { tag = "dt"; attrs; children }
let dd ?(attrs = []) children = El { tag = "dd"; attrs; children }

(* Tables *)
let table ?(attrs = []) children = El { tag = "table"; attrs; children }
let caption ?(attrs = []) children = El { tag = "caption"; attrs; children }
let thead ?(attrs = []) children = El { tag = "thead"; attrs; children }
let tbody ?(attrs = []) children = El { tag = "tbody"; attrs; children }
let tfoot ?(attrs = []) children = El { tag = "tfoot"; attrs; children }
let tr ?(attrs = []) children = El { tag = "tr"; attrs; children }
let th ?(attrs = []) children = El { tag = "th"; attrs; children }
let td ?(attrs = []) children = El { tag = "td"; attrs; children }
let col ?(attrs = []) () = El { tag = "col"; attrs; children = [] }
let colgroup ?(attrs = []) children = El { tag = "colgroup"; attrs; children }

(* Forms *)
let form ?(attrs = []) children = El { tag = "form"; attrs; children }
let fieldset ?(attrs = []) children = El { tag = "fieldset"; attrs; children }
let legend ?(attrs = []) children = El { tag = "legend"; attrs; children }
let label ?(attrs = []) children = El { tag = "label"; attrs; children }
let input ?(attrs = []) () = El { tag = "input"; attrs; children = [] }
let button ?(attrs = []) children = El { tag = "button"; attrs; children }
let select ?(attrs = []) children = El { tag = "select"; attrs; children }
let datalist ?(attrs = []) children = El { tag = "datalist"; attrs; children }
let optgroup ?(attrs = []) children = El { tag = "optgroup"; attrs; children }
let option ?(attrs = []) children = El { tag = "option"; attrs; children }
let textarea ?(attrs = []) children = El { tag = "textarea"; attrs; children }
let output ?(attrs = []) children = El { tag = "output"; attrs; children }
let progress ?(attrs = []) children = El { tag = "progress"; attrs; children }
let meter ?(attrs = []) children = El { tag = "meter"; attrs; children }

(* Interactive *)
let details ?(attrs = []) children = El { tag = "details"; attrs; children }
let summary ?(attrs = []) children = El { tag = "summary"; attrs; children }
let dialog ?(attrs = []) children = El { tag = "dialog"; attrs; children }

(* Image and multimedia *)
let area ?(attrs = []) () = El { tag = "area"; attrs; children = [] }
let audio ?(attrs = []) children = El { tag = "audio"; attrs; children }
let img ?(attrs = []) () = El { tag = "img"; attrs; children = [] }
let map ?(attrs = []) children = El { tag = "map"; attrs; children }
let track ?(attrs = []) () = El { tag = "track"; attrs; children = [] }
let video ?(attrs = []) children = El { tag = "video"; attrs; children }

(* Embedded content *)
let embed ?(attrs = []) () = El { tag = "embed"; attrs; children = [] }
let iframe ?(attrs = []) children = El { tag = "iframe"; attrs; children }
let object_ ?(attrs = []) children = El { tag = "object"; attrs; children }
let picture ?(attrs = []) children = El { tag = "picture"; attrs; children }
let source ?(attrs = []) () = El { tag = "source"; attrs; children = [] }

(* Scripting *)
let canvas ?(attrs = []) children = El { tag = "canvas"; attrs; children }
let noscript ?(attrs = []) children = El { tag = "noscript"; attrs; children }

(* SVG and MathML *)
let svg ?(attrs = []) children = El { tag = "svg"; attrs; children }
let math ?(attrs = []) children = El { tag = "math"; attrs; children }

(** Content Helpers *)

let text str = Text str
let int n = Text (Int.to_string n)
let float f = Text (Float.to_string f)
let fragment children = Fragment children
let empty = Fragment []

(** Conditional Rendering *)

let when_ condition element = if condition then element else empty
let unless condition element = if not condition then element else empty
let maybe opt f = match opt with Some x -> f x | None -> empty

(** Rendering *)

(* Web Components *)
let slot ?(attrs = []) children = El { tag = "slot"; attrs; children }
let template ?(attrs = []) children = El { tag = "template"; attrs; children }

(** Rendering *)

(* Self-closing tags per HTML5 spec *)
let self_closing_tags = [
  "area"; "base"; "br"; "col"; "embed"; "hr"; "img"; "input";
  "link"; "meta"; "source"; "track"; "wbr";
]

let is_self_closing tag = List.mem tag self_closing_tags

let rec to_html t =
  match t with
  | Text str -> str
  | Fragment children -> String.concat "" (List.map to_html children)
  | El { tag; attrs; children } ->
      let attrs_str = attrs_to_string attrs in
      let attrs_part = if attrs_str = "" then "" else " " ^ attrs_str in
      
      if is_self_closing tag then
        "<" ^ tag ^ attrs_part ^ " />"
      else
        let children_html = String.concat "" (List.map to_html children) in
        "<" ^ tag ^ attrs_part ^ ">" ^ children_html ^ "</" ^ tag ^ ">"

and attrs_to_string attrs =
  attrs
  |> List.filter_map (function
      | Attr (k, v) -> Some (k ^ "=\"" ^ escape_attr v ^ "\"")
      | Event _ -> None)  (* Events ignored in static HTML *)
  |> String.concat " "

and escape_attr str =
  (* HTML attribute escaping *)
  let buf = IO.Buffer.create (String.length str) in
  String.iter (function
    | '"' -> IO.Buffer.add_string buf "&quot;"
    | '&' -> IO.Buffer.add_string buf "&amp;"
    | '<' -> IO.Buffer.add_string buf "&lt;"
    | '>' -> IO.Buffer.add_string buf "&gt;"
    | c -> IO.Buffer.add_char buf c
  ) str;
  IO.Buffer.contents buf

(** Advanced *)

let rec map f t =
  match t with
  | Text str -> Text str
  | Fragment children -> Fragment (List.map (map f) children)
  | El { tag; attrs; children } ->
      let attrs' = List.map (map_attr f) attrs in
      let children' = List.map (map f) children in
      El { tag; attrs = attrs'; children = children' }

and map_attr f attr =
  match attr with
  | Attr (k, v) -> Attr (k, v)
  | Event (name, handler) -> Event (name, fun ev -> f (handler ev))

let extract_handlers t =
  let rec go acc = function
    | Text _ -> acc
    | Fragment children -> List.fold_left go acc children
    | El { attrs; children; _ } ->
        let attr_handlers = List.filter_map (function
          | Event (name, handler) -> Some (name, handler)
          | Attr _ -> None
        ) attrs in
        List.fold_left go (attr_handlers @ acc) children
  in
  go [] t
