(**
   Type-safe HTML DSL for LiveView.

   Html provides a type-safe DSL for building HTML elements with event handlers.
   The DSL is parameterized by a message type, ensuring type-safe event handling
   throughout your component tree.

   ## Quick Start

   ```ocaml
   open Suri.LiveView.Html

   type msg = Increment | Decrement

   let view count =
     div ~id:"counter" [
       h1 [ string "Count: "; int count ];
       button ~on_click:(fun _ -> Increment) [ string "+" ] ();
       button ~on_click:(fun _ -> Decrement) [ string "-" ] ()
     ] ()
   ```

   ## Type Safety

   The HTML DSL is parameterized by your message type ('msg), which ensures:
   - Event handlers produce the correct message type
   - No runtime type errors in event handling
   - Compiler catches mismatched event handlers

   ## Event Handling

   Event handlers are functions from string to your message type:
   ```ocaml
   on_click : (string -> 'msg) -> 'msg attr
   ```

   The string parameter contains event data from the browser (usually empty for clicks).
   For form inputs, it would contain the input value.

   ## Rendering

   Convert your HTML tree to a string for sending to the client:
   ```ocaml
   let html = div [ h1 [ string "Hello" ] () ] ()
   let html_string = to_string html
   (* "<div><h1>Hello</h1></div>" *)
   ```
*)
type 'msg attr =
  | Attr of string * string
  (** Static HTML attribute like id, class, etc. *)
  | Event of string * (string -> 'msg)

(** Event handler that produces a message *)

(** HTML attribute - either a static attribute or an event handler *)
val attr: string -> string -> 'msg attr

(**
   Create a static attribute.

   Example:
   ```ocaml
   attr "class" "btn btn-primary"
   ```
*)
val attr_id: string -> 'msg attr

(**
   Create an id attribute.

   Example:
   ```ocaml
   attr_id "main-content"
   ```
*)
val attr_type: string -> 'msg attr

(**
   Create a type attribute for inputs/buttons.

   Example:
   ```ocaml
   attr_type "submit"
   ```
*)
val attr_src: string -> 'msg attr

(**
   Create a src attribute for images/scripts.

   Example:
   ```ocaml
   attr_src "/static/logo.png"
   ```
*)
type 'msg t =
  | El of {
      tag: string;
      attrs: 'msg attr list;
      children: 'msg t list;
    }
  (** HTML element with tag, attributes, and children *)
  | Text of string
  (** Text node *)
  | Splat of 'msg t list

(** List of elements to render inline *)

(** HTML tree - elements, text nodes, or lists of nodes *)
val list: 'msg t list -> 'msg t

(**
   Create a list of elements to render inline (no wrapper element).

   Useful for conditionally rendering multiple elements:
   ```ocaml
   let items = [
     div [ string "Item 1" ] ();
     div [ string "Item 2" ] ();
   ] in
   list items
   ```
*)
val button: on_click:'msg attr -> ?children:'msg t list -> unit -> 'msg t

(**
   Create a button element with click handler.

   Example:
   ```ocaml
   button ~on_click:(fun _ -> Submit) [ string "Submit" ] ()
   ```
*)
val html: ?children:'msg t list -> unit -> 'msg t

(** Create an <html> root element *)
val body: ?children:'msg t list -> unit -> 'msg t

(** Create a <body> element *)
val div: ?attrs:(string * string) list -> ?id:string -> ?children:'msg t list -> unit -> 'msg t

(**
   Create a <div> element.

   Example:
   ```ocaml
   div ~id:"main" ~attrs:[("class", "container")] [
     h1 [ string "Title" ] ()
   ] ()
   ```
*)
val h1: ?children:'msg t list -> unit -> 'msg t

(** Create an <h1> heading element *)
val h2: ?children:'msg t list -> unit -> 'msg t

(** Create an <h2> heading element *)
val h3: ?children:'msg t list -> unit -> 'msg t

(** Create an <h3> heading element *)
val h4: ?children:'msg t list -> unit -> 'msg t

(** Create an <h4> heading element *)
val h5: ?children:'msg t list -> unit -> 'msg t

(** Create an <h5> heading element *)
val h6: ?children:'msg t list -> unit -> 'msg t

(** Create an <h6> heading element *)
val span: ?children:'msg t list -> unit -> 'msg t

(** Create a <span> inline element *)
val p: ?children:'msg t list -> unit -> 'msg t

(** Create a <p> paragraph element *)
val script: ?src:string -> ?id:string -> ?type_:string -> ?children:'msg t list -> unit -> 'msg t

(**
   Create a <script> element for JavaScript.

   Example:
   ```ocaml
   script ~src:"/static/app.js" ~type_:"text/javascript" () ()
   ```
*)
val event: string -> (string -> 'msg) -> 'msg attr

(**
   Create an event handler attribute.

   The string parameter is the event name (e.g., "click", "input", "submit").

   Example:
   ```ocaml
   event "mouseover" (fun _ -> Hover)
   ```
*)
val on_click: (string -> 'msg) -> 'msg attr

(**
   Create a click event handler.

   Shorthand for `event "click" handler`.

   Example:
   ```ocaml
   button ~on_click:(fun _ -> Clicked) [ string "Click me" ] ()
   ```
*)
val string: string -> 'msg t

(**
   Create a text node from a string.

   Text nodes are HTML-escaped when rendered with `to_string`.

   Example:
   ```ocaml
   h1 [ string "Hello, world!" ] ()
   ```
*)
val int: int -> 'msg t

(**
   Create a text node from an integer.

   Example:
   ```ocaml
   div [ string "Count: "; int 42 ] ()
   ```
*)
val to_string: 'msg t -> string

(**
   Render an HTML tree to a string.

   This converts your HTML tree into an HTML string that can be sent to the client.
   Text and attribute values are escaped, while invalid dynamic tag and
   attribute names are omitted from output. `script` contents are trusted
   raw-text content.

   Example:
   ```ocaml
   let html = div [ h1 [ string "Title" ] () ] () in
   let html_str = to_string html
   (* "<div><h1>Title</h1></div>" *)
   ```
*)
val attrs_to_string: 'msg attr list -> string

(**
   Convert attributes to HTML attribute string (internal use).

   Example output: `id="main" class="container"`
*)
val event_handlers: 'msg attr list -> (string * (string -> 'msg)) list

(**
   Extract event handlers from attribute list (internal use).

   Used by the LiveView runtime to register event handlers.
*)
val map_action: ('msg_a -> 'msg_b) -> 'msg_a t -> 'msg_b t

(**
   Map event handlers to a different message type.

   Useful for composing components with different message types:
   ```ocaml
   type parent_msg = ChildMsg of child_msg | Other

   let child_view = ... (* returns child_msg Html.t *)
   let parent_view = Html.map_action (fun m -> ChildMsg m) child_view
   ```
*)
