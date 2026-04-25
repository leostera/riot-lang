(**
   {1 Suri.Component - Type-Safe HTML Component System}

   A unified component system for building HTML UIs that work seamlessly
   with both static HTML rendering and LiveView interactive applications.

   {2 Philosophy}

   - Write components once, render anywhere (static HTML or LiveView)
   - Event handlers are LiveView-only (ignored in static HTML)
   - Type-safe, composable, React-style component trees
   - No inline JavaScript - use LiveView for interactivity

   {2 Quick Start}

   {3 Static HTML}

   {[
     open Suri.Component

     let my_page =
       div ~attrs:[class_ "container"] [
         h1 [text "Welcome"];
         p [text "Build type-safe HTML with components!"];
         button ~attrs:[class_ "btn"] [text "Click me"]
       ]

     let html_string = to_html my_page
   ]}

   {3 LiveView (Interactive)}

   {[
     open Suri.Component

     type msg = Increment | Decrement

     let counter_view count =
       div [
         h1 [text "Counter"];
         div [text (Int.to_string count)];
         button ~attrs:[on_click (fun _ -> Increment)] [text "+"];
         button ~attrs:[on_click (fun _ -> Decrement)] [text "-"];
       ]

     (* Same tree works for static preview: *)
     let preview = to_html (counter_view 0)

     (* And for LiveView rendering: *)
     let render state = counter_view state.count
   ]}

   {2 Core Concepts}

   {3 Components are Trees}

   Every component is a tree of:
   - {b Elements} - 115+ HTML5 tags with attributes and children
   - {b Text} - String content
   - {b Fragments} - Lists of components without a wrapper

   {3 Attributes vs Events}

   - {b Attributes} (class, id, href, etc.) render in static HTML
   - {b Events} (on_click, on_submit, etc.) only work in LiveView
   - In static rendering, events are completely ignored

   {3 Design Systems}

   Build reusable component libraries:
   {[
     module MyDesign = struct
       let card ?(class_extra = "") children =
         div ~attrs:[
           class_ ("card " ^ class_extra);
           style_ "border: 1px solid #ccc; padding: 16px";
         ] children

       let button_primary children =
         button ~attrs:[class_ "btn btn-primary"] children
     end

     let my_page =
       MyDesign.card [
         h1 [text "Products"];
         MyDesign.button_primary [text "Buy Now"]
       ]
   ]}
*)
(** {1 Core Types} *)
type 'msg attr =
  | Attr of string * string
  | Event of string * (string -> 'msg)

(**
   HTML attribute or event handler.

   - ['msg] is the message type for event handlers (e.g., your app's action type)
   - Attributes (class, id, etc.) are rendered in static HTML
   - Events (on_click, etc.) are only active in LiveView
*)
type 'msg t =
  | El of { tag: string; attrs: 'msg attr list; children: 'msg t list }
  | Text of string
  | Fragment of 'msg t list

(** Component tree - can be an element, text, or fragment. *)
(** {1 Creating Attributes} *)
val attr: string -> string -> 'msg attr

(**
   Create any HTML attribute.

   Examples:
   {[
     attr "aria-label" "Close button"
     attr "data-user-id" "123"
   ]}
*)
val class_: string -> 'msg attr

(**
   CSS class attribute.

   Example: [class_ "btn btn-primary"]
*)
val style_: string -> 'msg attr

(**
   Inline style attribute.

   Example: [style_ "color: red; font-weight: bold"]
*)
val id: string -> 'msg attr

(**
   Element ID attribute.

   Example: [id "main-content"]
*)
val title_: string -> 'msg attr

(** Title attribute (tooltip text). *)
val name: string -> 'msg attr

(** Form input name attribute. *)
val value: string -> 'msg attr

(** Form input value attribute. *)
val placeholder: string -> 'msg attr

(** Form input placeholder text. *)
val type_: string -> 'msg attr

(**
   Input/button type attribute.

   Examples: [type_ "text"], [type_ "submit"]
*)
val href: string -> 'msg attr

(**
   Link href attribute.

   Example: [href "/products"]
*)
val src: string -> 'msg attr

(** Image/script source attribute. *)
val alt: string -> 'msg attr

(** Image alt text. *)
val target: string -> 'msg attr

(**
   Link target attribute.

   Example: [target "_blank"]
*)
val rel: string -> 'msg attr

(**
   Link relationship attribute.

   Example: [rel "noopener noreferrer"]
*)
val for_: string -> 'msg attr

(** Label for attribute (links label to input). *)
val action: string -> 'msg attr

(** Form action URL. *)
val method_: string -> 'msg attr

(**
   Form method attribute.

   Examples: [method_ "GET"], [method_ "POST"]
*)
val disabled: 'msg attr

(** Boolean disabled attribute. *)
val readonly: 'msg attr

(** Boolean readonly attribute. *)
val required: 'msg attr

(** Boolean required attribute. *)
val checked: 'msg attr

(** Boolean checked attribute (for checkboxes). *)
val selected: 'msg attr

(** Boolean selected attribute (for options). *)
val autofocus: 'msg attr

(** Boolean autofocus attribute. *)
val autocomplete: string -> 'msg attr

(**
   Autocomplete attribute.

   Examples: [autocomplete "off"], [autocomplete "email"]
*)
val data: string -> string -> 'msg attr

(**
   Data attribute.

   Example: [data "user-id" "123"] creates [data-user-id="123"]
*)
(**
   {1 Event Handlers}

   {b Note:} Events are {b LiveView only}. They are completely ignored
   when rendering static HTML with [to_html].
*)
val on: string -> (string -> 'msg) -> 'msg attr

(**
   Generic event handler.

   Example: [on "mouseover" (fun _ -> Hover)]
*)
val on_click: (string -> 'msg) -> 'msg attr

(** Click event handler. *)
val on_dblclick: (string -> 'msg) -> 'msg attr

(** Double-click event handler. *)
val on_submit: (string -> 'msg) -> 'msg attr

(** Form submit event handler. *)
val on_input: (string -> 'msg) -> 'msg attr

(** Input change event (fires on every keystroke). *)
val on_change: (string -> 'msg) -> 'msg attr

(** Change event (fires when input loses focus). *)
val on_focus: (string -> 'msg) -> 'msg attr

(** Focus event handler. *)
val on_blur: (string -> 'msg) -> 'msg attr

(** Blur (lost focus) event handler. *)
val on_keydown: (string -> 'msg) -> 'msg attr

(** Key down event handler. *)
val on_keyup: (string -> 'msg) -> 'msg attr

(** Key up event handler. *)
val on_keypress: (string -> 'msg) -> 'msg attr

(** Key press event handler. *)
val on_mouseenter: (string -> 'msg) -> 'msg attr

(** Mouse enter event handler. *)
val on_mouseleave: (string -> 'msg) -> 'msg attr

(** Mouse leave event handler. *)
val on_mouseover: (string -> 'msg) -> 'msg attr

(** Mouse over event handler. *)
val on_mouseout: (string -> 'msg) -> 'msg attr

(** Mouse out event handler. *)
(**
   {1 HTML Elements}

   All element constructors follow the pattern:
   {[
     element_name : ?attrs:'msg attr list -> 'msg t list -> 'msg t
   ]}

   Self-closing elements (like [input], [br], [img]) take [unit] instead
   of children and return ['msg t].
*)
(** {2 Document Structure} *)
val html: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Root HTML element *)
val head: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Document head (metadata container) *)
val body: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Document body (content container) *)
val title: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Document title (shown in browser tab) *)
val base: ?attrs:'msg attr list -> unit -> 'msg t

(** Base URL for relative URLs in document *)
val meta: ?attrs:'msg attr list -> unit -> 'msg t

(** Metadata (charset, viewport, etc.) *)
val link: ?attrs:'msg attr list -> unit -> 'msg t

(** External resource link (CSS, favicon, etc.) *)
val style: ?attrs:'msg attr list -> string -> 'msg t

(** Inline CSS styles *)
val script: ?attrs:'msg attr list -> string -> 'msg t

(** Inline or external JavaScript *)
(** {2 Content Sectioning} *)
val header: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Introductory content or navigation *)
val nav: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Navigation links section *)
val main: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Main content of document *)
val section: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Generic standalone section *)
val article: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Self-contained composition (blog post, article, etc.) *)
val aside: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Sidebar or tangentially related content *)
val footer: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Footer for section or page *)
val address: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Contact information *)
val hgroup: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Heading group with subheadings *)
val search: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Search or filtering section *)
(** {2 Text Content} *)
val div: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Generic container (block-level) *)
val p: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Paragraph *)
val span: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Generic container (inline) *)
val h1: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Heading level 1 (highest) *)
val h2: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Heading level 2 *)
val h3: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Heading level 3 *)
val h4: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Heading level 4 *)
val h5: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Heading level 5 *)
val h6: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Heading level 6 (lowest) *)
val pre: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Preformatted text (preserves whitespace) *)
val blockquote: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Extended quotation *)
val hr: ?attrs:'msg attr list -> unit -> 'msg t

(** Horizontal rule (thematic break) *)
val br: unit -> 'msg t

(** Line break *)
val figure: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Self-contained content with optional caption *)
val figcaption: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Caption for figure element *)
val menu: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Unordered list of items (semantic alternative to ul) *)
(** {2 Inline Text Semantics} *)
val a: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Hyperlink *)
val abbr: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Abbreviation or acronym *)
val b: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Bring attention to (stylistically offset) *)
val bdi: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Bidirectional isolate *)
val bdo: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Bidirectional text override *)
val cite: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Title of creative work *)
val code: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Inline code fragment *)
val data: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Machine-readable data *)
val dfn: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Term being defined *)
val em: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Emphasis *)
val i: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Idiomatic text (technical terms, etc.) *)
val kbd: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Keyboard input *)
val mark: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Highlighted/marked text *)
val q: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Inline quotation *)
val rp: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Ruby fallback parentheses *)
val rt: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Ruby text annotation *)
val ruby: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Ruby annotation (pronunciation, translation) *)
val s: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Strikethrough *)
val samp: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Sample output *)
val small: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Side comment, fine print *)
val strong: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Strong importance *)
val sub: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Subscript *)
val sup: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Superscript *)
val time: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Time or date *)
val u: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Unarticulated annotation (underline) *)
val var: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Variable name *)
val wbr: ?attrs:'msg attr list -> unit -> 'msg t

(** Word break opportunity *)
val del: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Deleted text *)
val ins: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Inserted text *)
(** {2 Lists} *)
val ul: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val ol: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val li: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val dl: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val dt: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val dd: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** {2 Tables} *)
val table: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val caption: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val thead: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val tbody: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val tfoot: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val tr: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val th: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val td: ?attrs:'msg attr list -> 'msg t list -> 'msg t

val col: ?attrs:'msg attr list -> unit -> 'msg t

val colgroup: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** {2 Forms} *)
val form: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Form for user input *)
val fieldset: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Group of form controls *)
val legend: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Caption for fieldset *)
val label: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Label for form control *)
val input: ?attrs:'msg attr list -> unit -> 'msg t

(** Input control (text, checkbox, radio, etc.) *)
val button: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Button (submit, reset, or custom action) *)
val select: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Dropdown selection control *)
val datalist: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Predefined options for input *)
val optgroup: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Group of options in select *)
val option: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Option in select or datalist *)
val textarea: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Multi-line text input *)
val output: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Result of calculation or user action *)
val progress: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Progress indicator *)
val meter: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Scalar measurement within range *)
(** {2 Interactive Elements} *)
val details: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Disclosure widget (expandable/collapsible) *)
val summary: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Summary/legend for details element *)
val dialog: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Dialog box or modal *)
(** {2 Image and Multimedia} *)
val area: ?attrs:'msg attr list -> unit -> 'msg t

(** Clickable area in image map *)
val audio: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Audio content *)
val img: ?attrs:'msg attr list -> unit -> 'msg t

(** Image *)
val map: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Image map (with area elements) *)
val track: ?attrs:'msg attr list -> unit -> 'msg t

(** Text tracks for media (subtitles, captions) *)
val video: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Video content *)
(** {2 Embedded Content} *)
val embed: ?attrs:'msg attr list -> unit -> 'msg t

(** External content (plugin) *)
val iframe: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Nested browsing context *)
val object_: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** External resource *)
val picture: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Container for multiple image sources *)
val source: ?attrs:'msg attr list -> unit -> 'msg t

(** Media resource for picture/audio/video *)
(** {2 Scripting} *)
val canvas: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Graphics canvas (2D/WebGL) *)
val noscript: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Fallback for disabled JavaScript *)
(** {2 SVG and MathML} *)
val svg: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** SVG graphics container *)
val math: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** MathML mathematical notation *)
(** {2 Web Components} *)
val slot: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** Web component placeholder *)
val template: ?attrs:'msg attr list -> 'msg t list -> 'msg t

(** HTML template (not rendered initially) *)
(** {2 Generic Element Builder} *)
val el: string -> ?attrs:'msg attr list -> 'msg t list -> 'msg t

(**
   Create any HTML element by tag name.

   Useful for custom elements or elements not yet covered.

   Example:
   {[
     el "custom-widget" ~attrs:[class_ "widget"] [
       text "Custom content"
     ]
   ]}
*)
(** {1 Content Helpers} *)
val text: string -> 'msg t

(**
   Create a text node.

   Example: [text "Hello, world!"]
*)
val int: int -> 'msg t

(**
   Create a text node from an integer.

   Example: [int 42]
*)
val float: float -> 'msg t

(**
   Create a text node from a float.

   Example: [float 3.14]
*)
val fragment: 'msg t list -> 'msg t

(**
   Group elements without a wrapper.

   Useful for conditional rendering or loops:
   {[
     fragment [
       h1 [text "Title"];
       p [text "Paragraph"];
     ]
   ]}
*)
val empty: 'msg t

(**
   Empty content (renders nothing).

   Useful for conditional rendering:
   {[
     if show_message then
       p [text "Message"]
     else
       empty
   ]}
*)
(** {1 Conditional Rendering} *)
val when_: bool -> 'msg t -> 'msg t

(**
   Render element only if condition is true.

   Example:
   {[
     when_ (count > 0) (
       p [text "You have items"]
     )
   ]}
*)
val unless: bool -> 'msg t -> 'msg t

(**
   Render element only if condition is false.

   Example:
   {[
     unless is_loading (
       div [text "Content loaded"]
     )
   ]}
*)
val maybe: 'a option -> ('a -> 'msg t) -> 'msg t

(**
   Render element if option is Some.

   Example:
   {[
     maybe user (fun u ->
       div [
         text "Welcome, ";
         text u.name;
       ]
     )
   ]}
*)
(** {1 Rendering} *)
val to_html: 'msg t -> string

(**
   Render component tree to HTML string.

   - Event handlers are ignored (LiveView only)
   - Self-closing tags are properly formatted
   - Attributes are HTML-escaped

   Example:
   {[
     let page = div ~attrs:[class_ "container"] [
       h1 [text "Hello"];
       button ~attrs:[on_click (fun _ -> Click)] [text "Click"]
     ] in
     to_html page
     (* Output: <div class="container"><h1>Hello</h1><button>Click</button></div> *)
     (* Note: on_click is ignored in static HTML *)
   ]}
*)
(** {1 Advanced} *)
val map: ('a -> 'b) -> 'a t -> 'b t

(**
   Transform event handlers from one message type to another.

   Essential for component composition with nested message types.

   Example:
   {[
     type parent_msg = ChildClicked of child_msg | Other
     type child_msg = Increment | Decrement

     let child_component = (* returns child_msg t *)
       button ~attrs:[on_click (fun _ -> Increment)] [text "+"]

     let parent_component =
       div [
         map (fun msg -> ChildClicked msg) child_component
       ]
   ]}
*)
val extract_handlers: 'msg t -> (string * (string -> 'msg)) list(**
   Extract all event handlers from a component tree.

   Used internally by LiveView runtime to register event handlers.
   Most users don't need this function.
*)
