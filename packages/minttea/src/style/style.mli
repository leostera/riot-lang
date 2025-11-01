(** Layout composition utilities *)
module Layout : sig
  val join_horizontal : pos:[`Top | `Center | `Bottom] -> string list -> string
  (** Place strings side-by-side horizontally *)

  val join_vertical : pos:[`Left | `Center | `Right] -> string list -> string
  (** Stack strings vertically *)

  val place : 
    width:int -> 
    height:int -> 
    h_pos:float -> 
    v_pos:float -> 
    string -> 
    string
  (** Position string within a box using fractional coordinates *)
end


type color = Tty.Color.t = private
  | RGB of int * int * int
  | ANSI of int
  | ANSI256 of int
  | No_color

val color : ?profile:Tty.Profile.t -> string -> color
val gradient : start:color -> finish:color -> steps:int -> color array

module Border : sig
  type t

  val make :
    ?top:string ->
    ?left:string ->
    ?bottom:string ->
    ?right:string ->
    ?top_left:string ->
    ?top_right:string ->
    ?bottom_left:string ->
    ?bottom_right:string ->
    ?middle_left:string ->
    ?middle_right:string ->
    ?middle:string ->
    ?middle_top:string ->
    ?middle_bottom:string ->
    unit ->
    t

  val normal : t
  val rounded : t
  val block : t
  val outer_half_block : t
  val inner_half_block : t
  val thick : t
  val double : t
  val hidden : t
end

type style

val default : style
val bg : color -> style -> style
val blink : bool -> style -> style
val bold : bool -> style -> style
val faint : bool -> style -> style
val fg : color -> style -> style
val height : int -> style -> style
val italic : bool -> style -> style
val margin_bottom : int -> style -> style
val margin_left : int -> style -> style
val margin_right : int -> style -> style
val margin_top : int -> style -> style
val max_height : int -> style -> style
val max_width : int -> style -> style
val padding_bottom : int -> style -> style
val padding_left : int -> style -> style
val padding_right : int -> style -> style
val padding_top : int -> style -> style
val reverse : bool -> style -> style
val strikethrough : bool -> style -> style
val underline : bool -> style -> style
val width : int option -> style -> style
val border : Border.t -> style -> style

val align_horizontal : [`Left | `Center | `Right] -> style -> style
(** [align_horizontal pos style] sets horizontal text alignment.
    
    Only applies when [width] is set. Text will be padded to reach the target width.
    
    - [`Left] - Align left, pad right
    - [`Center] - Center text
    - [`Right] - Align right, pad left
    
    Example:
    ```ocaml
    let style = default 
      |> width (Some 20) 
      |> align_horizontal `Center
      |> fg (color "cyan") in
    render style "Hello"
    (* Renders:      Hello       *)
    ``` *)

val align_vertical : [`Top | `Center | `Bottom] -> style -> style
(** `align_vertical pos style` sets vertical text alignment.
    
    Only applies when `height` is set. Content will be padded with empty lines.
    
    - `` `Top`` - Align to top, pad bottom
    - `` `Center`` - Center content vertically
    - `` `Bottom`` - Align to bottom, pad top
    
    Example:
    ```ocaml
    let style = default 
      |> height 5 
      |> align_vertical `Center
      |> border Border.rounded in
    render style "Middle"
    (* Renders centered in 5-line box *)
    ``` *)

val render : style -> string -> string
(** `render style text` applies the style to text and returns formatted string.
    
    Processing order: padding, horizontal alignment, vertical alignment,
    text formatting (colors, bold, etc), borders, margins, max constraints
    
    Example:
    ```ocaml
    let styled = default
      |> fg (color "green")
      |> bg (color "black")
      |> bold true
      |> padding_left 2
      |> padding_right 2
      |> border Border.rounded
      |> render in
    styled "Hello World"
    ``` *)
