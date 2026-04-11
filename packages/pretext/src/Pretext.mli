(** Tiny Unicode-aware pretty-printing documents. *)
type t

(** Empty document. *)
val empty: t

(** Literal text. *)
val str: string -> t

(** A literal space that never becomes a newline. *)
val space: t

(** [spaces n] emits [n] literal spaces. *)
val spaces: int -> t

(** A break that becomes a single space when the current group fits. *)
val brk: t

(** [break ?flat ()] emits [flat] when the current group fits, otherwise a newline. *)
val break: ?flat:string -> unit -> t

(** A break that disappears in flat mode and becomes a newline in broken mode. *)
val softline: t

(** A hard newline. *)
val line: t

(** Alias for [line]. *)
val hardline: t

(** Concatenate documents. *)
val concat: t list -> t

(** Alias for [concat]. *)
val of_list: t list -> t

(** Group a document list so its breaks flatten when it fits. *)
val group: t list -> t

(** Indent a document list by the given number of spaces after line breaks. *)
val nest: int -> t list -> t

(** Join documents with a separator. *)
val join: t -> t list -> t

(** Format a single document.

    The root document is implicitly grouped, so [brk] acts like a space when
    the full document fits within [width]. *)
val layout_doc: ?width:int -> t -> string

(** Format a document list.

    Equivalent to [layout_doc ~width (concat docs)]. *)
val layout: ?width:int -> t list -> string

(** Alias for [layout_doc]. *)
val format_doc: ?width:int -> t -> string

(** Alias for [layout]. *)
val format: ?width:int -> t list -> string
