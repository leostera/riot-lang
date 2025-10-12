open Std

(** {1 Jambda IR - JavaScript Lambda}

    Jambda is the JavaScript-aware intermediate representation between Lambda IR
    and JavaScript output.

    Pipeline: TypedTree → Lambda → Jambda → JsTree → JS

    Key transformations in Jambda:
    - Uncurrying optimization (f(a)(b) → f(a,b))
    - Runtime representation decisions (variants, records, etc.)
    - JS-specific primitives (array access, object operations)
    - Module system mapping (OCaml modules → JS modules)

    Jambda is still functional and high-level, but aware of how things will be
    represented in JavaScript. *)

module Identifier = Typechecker.Identifier
module Location = Typechecker.Location

(** {2 Runtime Representations}

    How OCaml values are represented in JavaScript. *)

type runtime_tag =
  | TagInt  (** OCaml int → JS number (no boxing needed) *)
  | TagFloat  (** OCaml float → JS number *)
  | TagString  (** OCaml string → JS string *)
  | TagBool  (** OCaml bool → JS number (0/1) or boolean (optimization) *)
  | TagUnit  (** OCaml unit → JS undefined *)
  | TagVariant of int  (** OCaml variant → JS object with TAG field *)
  | TagRecord  (** OCaml record → JS object *)
  | TagArray  (** OCaml array → JS array *)
  | TagTuple  (** OCaml tuple → JS array *)
  | TagClosure  (** OCaml closure → JS function (with captured env) *)

(** {2 Jambda Primitives}

    Operations that map directly to JavaScript primitives. *)

type jambda_primitive =
  (* Arithmetic - direct JS operators *)
  | Jadd
  | Jsub
  | Jmul
  | Jdiv
  | Jmod
  (* Comparisons - JS operators with correct semantics *)
  | Jeq
  | Jneq
  | Jlt
  | Jle
  | Jgt
  | Jge
  | Jstricteq
  | Jstrictneq (* === and !== *)
  (* Boolean *)
  | Jnot
  | Jand
  | Jor
  (* Array operations *)
  | Jarray_get (* arr[i] *)
  | Jarray_set (* arr[i] = x *)
  | Jarray_length (* arr.length *)
  | Jarray_make (* new Array(n) *)
  (* Object operations *)
  | Jobject_get of string (* obj.field *)
  | Jobject_set of string (* obj.field = x *)
  | Jobject_make of string list (* { field1, field2, ... } *)
  (* Variant operations *)
  | Jmake_variant of int * int (* { TAG: n, _0: x, _1: y } *)
  | Jvariant_tag (* x.TAG *)
  | Jvariant_field of int (* x._0, x._1, etc *)
  (* Function application *)
  | Japply of int (* f(a, b, c) - arity known *)
  | Japply_method of string (* obj.method(args) *)
  (* Type conversions *)
  | Jto_bool (* Convert to JS boolean *)
  | Jfrom_bool (* Convert from JS boolean *)

(** {2 Jambda Expressions} *)

type jambda =
  | Jvar of Identifier.t  (** Variable reference *)
  | Jconst of jambda_constant  (** Constant value *)
  | Jfunction of {
      arity : int;  (** Function arity (for uncurrying optimization) *)
      params : Identifier.t list;
      body : jambda;
      loc : Location.t option;
    }  (** Function definition - arity known for optimization *)
  | Japply_uncurried of {
      func : jambda;
      args : jambda list;  (** All arguments provided at once (uncurried) *)
      loc : Location.t option;
    }  (** Uncurried function application: f(a, b, c) *)
  | Japply_curried of {
      func : jambda;
      arg : jambda;  (** Single argument (partial application possible) *)
      loc : Location.t option;
    }  (** Curried function application: f(a) - may return closure *)
  | Jlet of {
      id : Identifier.t;
      value : jambda;
      body : jambda;
      loc : Location.t option;
    }  (** Let binding *)
  | Jletrec of {
      bindings : (Identifier.t * jambda) list;
      body : jambda;
      loc : Location.t option;
    }  (** Recursive let bindings *)
  | Jprim of {
      op : jambda_primitive;
      args : jambda list;
      loc : Location.t option;
    }  (** Primitive operation *)
  | Jifthenelse of jambda * jambda * jambda option  (** Conditional *)
  | Jsequence of jambda * jambda  (** Sequential execution *)
  | Jswitch of {
      scrutinee : jambda;
      cases : (int * jambda) list;  (** Variant tag → branch *)
      default : jambda option;
      loc : Location.t option;
    }  (** Switch on variant tag (becomes JS switch) *)
  | Jmake_tuple of jambda list  (** Tuple creation → JS array *)
  | Jmake_record of (string * jambda) list  (** Record creation → JS object *)
  | Jmake_array of jambda list  (** Array creation → JS array *)
  | Jmake_variant of { tag : int; args : jambda list; loc : Location.t option }
      (** Variant construction → JS object with TAG *)

(** {2 Jambda Constants} *)

and jambda_constant =
  | Jconst_int of int
  | Jconst_float of float
  | Jconst_string of string
  | Jconst_bool of bool
  | Jconst_unit

(** {2 Jambda Modules}

    Modules preserve structure for JS module systems. *)

type jambda_export =
  | JExport_value of Identifier.t * jambda  (** Exported value *)
  | JExport_function of Identifier.t * jambda  (** Exported function *)

type jambda_module = {
  name : string;  (** Module name *)
  imports : (string * Identifier.t list) list;
      (** Imported modules: (module_path, [exported_names]) *)
  exports : jambda_export list;  (** Exported definitions *)
  body : jambda;  (** Module body (initialization code) *)
}

(** {2 Translation from Lambda IR} *)

val translate_from_lambda : Lambda.Ir.lambda -> jambda
(** Translate Lambda IR to Jambda IR.

    Key transformations:
    - Detect curried vs uncurried applications
    - Choose runtime representations for data types
    - Map Lambda primitives to Jambda primitives *)

val translate_module_from_lambda : string -> Lambda.Ir.lambda -> jambda_module
(** Translate a Lambda IR module to Jambda module *)

(** {2 Pretty Printing} *)

val jambda_to_string : jambda -> string
val jambda_module_to_string : jambda_module -> string
