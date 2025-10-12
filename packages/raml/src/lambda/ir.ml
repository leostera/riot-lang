open Std
module Identifier = Typechecker.Identifier
module Location = Typechecker.Location

(** {1 Lambda Intermediate Representation}

    Lambda is the intermediate representation (IR) used in RAML after type
    checking.

    {b For beginners:} Think of Lambda as a simpler version of OCaml with:
    - Explicit function calls
    - Simplified pattern matching (compiled to switches)
    - Primitive operations made explicit
    - No modules or complex features

    {b Why Lambda IR?}
    - Easier to optimize than the full typed AST
    - Closer to machine code but still high-level
    - Standard in functional compilers (GHC, MLton, etc. all use similar IRs)
    - Clear separation between frontend (typing) and backend (code gen)

    {b Example transformation:}
    {[
      (* OCaml: *)
      let x = 1 + 2 in
      x
      * 3
          (* Lambda: *)
          Llet
          {
            id = x;
            value = Lprim (Pint_add, [ Lconst (Int 1); Lconst (Int 2) ]);
            body = Lprim (Pint_mul, [ Lvar x; Lconst (Int 3) ]);
          }
    ]} *)

(** {2 Constants} *)

type structured_constant =
  | Const_int of int  (** Integer constant. Example: [42] *)
  | Const_string of string  (** String constant. Example: ["hello"] *)
  | Const_float of float  (** Float constant. Example: [3.14] *)
  | Const_block of int * structured_constant list
      (** Block with tag and fields. Used for tuples, records, variants.
          Example: [Const_block (0, [Const_int 1; Const_int 2])] for tuple
          [(1, 2)] *)

(** {2 Primitive Operations}

    Primitives are built-in operations that the compiler knows how to implement
    efficiently. Each primitive has a fixed meaning and can be compiled directly
    to machine instructions. *)

type primitive =
  (* {3 Integer Arithmetic} *)
  | Pint_add  (** Integer addition: [a + b] *)
  | Pint_sub  (** Integer subtraction: [a - b] *)
  | Pint_mul  (** Integer multiplication: [a * b] *)
  | Pint_div  (** Integer division: [a / b] *)
  | Pint_mod  (** Integer modulo: [a mod b] *)
  | Pint_neg  (** Integer negation: [-a] *)
  (* {3 Integer Comparisons} *)
  | Pint_lt  (** Less than: [a < b] *)
  | Pint_le  (** Less than or equal: [a <= b] *)
  | Pint_gt  (** Greater than: [a > b] *)
  | Pint_ge  (** Greater than or equal: [a >= b] *)
  | Pint_eq  (** Equal: [a = b] *)
  | Pint_ne  (** Not equal: [a <> b] *)
  (* {3 Memory Operations} *)
  | Pmakeblock of int
      (** Create a heap-allocated block with given tag. Example: [Pmakeblock 0]
          creates a tuple/record.

          {b Tags:}
          - 0 = tuple or first variant constructor
          - 1+ = other variant constructors
          - 248 = lazy value
          - 249 = closure
          - 250 = object *)
  | Pfield of int
      (** Get field from block: [block.(n)] Example: [Pfield 0] gets first field
          of tuple *)
  | Psetfield of int
      (** Set field in block: [block.(n) <- value] Used for mutable records *)
  (* {3 Boolean Operations} *)
  | Pnot  (** Boolean negation: [not b] *)
  (* {4 Array Operations} *)
  | Pmakearray  (** Create array from list of values *)
  | Parraylength  (** Get array length *)
  | Parrayrefu  (** Unsafe array get: [arr.(i)] without bounds check *)
  | Parraysetu  (** Unsafe array set: [arr.(i) <- v] without bounds check *)

let primitive_to_string = function
  | Pint_add -> "+"
  | Pint_sub -> "-"
  | Pint_mul -> "*"
  | Pint_div -> "/"
  | Pint_mod -> "mod"
  | Pint_neg -> "~-"
  | Pint_lt -> "<"
  | Pint_le -> "<="
  | Pint_gt -> ">"
  | Pint_ge -> ">="
  | Pint_eq -> "="
  | Pint_ne -> "<>"
  | Pmakeblock tag -> format "makeblock[%d]" tag
  | Pfield n -> format "field[%d]" n
  | Psetfield n -> format "setfield[%d]" n
  | Pnot -> "not"
  | Pmakearray -> "makearray"
  | Parraylength -> "array.length"
  | Parrayrefu -> "array.unsafe_get"
  | Parraysetu -> "array.unsafe_set"

(** {2 Lambda Expressions}

    The core Lambda IR. Every OCaml expression gets translated to one of these
    forms. *)

type lambda =
  | Var of Identifier.t  (** Variable reference. Example: [x] → [Lvar x_id] *)
  | Const of structured_constant
      (** Constant value. Example: [42] → [Lconst (Const_int 42)] *)
  | Apply of { func : lambda; args : lambda list; loc : Location.t option }
      (** Function application.

          Example: [f x y] → [Lapply { func = Lvar f; args = [Lvar x; Lvar y] }]

          {b Note:} Multi-argument application is direct, not curried at Lambda
          level. The translation from TypedTree handles currying. *)
  | Function of {
      params : Identifier.t list;
      body : lambda;
      loc : Location.t option;
    }
      (** Function definition (lambda abstraction).

          Example: [fun x y -> x + y] →
          {[
            Lfunction
              { params = [ x; y ]; body = Lprim (Pint_add, [ Lvar x; Lvar y ]) }
          ]}

          {b Note:} Multi-parameter functions are direct, not curried. *)
  | Let of {
      id : Identifier.t;
      value : lambda;
      body : lambda;
      loc : Location.t option;
    }
      (** Let binding (non-recursive).

          Example: [let x = 42 in x + 1] →
          {[
            Llet
              {
                id = x;
                value = Lconst (Const_int 42);
                body = Lprim (Pint_add, [ Lvar x; Lconst (Const_int 1) ]);
              }
          ]} *)
  | LetRec of {
      bindings : (Identifier.t * lambda) list;
      body : lambda;
      loc : Location.t option;
    }
      (** Recursive let binding(s).

          Example: [let rec f x = f x in f 0] →
          {[
            Lletrec {
              bindings = [(f, Lfunction { params = [x]; body = Lapply {...} })];
              body = Lapply { func = Lvar f; args = [Lconst (Const_int 0)] }
            }
          ]} *)
  | Prim of primitive * lambda list
      (** Primitive operation application.

          Example: [x + 1] →
          {[
            Lprim (Pint_add, [ Lvar x; Lconst (Const_int 1) ])
          ]} *)
  | IfThenElse of lambda * lambda * lambda option
      (** Conditional expression.

          Example: [if x > 0 then 1 else -1] →
          {[
            Lifthenelse
              ( Lprim (Pint_gt, [ Lvar x; Lconst (Const_int 0) ]),
                Lconst (Const_int 1),
                Some (Lprim (Pint_neg, [ Lconst (Const_int 1) ])) )
          ]}

          {b Note:} If else branch is None, it's implicitly unit. *)
  | Sequence of lambda * lambda
      (** Sequential execution: evaluate first, then second, return second.

          Example: [print "hi"; 42] →
          {[
            Lsequence
              ( Lapply
                  { func = Lvar print; args = [ Lconst (Const_string "hi") ] },
                Lconst (Const_int 42) )
          ]} *)
  | While of { condition : lambda; body : lambda; loc : Location.t option }
      (** While loop.

          Example: [while !x > 0 do x := !x - 1 done] *)
  | For of {
      id : Identifier.t;
      start : lambda;
      stop : lambda;
      direction : direction;
      body : lambda;
      loc : Location.t option;
    }
      (** For loop.

          Example: [for i = 0 to 10 do print i done] *)
  | Switch of {
      scrutinee : lambda;
      cases : (int * lambda) list;
      default : lambda option;
      loc : Location.t option;
    }
      (** Switch on integer (compiled pattern match).

          {b For beginners:} Pattern matching like:
          {[
            match x with None -> 0 | Some y -> y
          ]}

          Gets compiled to:
          {[
            Lswitch {
              scrutinee = Lfield 0 (Lvar x);  (* get tag *)
              cases = [(0, Lconst 0);         (* None case *)
                       (1, Lfield 1 (Lvar x))]; (* Some case - get value *)
              default = None
            }
          ]}

          {b Why switch?} Much simpler than patterns, easy to compile to
          assembly. *)
  | StaticRaise of int * lambda list
      (** Static exception raise (for pattern matching).

          {b Advanced:} Used internally by pattern match compiler. Not directly
          visible in source code. *)
  | StaticCatch of lambda * (int * Identifier.t list) * lambda
      (** Static exception handler (for pattern matching).

          {b Advanced:} Catches static raises. Used for pattern match
          compilation. *)

and direction =
  | Upto
  | Downto  (** Direction for for-loops: [to] vs [downto] *)

(** {2 Pretty Printing}

    Convert Lambda IR back to readable text (for debugging). *)

let rec lambda_to_string = function
  | Var id -> Identifier.name id
  | Const c -> const_to_string c
  | Apply { func; args; _ } ->
      format "(%s %s)" (lambda_to_string func)
        (String.concat " " (List.map lambda_to_string args))
  | Function { params; body; _ } ->
      format "(fun %s -> %s)"
        (String.concat " " (List.map Identifier.name params))
        (lambda_to_string body)
  | Let { id; value; body; _ } ->
      format "(let %s = %s in %s)" (Identifier.name id) (lambda_to_string value)
        (lambda_to_string body)
  | LetRec { bindings; body; _ } ->
      let bindings_str =
        bindings
        |> List.map (fun (id, value) ->
            format "%s = %s" (Identifier.name id) (lambda_to_string value))
        |> String.concat " and "
      in
      format "(let rec %s in %s)" bindings_str (lambda_to_string body)
  | Prim (prim, args) ->
      format "(%s %s)" (primitive_to_string prim)
        (String.concat " " (List.map lambda_to_string args))
  | IfThenElse (cond, then_, else_) -> (
      match else_ with
      | None ->
          format "(if %s then %s)" (lambda_to_string cond)
            (lambda_to_string then_)
      | Some else_ ->
          format "(if %s then %s else %s)" (lambda_to_string cond)
            (lambda_to_string then_) (lambda_to_string else_))
  | Sequence (e1, e2) ->
      format "(%s; %s)" (lambda_to_string e1) (lambda_to_string e2)
  | While { condition; body; _ } ->
      format "(while %s do %s)"
        (lambda_to_string condition)
        (lambda_to_string body)
  | For { id; start; stop; direction; body; _ } ->
      let dir = match direction with Upto -> "to" | Downto -> "downto" in
      format "(for %s = %s %s %s do %s)" (Identifier.name id)
        (lambda_to_string start) dir (lambda_to_string stop)
        (lambda_to_string body)
  | Switch { scrutinee; cases; default; _ } ->
      let cases_str =
        cases
        |> List.map (fun (tag, body) ->
            format "%d -> %s" tag (lambda_to_string body))
        |> String.concat " | "
      in
      let default_str =
        match default with
        | None -> ""
        | Some body -> format " | _ -> %s" (lambda_to_string body)
      in
      format "(switch %s with %s%s)"
        (lambda_to_string scrutinee)
        cases_str default_str
  | StaticRaise (id, args) ->
      format "(raise#%d %s)" id
        (String.concat " " (List.map lambda_to_string args))
  | StaticCatch (body, (id, params), handler) ->
      format "(catch %s with #%d(%s) -> %s)" (lambda_to_string body) id
        (String.concat ", " (List.map Identifier.name params))
        (lambda_to_string handler)

and const_to_string = function
  | Const_int n -> string_of_int n
  | Const_string s -> format "\"%s\"" s
  | Const_float f -> string_of_float f
  | Const_block (tag, fields) ->
      format "[%d: %s]" tag
        (String.concat ", " (List.map const_to_string fields))

(** {2 JSON Serialization} *)

let rec const_to_json = function
  | Const_int n ->
      Data.Json.obj
        [ ("type", Data.Json.string "int"); ("value", Data.Json.int n) ]
  | Const_string s ->
      Data.Json.obj
        [ ("type", Data.Json.string "string"); ("value", Data.Json.string s) ]
  | Const_float f ->
      Data.Json.obj
        [ ("type", Data.Json.string "float"); ("value", Data.Json.float f) ]
  | Const_block (tag, fields) ->
      Data.Json.obj
        [
          ("type", Data.Json.string "block");
          ("tag", Data.Json.int tag);
          ("fields", Data.Json.array (List.map const_to_json fields));
        ]

let rec lambda_to_json = function
  | Var id ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lvar");
          ("id", Data.Json.string (Identifier.name id));
        ]
  | Const c ->
      Data.Json.obj
        [ ("type", Data.Json.string "Lconst"); ("value", const_to_json c) ]
  | Apply { func; args; _ } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lapply");
          ("func", lambda_to_json func);
          ("args", Data.Json.array (List.map lambda_to_json args));
        ]
  | Function { params; body; _ } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lfunction");
          ( "params",
            Data.Json.array
              (List.map
                 (fun id -> Data.Json.string (Identifier.name id))
                 params) );
          ("body", lambda_to_json body);
        ]
  | Let { id; value; body; _ } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Llet");
          ("id", Data.Json.string (Identifier.name id));
          ("value", lambda_to_json value);
          ("body", lambda_to_json body);
        ]
  | LetRec { bindings; body; _ } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lletrec");
          ( "bindings",
            Data.Json.array
              (List.map
                 (fun (id, value) ->
                   Data.Json.obj
                     [
                       ("id", Data.Json.string (Identifier.name id));
                       ("value", lambda_to_json value);
                     ])
                 bindings) );
          ("body", lambda_to_json body);
        ]
  | Prim (prim, args) ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lprim");
          ("primitive", Data.Json.string (primitive_to_string prim));
          ("args", Data.Json.array (List.map lambda_to_json args));
        ]
  | IfThenElse (cond, then_, else_) ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lifthenelse");
          ("condition", lambda_to_json cond);
          ("then", lambda_to_json then_);
          ( "else",
            match else_ with
            | None -> Data.Json.null
            | Some e -> lambda_to_json e );
        ]
  | Sequence (e1, e2) ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lsequence");
          ("first", lambda_to_json e1);
          ("second", lambda_to_json e2);
        ]
  | While { condition; body; _ } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lwhile");
          ("condition", lambda_to_json condition);
          ("body", lambda_to_json body);
        ]
  | For { id; start; stop; direction; body; _ } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lfor");
          ("id", Data.Json.string (Identifier.name id));
          ("start", lambda_to_json start);
          ("stop", lambda_to_json stop);
          ( "direction",
            Data.Json.string
              (match direction with Upto -> "to" | Downto -> "downto") );
          ("body", lambda_to_json body);
        ]
  | Switch { scrutinee; cases; default; _ } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lswitch");
          ("scrutinee", lambda_to_json scrutinee);
          ( "cases",
            Data.Json.array
              (List.map
                 (fun (tag, body) ->
                   Data.Json.obj
                     [
                       ("tag", Data.Json.int tag); ("body", lambda_to_json body);
                     ])
                 cases) );
          ( "default",
            match default with
            | None -> Data.Json.null
            | Some body -> lambda_to_json body );
        ]
  | StaticRaise (id, args) ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lstaticraise");
          ("id", Data.Json.int id);
          ("args", Data.Json.array (List.map lambda_to_json args));
        ]
  | StaticCatch (body, (id, params), handler) ->
      Data.Json.obj
        [
          ("type", Data.Json.string "Lstaticcatch");
          ("body", lambda_to_json body);
          ("exception_id", Data.Json.int id);
          ( "params",
            Data.Json.array
              (List.map (fun p -> Data.Json.string (Identifier.name p)) params)
          );
          ("handler", lambda_to_json handler);
        ]
