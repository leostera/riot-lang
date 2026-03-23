open Std

(** Lossless concrete syntax tree (CST) nodes produced from successful `syn`
    parses.

    This module is intentionally grammar-oriented. Each lifted value keeps a
    handle to the original `syntax_node`, and many nodes also preserve the exact
    `Token.t` values that spelled the construct. That makes the CST suitable for
    refactoring tools, diagnostics, formatting, and fixture-based tests that
    need structured syntax without giving up source fidelity.

    The main families of grammar covered here are:

    - `pattern` for pattern syntax such as `Some x`, `{ field }`, or
      `effect Yield k`
    - `expression` for expression syntax such as `f x`, `match x with`, or
      `let module M = N in ...`
    - `core_type` for core types such as `'a list`, `int -> string`, or
      `(module S with type t = int)`
    - `module_type` and `module_expression` for module-language syntax such as
      `sig ... end`, `functor (X : S) -> T`, `F(X)`, or `F()`
    - `Item.t` and `SourceFile.t` for file-level structure

    Some public nodes deliberately retain raw `syntax_node`s for subgrammars
    that parse successfully but are not yet reified into a richer public type.
    When that happens, the documentation calls it out explicitly so consumers
    know which details live in the red tree instead of dedicated fields.
*)
type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node
(** A red-tree node from Ceibo.

    Every major CST value keeps its originating `syntax_node`, which lets tools
    recover spans, inspect retained trivia, or fall back to lower-level syntax
    when the public CST does not expose a dedicated field.
*)

type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token
(** A red-tree token from Ceibo.

    Tokens are surfaced directly only when the exact spelling matters to CST
    consumers, for example identifier names, operator symbols, or literal text.
*)

type green_node = (Syntax_kind.t, string) Ceibo.Green.node
(** A green-tree node from Ceibo.

    This appears in the public interface for consumers that need to bridge
    between the immutable green tree and the lifted CST, but most callers stay
    at the `syntax_node` level.
*)

(** Thin wrapper around a Ceibo token with helpers for common token-oriented
    queries.

    Use `text` when you care about the source spelling and `span` when you need
    source coordinates.
*)
module Token : sig
  (** A CST token wrapper.

      This is used throughout the CST anywhere the concrete spelling is part of
      the public shape, for example the `name_token` in `let x = ...` or the
      `operator_token` in `a + b`.
  *)
  type t = { syntax_token : syntax_token }

  val syntax_token : t -> syntax_token
  val text : t -> string
  val span : t -> Ceibo.Span.t
end

(** Dotted identifiers and paths.

    The same representation is used for value paths, type constructor paths, and
    module paths whenever the grammar is simply a chain of segments separated by
    dots.

    Examples:

    ```ocaml,norun
    x
    List.map
    ```

    `Map.Make(Key).t` is represented only for the dotted segments that the CST
    lifts directly; richer functor application syntax is modeled elsewhere.
*)
module Ident : sig
  type t =
    | Ident of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
        (** A single unqualified segment such as `x`, `result`, or `List`. *)
    | Qualified of {
        syntax_node : syntax_node;
        prefix : t;
        dot_token : Token.t;
        name_token : Token.t;
      }
        (** A dotted path segment appended to an earlier path.

            Examples include `Std.List`, `M.N`, and `Foo.Bar.baz`.
        *)

  val syntax_node : t -> syntax_node
  val segments : t -> Token.t list
  val last_segment : t -> Token.t option
  val name : t -> string option
end

(** Module paths reuse `Ident`'s dotted-path representation.

    Examples include `String`, `Set.S`, and `Driver.Sqlite`.
*)
module ModulePath = Ident

(** An OCaml attribute attached to some surrounding grammar node.

    This covers item, type, pattern, expression, and module-language attributes.
    The `sigil_token` preserves whether the attribute was introduced with a
    single `@`, double `@@`, or floating `@@@`-style sigil, while
    `payload_syntax_node` keeps the raw payload when one was present.

    Examples:

    ```ocaml,norun
    let x = 1 [@inline]
    type t = int [@@boxed]
    [@@@warning "-32"]
    ```
*)
type attribute = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  name : Ident.t;
  payload_syntax_node : syntax_node option;
}

(** A PPX extension node.

    Extensions appear in whatever grammar position admits `[%name ...]`,
    `[%%name ...]`, or similar extension forms. The payload is intentionally
    retained as a raw syntax node so tooling can inspect the exact embedded
    grammar accepted by the parser.

    Examples:

    ```ocaml,norun
    [%sql "select 1"]
    [%foo: int]
    ```
*)
type extension = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  name : Ident.t;
  payload_syntax_node : syntax_node option;
}

(** A field inside an object type.

    This covers method-like object type members written between `<` and `>`.

    Examples:

    ```ocaml,norun
    < next : int; close : unit -> unit >
    ```
*)
type object_type_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  field_type : core_type;
}

(** Binder names introduced by explicitly polymorphic type syntax.

    These show up in quantified types such as `'a. 'a -> 'a` and
    `type a b. a -> b -> a`.
*)
and type_binder =
  | Quoted of {
      syntax_node : syntax_node;
      name_token : Token.t;
    }
      (** A quoted type binder written with a leading apostrophe.

          Example: `'a` in `'a. 'a -> 'a`.
      *)
  | Bare of {
      name_token : Token.t;
    }
      (** A bare binder reconstructed from `type ... .` syntax.

          Example: `a` in `type a. a -> a`.

          Bare binders do not currently expose their own `syntax_node`; they are
          recovered from the token stream of the surrounding quantified type.
      *)

(** A field inside a record type.

    This covers immutable and mutable record fields declared between `{` and
    `}`. Field-level attributes such as `[@deprecated]` are attached here,
    while attributes nested inside parentheses remain part of `field_type`.

    Examples:

    ```ocaml,norun
    { name : string; mutable count : int }
    { count : int [@deprecated] }
    ```
*)
and record_type_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  field_type : core_type;
  is_mutable : bool;
  attributes : attribute list;
}

(** A tag inside a polymorphic variant type.

    Examples:

    ```ocaml,norun
    [ `Ok of int | `Error of string ]
    [ `Ready ]
    ```
*)
and poly_variant_tag = {
  syntax_node : syntax_node;
  tag_name : Token.t;
  payload_type : core_type option;
}

(** The bound marker on a polymorphic variant type.

    Examples:

    ```ocaml,norun
    [ `A | `B ]
    [< `A | `B ]
    [> `A | `B ]
    ```
*)
and poly_variant_bound =
  | Exact
      (** An exact row with no explicit bound marker.

          Example: `[ `A | `B ]`.
      *)
  | UpperBound of {
      marker_token : Token.t;
    }
      (** An upper-bounded row introduced with `<`.

          Example: `[< `A | `B ]`.
      *)
  | LowerBound of {
      marker_token : Token.t;
    }
      (** A lower-bounded row introduced with `>`.

          Example: `[> `A | `B ]`.
      *)

(** A row field inside a polymorphic variant type.

    This covers both concrete tags and inherited rows.

    Examples:

    ```ocaml,norun
    [ `Ok of int | `Error of string ]
    [ color | `Yellow ]
    ```
*)
and row_field =
  | Tag of poly_variant_tag
      (** A concrete tag row such as `` `Ok of int ``. *)
  | Inherit of {
      syntax_node : syntax_node;
      type_ : core_type;
    }
      (** An inherited row type such as `color` in
          `[ color | `Yellow ]`.
      *)

(** A full polymorphic variant row.

    The row keeps the surrounding syntax node, the bound marker, and the row
    fields in source order.
*)
and poly_variant = {
  syntax_node : syntax_node;
  kind : poly_variant_bound;
  fields : row_field list;
}

(** A `constraint ... = ...` attached to a type declaration.

    These correspond to `ptype_cstrs` in the stock parsetree and preserve both
    sides as lifted core types.

    Examples:

    ```ocaml,norun
    type 'a t = 'a list constraint 'a = int
    type ('a, 'b) pair = 'a * 'b constraint 'a = int constraint 'b = string
    ```
*)
and type_constraint = {
  syntax_node : syntax_node;
  left : core_type;
  right : core_type;
}

(** A `with type` constraint attached to a module type.

    Both ordinary equality constraints and destructive substitutions are covered
    here.

    Examples:

    ```ocaml,norun
    S with type t = int
    S with type t := string
    ```
*)
and module_type_constraint = {
  syntax_node : syntax_node;
  type_name : Token.t;
  replacement_type : core_type;
  is_destructive : bool;
}

(** A named functor parameter.

    Examples:

    ```ocaml,norun
    functor (X : S) -> T
    module F (X : S) (Y : T) = ...
    ```
*)
and functor_parameter = {
  syntax_node : syntax_node;
  name_token : Token.t;
  module_type : module_type;
}

(** A locally opened type expression.

    This preserves the explicit `Module.(...)` wrapper used to resolve the body
    type inside a temporary module-open scope.

    Examples:

    ```ocaml,norun
    Outer.Inner.(request -> response)
    M.(t list)
    ```
*)
and local_open_core_type = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  type_ : core_type;
}

(** Core type syntax.

    This family covers the type grammar used in annotations, manifests, record
    fields, object fields, value declarations, and first-class module types. The
    representation aims to surface the common structural forms directly while
    still retaining the original `syntax_node` for details that are only kept in
    the underlying Ceibo tree.
*)
and core_type =
  | Wildcard of {
      syntax_node : syntax_node;
      wildcard_token : Token.t;
    }
      (** An anonymous type variable written as `_`.

          Examples include `_` in `type (_, 'a) t` or `_` in
          `let f (_ : _) = ...`.
      *)
  | Var of {
      syntax_node : syntax_node;
      name_token : Token.t;
    }
      (** A named type variable such as `'a`, `'state`, or `'msg`. *)
  | Constr of {
      syntax_node : syntax_node;
      constructor_path : Ident.t;
      arguments : core_type list;
    }
      (** A named type constructor, optionally applied to arguments.

          Covered grammar includes plain constructor names and type
          applications such as `int`, `'a list`, `(int, string) result`, or
          `Map.S.t`.
      *)
  | Class of {
      syntax_node : syntax_node;
      hash_token : Token.t;
      class_path : Ident.t;
      arguments : core_type list;
    }
      (** A class type written with `#`.

          Examples:

          ```ocaml,norun
          #iterator
          int #iterator
          ```
      *)
  | Alias of {
      syntax_node : syntax_node;
      type_ : core_type;
      name_token : Token.t;
    }
      (** A type aliased with `as`.

          Example: `('a list as 'whole)`.
      *)
  | Attribute of {
      syntax_node : syntax_node;
      type_ : core_type;
      attribute : attribute;
    }
      (** A type expression with an attached attribute.

          Example: `int [@boxed]`.
      *)
  | Extension of extension
      (** A PPX extension parsed in type position.

          Example: `[%foo: int]`.
      *)
  | Poly of {
      syntax_node : syntax_node;
      binders : type_binder list;
      body : core_type;
    }
      (** An explicitly quantified type.

          Covered grammar includes both apostrophe binders and locally abstract
          `type` binders.

          Examples:

          ```ocaml,norun
          'a. 'a -> 'a
          type a. a -> a
          ```
      *)
  | Arrow of {
      syntax_node : syntax_node;
      parameter_type : core_type;
      result_type : core_type;
    }
      (** A function type.

          This covers simple, labeled, and optional arrows. The structured CST
          stores only the parameter and result types; labels remain recoverable
          from the underlying `syntax_node`.

          Examples:

          ```ocaml,norun
          int -> string
          ~label:int -> string
          ?state:string -> bool
          ```
      *)
  | Tuple of {
      syntax_node : syntax_node;
      elements : core_type list;
    }
      (** A tuple type with two or more elements.

          Example: `int * string * bool`.
      *)
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : core_type;
    }
      (** A parenthesized type used for grouping.

          Examples:

          ```ocaml,norun
          (int -> string)
          ('a * 'b) list
          ```
      *)
  | LocalOpen of local_open_core_type
      (** A locally opened type expression.

          Examples:

          ```ocaml,norun
          Outer.Inner.(request -> response)
          M.(t list)
          ```
      *)
  | PolyVariant of poly_variant
      (** A polymorphic variant type.

          Examples:

          ```ocaml,norun
          [ `Ok of int | `Error of string ]
          [ `Tick | `Tock ]
          [ color | `Yellow ]
          [> `Ready | `Pending ]
          ```
      *)
  | Record of {
      syntax_node : syntax_node;
      fields : record_type_field list;
    }
      (** A record type definition written between `{` and `}`.

          Example: `{ name : string; mutable visits : int }`.
      *)
  | FirstClassModule of {
      syntax_node : syntax_node;
      module_type : module_type;
    }
      (** A first-class module type.

          Examples:

          ```ocaml,norun
          (module S)
          (module Handler with type t = int)
          ```
      *)
  | Object of {
      syntax_node : syntax_node;
      fields : object_type_field list;
    }
      (** An object type written between `<` and `>`.

          Example: `< push : int -> unit; close : unit -> unit >`.
      *)

(** Module type expressions.

    These nodes cover signature-like grammar that appears in module
    declarations, module type declarations, functor parameters, `with type`
    constraints, and first-class module types.
*)
and module_type =
  | Path of Ident.t
      (** A named module type path such as `S`, `Set.S`, or `Driver.Intf`. *)
  | TypeOf of {
      syntax_node : syntax_node;
      module_path : Ident.t;
    }
      (** A `module type of` expression.

          Example: `module type of M`.
      *)
  | Signature of {
      syntax_node : syntax_node;
      signature_syntax_node : syntax_node;
    }
      (** A raw `sig ... end` module type.

          The signature body is preserved as `signature_syntax_node` rather than
          being reified into a separate public signature-item tree.

          Example: `sig type t val make : unit -> t end`.
      *)
  | Functor of {
      syntax_node : syntax_node;
      parameters : functor_parameter list;
      result : module_type;
    }
      (** A functor module type.

          Examples:

          ```ocaml,norun
          functor (X : S) -> T
          functor (X : S) (Y : T) -> U
          ```
      *)
  | With of {
      syntax_node : syntax_node;
      base : module_type;
      constraints : module_type_constraint list;
    }
      (** A constrained module type using `with`.

          Examples:

          ```ocaml,norun
          S with type t = int
          Handler with type state := string and type item = int
          ```
      *)
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : module_type;
    }
      (** A parenthesized module type used for grouping.

          Example: `(S with type t = int)`.
      *)
  | Attribute of {
      syntax_node : syntax_node;
      module_type : module_type;
      attribute : attribute;
    }
      (** A module type with an attached attribute.

          Example: `(module type of M [@foo])`.
      *)
  | Extension of extension
      (** A PPX extension parsed in module-type position.

          Example: `[%foo: S]`.
      *)

(** A locally opened class type.

    This preserves the explicit `Module.(...)` wrapper used to resolve a class
    type inside a temporary module-open scope.

    Examples:

    ```ocaml,norun
    M.(c)
    Outer.Inner.(service)
    ```
*)
and local_open_class_type = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  class_type : class_type;
}

(** Class type syntax.

    This family covers the type grammar used by `class ... : ...` annotations
    and `class type ... = ...` declarations.
*)
and class_type =
  | Path of Ident.t
      (** A named class type path such as `c`, `Widget.t`, or `Driver.class_t`. *)
  | Signature of {
      syntax_node : syntax_node;
      fields : class_type_field list;
    }
      (** An `object ... end` class signature.

          Example:

          ```ocaml,norun
          object
            inherit base
            val mutable state : int
            method run : unit -> unit
            constraint t = int
          end
          ```
      *)
  | Arrow of {
      syntax_node : syntax_node;
      parameter_type : core_type;
      result_type : class_type;
    }
      (** An arrow-style class type.

          Examples:

          ```ocaml,norun
          int -> object method run : int end
          request -> response -> service
          ```
      *)
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : class_type;
    }
      (** A parenthesized class type used for grouping.

          Example: `([%driver])`.
      *)
  | LocalOpen of local_open_class_type
      (** A locally opened class type such as `M.(c)`. *)
  | Attribute of {
      syntax_node : syntax_node;
      class_type : class_type;
      attribute : attribute;
    }
      (** A class type with an attached attribute.

          Example: `object method x : int end [@foo]`.
      *)
  | Extension of extension
      (** A PPX extension parsed in class-type position.

          Example: `[%foo]`.
      *)

(** Fields inside an `object ... end` class signature. *)
and class_type_field =
  | Inherit of {
      syntax_node : syntax_node;
      class_type : class_type;
    }
      (** An inherited class type field.

          Example: `inherit base`.
      *)
  | Value of {
      syntax_node : syntax_node;
      name_token : Token.t;
      type_ : core_type;
      is_mutable : bool;
    }
      (** A value declaration in a class signature.

          Examples:

          ```ocaml,norun
          val x : int
          val mutable state : int
          ```
      *)
  | Method of {
      syntax_node : syntax_node;
      name_token : Token.t;
      type_ : core_type;
      is_private : bool;
    }
      (** A method declaration in a class signature.

          Examples:

          ```ocaml,norun
          method run : unit -> unit
          method private close : unit
          ```
      *)
  | Constraint of {
      syntax_node : syntax_node;
      left : core_type;
      right : core_type;
    }
      (** A class-type constraint.

          Example: `constraint t = int`.
      *)
  | Attribute of {
      syntax_node : syntax_node;
      field : class_type_field;
      attribute : attribute;
    }
      (** A class-type field with an attached attribute.

          Example: `val x : int [@@foo]`.
      *)
  | Extension of extension
      (** A PPX extension parsed as a class-type field.

          Example: `[%%foo]`.
      *)

(** Namespace view over `core_type`.

    This is useful when you want constructor names under `CoreType`, for
    example `CoreType.Arrow` or `CoreType.Record`.

    The constructors mirror `core_type` exactly, so the grammar coverage and
    examples documented on `core_type` apply here unchanged.
*)
module CoreType : sig
  type t = core_type =
    | Wildcard of {
        syntax_node : syntax_node;
        wildcard_token : Token.t;
      }
    | Var of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Constr of {
        syntax_node : syntax_node;
        constructor_path : Ident.t;
        arguments : core_type list;
      }
    | Class of {
        syntax_node : syntax_node;
        hash_token : Token.t;
        class_path : Ident.t;
        arguments : core_type list;
      }
    | Alias of {
        syntax_node : syntax_node;
        type_ : core_type;
        name_token : Token.t;
      }
    | Attribute of {
        syntax_node : syntax_node;
        type_ : core_type;
        attribute : attribute;
      }
    | Extension of extension
    | Poly of {
        syntax_node : syntax_node;
        binders : type_binder list;
        body : core_type;
      }
    | Arrow of {
        syntax_node : syntax_node;
        parameter_type : core_type;
        result_type : core_type;
      }
    | Tuple of {
        syntax_node : syntax_node;
        elements : core_type list;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : core_type;
      }
    | LocalOpen of local_open_core_type
    | PolyVariant of poly_variant
    | Record of {
        syntax_node : syntax_node;
        fields : record_type_field list;
      }
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type : module_type;
      }
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }

  val syntax_node : t -> syntax_node
end

(** Namespace view over `module_type_constraint`. *)
module ModuleTypeConstraint : sig
  type t = module_type_constraint = {
    syntax_node : syntax_node;
    type_name : Token.t;
    replacement_type : core_type;
    is_destructive : bool;
  }
end

(** Namespace view over `type_constraint`. *)
module TypeConstraint : sig
  type t = type_constraint = {
    syntax_node : syntax_node;
    left : core_type;
    right : core_type;
  }
end

(** Namespace view over `functor_parameter`. *)
module FunctorParameter : sig
  type t = functor_parameter = {
    syntax_node : syntax_node;
    name_token : Token.t;
    module_type : module_type;
  }
end

(** Namespace view over `module_type`.

    The constructors mirror `module_type` exactly, so the grammar coverage and
    examples documented on `module_type` apply here unchanged.
*)
module ModuleType : sig
  type t = module_type =
    | Path of Ident.t
    | TypeOf of {
        syntax_node : syntax_node;
        module_path : Ident.t;
      }
    | Signature of {
        syntax_node : syntax_node;
        signature_syntax_node : syntax_node;
      }
    | Functor of {
        syntax_node : syntax_node;
        parameters : functor_parameter list;
        result : module_type;
      }
    | With of {
        syntax_node : syntax_node;
        base : module_type;
        constraints : module_type_constraint list;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : module_type;
      }
    | Attribute of {
        syntax_node : syntax_node;
        module_type : module_type;
        attribute : attribute;
      }
    | Extension of extension

  val syntax_node : t -> syntax_node
end

(** Namespace view over `class_type`.

    The constructors mirror `class_type` exactly, so the grammar coverage and
    examples documented on `class_type` apply here unchanged.
*)
module ClassType : sig
  type t = class_type =
    | Path of Ident.t
    | Signature of {
        syntax_node : syntax_node;
        fields : class_type_field list;
      }
    | Arrow of {
        syntax_node : syntax_node;
        parameter_type : core_type;
        result_type : class_type;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : class_type;
      }
    | LocalOpen of local_open_class_type
    | Attribute of {
        syntax_node : syntax_node;
        class_type : class_type;
        attribute : attribute;
      }
    | Extension of extension

  val syntax_node : t -> syntax_node
end

(** Namespace view over `class_type_field`. *)
module ClassTypeField : sig
  type t = class_type_field =
    | Inherit of {
        syntax_node : syntax_node;
        class_type : class_type;
      }
    | Value of {
        syntax_node : syntax_node;
        name_token : Token.t;
        type_ : core_type;
        is_mutable : bool;
      }
    | Method of {
        syntax_node : syntax_node;
        name_token : Token.t;
        type_ : core_type;
        is_private : bool;
      }
    | Constraint of {
        syntax_node : syntax_node;
        left : core_type;
        right : core_type;
      }
    | Attribute of {
        syntax_node : syntax_node;
        field : class_type_field;
        attribute : attribute;
      }
    | Extension of extension

  val syntax_node : t -> syntax_node
end

(** Literal forms accepted directly in pattern position.

    These are the exact constant-pattern shapes that the CST lifts without
    wrapping them in a more general `pattern` payload.
*)
module PatternLiteral : sig
  type t =
    | String of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** String literal patterns that match a specific quoted string.

            Examples:

            ```ocaml,norun
            "hello"
            "world"
            ```
        *)
    | Int of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Integer literal patterns such as `0`, `42`, or `2112`. *)
    | Float of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Floating-point literal patterns such as `0.0` or `3.14`. *)
    | Char of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Character literal patterns such as `'a'` or `'\n'`. *)
    | Bool of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Boolean literal patterns `true` and `false`. *)
    | Unit of { syntax_node : syntax_node }
        (** The unit literal pattern `()`. *)
end

(** Alias for `PatternLiteral.t`. *)
type pattern_literal = PatternLiteral.t

(** Namespace helpers for `type_binder`.

    The constructors mirror `type_binder` exactly, so the binder grammar
    documented above applies here unchanged.
*)
module TypeBinder : sig
  type t = type_binder =
    | Quoted of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Bare of {
        name_token : Token.t;
      }

  val name_token : t -> Token.t
  val name : t -> string
  val text : t -> string
  val is_quoted : t -> bool
end

(** Pattern syntax.

    Patterns appear in `let` bindings, function parameters, `match` cases,
    `try ... with` handlers, object self patterns, and effect handlers. This
    type covers the common structural forms directly and uses the record payload
    types below to expose the relevant nested grammar.
*)
type pattern =
  | Identifier of identifier_pattern
      (** A binding-name pattern such as `x`, `value`, or `state`. *)
  | Wildcard of wildcard_pattern
      (** A wildcard pattern written as `_`. *)
  | Attribute of attributed_pattern
      (** A pattern with an attached attribute.

          Example: `p [@foo]`.
      *)
  | Extension of extension
      (** A PPX extension parsed in pattern position.

          Example: `[%foo? p]`.
      *)
  | Literal of pattern_literal
      (** A literal pattern such as `"hello"`, `true`, `2112`, or `()`. *)
  | Lazy of lazy_pattern
      (** A lazy pattern.

          Example: `lazy p`.
      *)
  | Exception of exception_pattern
      (** An exception pattern used in handlers.

          Example: `exception Not_found`.
      *)
  | Range of range_pattern
      (** A range pattern.

          Examples include `'a' .. 'z'` and `0 .. 9`.
      *)
  | Operator of operator_pattern
      (** An operator identifier used as a pattern name.

          Example: `( :: )`.
      *)
  | FirstClassModule of first_class_module_pattern
      (** A first-class module unpacking pattern.

          Examples:

          ```ocaml,norun
          (module M)
          (module M : S)
          ```
      *)
  | PolyVariant of poly_variant_pattern
      (** A polymorphic variant tag pattern such as `` `Ok x `` or `` `Done ``. *)
  | PolyVariantInherit of poly_variant_inherit_pattern
      (** A polymorphic variant type inheritance pattern such as `#message`. *)
  | Constructor of constructor_pattern
      (** A constructor pattern such as `Some x`, `Ok (a, b)`, or `M.Error`. *)
  | Tuple of tuple_pattern
      (** A tuple pattern such as `(x, y)` or `(x, y, z)`. *)
  | List of list_pattern
      (** A list pattern such as `[x; y; z]`. *)
  | Array of array_pattern
      (** An array pattern such as `[| x; y |]`. *)
  | Record of record_pattern
      (** A record pattern such as `{ x; y = Some z }`. *)
  | Cons of cons_pattern
      (** A cons pattern written with `::`, such as `x :: xs`. *)
  | Or of or_pattern
      (** An alternative pattern such as `None | Some _`. *)
  | Alias of alias_pattern
      (** A pattern alias introduced with `as`.

          Example: `(Some x as whole)`.
      *)
  | Typed of typed_pattern
      (** A type-constrained pattern.

          Example: `(p : t)`.
      *)
  | Effect of effect_pattern
      (** An effect pattern.

          Example: `effect Yield k`.
      *)
  | LocalOpen of local_open_pattern
      (** A locally opened module path in pattern position.

          Example: `M.(Some x)`.
      *)
  | Parenthesized of parenthesized_pattern
      (** A parenthesized pattern used for grouping, such as `(Some x)`. *)

(** Payload for `Pattern.Identifier`.

    This covers simple lowercase binding names and raw identifiers when parsed
    as patterns.
*)
and identifier_pattern = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

(** Payload for `Pattern.Attribute`.

    The nested `pattern` is the attributed payload, and `attribute` is the
    trailing attribute node attached to it.
*)
and attributed_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  attribute : attribute;
}

(** Payload for `Pattern.Wildcard`.

    Carries the original `syntax_node` for `_`.
*)
and wildcard_pattern = {
  syntax_node : syntax_node;
}

(** Payload for `Pattern.Lazy`.

    Covers `lazy p`.
*)
and lazy_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
}

(** Payload for `Pattern.Exception`.

    Covers handler patterns like `exception Exit`.
*)
and exception_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
}

(** Payload for `Pattern.Range`.

    The public CST preserves the lower and upper endpoint tokens exactly as they
    appeared in source.
*)
and range_pattern = {
  syntax_node : syntax_node;
  lower_token : Token.t;
  upper_token : Token.t;
}

(** Payload for `Pattern.Operator`.

    This is used when the pattern is spelled as an operator token sequence
    instead of an alphanumeric identifier.
*)
and operator_pattern = {
  syntax_node : syntax_node;
  operator_tokens : Token.t list;
}

(** Payload for `Pattern.FirstClassModule`.

    This covers patterns of the form `(module Name)` and `(module Name : S)`.
*)
and first_class_module_pattern = {
  syntax_node : syntax_node;
  name_token : Token.t;
  module_type : module_type option;
}

(** Payload for `Pattern.PolyVariant`.

    The `payload` is present for forms like `` `Ok x `` and absent for bare tags
    like `` `Done ``.
*)
and poly_variant_pattern = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : pattern option;
}

(** Payload for `Pattern.PolyVariantInherit`.

    This covers inheritance-style polymorphic variant patterns such as
    `#message` or `#M.message`.
*)
and poly_variant_inherit_pattern = {
  syntax_node : syntax_node;
  type_path : Ident.t;
}

(** Payload for `Pattern.Constructor`.

    Constructor arguments are stored in source order, so both unary and tupled
    payloads can be inspected structurally.
*)
and constructor_pattern = {
  syntax_node : syntax_node;
  constructor_path : Ident.t;
  arguments : pattern list;
}

(** Payload for `Pattern.Tuple`.

    Covers tuple patterns such as `(x, y)` and `(x, y, z)`.
*)
and tuple_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
}

(** Payload for `Pattern.List`.

    Covers list patterns such as `[x]` and `[head; tail]`.
*)
and list_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
}

(** Payload for `Pattern.Array`.

    Covers array patterns such as `[| x; y |]`.
*)
and array_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
}

(** Payload for `Pattern.Record`.

    Each field may be explicit, as in `{ x = p }`, or punning, as in `{ x }`.
*)
and record_pattern = {
  syntax_node : syntax_node;
  fields : record_pattern_field list;
}

(** A single field inside a record pattern.

    When `pattern` is `None`, the source used punning syntax such as `{ field }`.
    When it is `Some p`, the source spelled an explicit field pattern such as
    `{ field = p }`.
*)
and record_pattern_field = {
  syntax_node : syntax_node;
  field_path : Ident.t;
  pattern : pattern option;
}

(** Payload for `Pattern.Cons`.

    Covers `head :: tail`.
*)
and cons_pattern = {
  syntax_node : syntax_node;
  head : pattern;
  tail : pattern;
}

(** Payload for `Pattern.Or`.

    Alternatives are preserved in source order from left to right.
*)
and or_pattern = {
  syntax_node : syntax_node;
  alternatives : pattern list;
}

(** Payload for `Pattern.Alias`.

    Covers `p as name`.
*)
and alias_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  name_token : Token.t;
}

(** Payload for `Pattern.Typed`.

    Covers `(p : t)` and related parenthesized type-constrained patterns.
*)
and typed_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  type_ : core_type;
}

(** Payload for `Pattern.Effect`.

    The `effect_pattern` is the matched effect constructor, and `continuation`
    is the continuation binder pattern.
*)
and effect_pattern = {
  syntax_node : syntax_node;
  effect_pattern : pattern;
  continuation : pattern;
}

(** Payload for `Pattern.LocalOpen`.

    Covers locally opened patterns such as `M.(Some x)`.
*)
and local_open_pattern = {
  syntax_node : syntax_node;
  module_path : ModulePath.t;
  pattern : pattern;
}

(** Payload for `Pattern.Parenthesized`. *)
and parenthesized_pattern = {
  syntax_node : syntax_node;
  inner : pattern;
}

(** A positional function parameter.

    This covers ordinary unlabeled parameters in `fun` expressions and
    function-like `let` bindings, such as `x`, `_`, or `(x : int)`.
*)
type positional_parameter = {
  syntax_node : syntax_node;
  name_token : Token.t option;
}

(** A labeled parameter introduced with `~`.

    Examples include `~x` and `~label`.
*)
type labeled_parameter = {
  syntax_node : syntax_node;
  label_token : Token.t;
  binding_name_token : Token.t option;
}

(** An optional parameter introduced with `?`.

    This covers both plain optional parameters like `?x` and parameters with a
    default such as `?(x = 0)`.
*)
type optional_parameter = {
  syntax_node : syntax_node;
  label_token : Token.t;
  binding_name_token : Token.t option;
  has_default : bool;
}

(** A locally abstract type parameter in function parameter position.

    Examples:

    ```ocaml,norun
    (type a)
    (type a b)
    ```
*)
type locally_abstract_type_parameter = {
  syntax_node : syntax_node;
  binders : type_binder list;
}

(** Function parameter syntax.

    Parameters appear in `fun` expressions and in function-shaped `let`
    bindings. The CST separates positional, labeled, optional, and locally
    abstract parameters so tooling can reason about the source-level calling
    convention directly.
*)
type parameter =
  | Positional of positional_parameter
      (** An ordinary unlabeled parameter such as `x` or `(x : int)`. *)
  | Labeled of labeled_parameter
      (** A labeled parameter such as `~x`. *)
  | Optional of optional_parameter
      (** An optional parameter such as `?x` or `?(x = 0)`. *)
  | LocallyAbstract of locally_abstract_type_parameter
      (** A locally abstract type binder such as `(type a)`. *)

(** Namespace helpers for `parameter`.

    The constructors mirror `parameter` exactly, so the parameter grammar
    documented above applies here unchanged.
*)
module Parameter : sig
  type t = parameter =
    | Positional of positional_parameter
    | Labeled of labeled_parameter
    | Optional of optional_parameter
    | LocallyAbstract of locally_abstract_type_parameter

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t option
  val name : t -> string option
  val is_named : t -> bool
  val has_default : t -> bool
end

(** Literal forms accepted directly in expression position.

    These are the same constant categories as `PatternLiteral`, but used for
    expressions instead of patterns.
*)
module Literal : sig
  type t =
    | String of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** String literal expressions such as `"hello"` or `"world"`. *)
    | Int of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Integer literal expressions such as `0`, `42`, or `0xff`. *)
    | Float of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Floating-point literal expressions such as `3.14`. *)
    | Char of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Character literal expressions such as `'a'`. *)
    | Bool of {
        syntax_node : syntax_node;
        literal_token : Token.t;
      }
        (** Boolean literal expressions `true` and `false`. *)
    | Unit of { syntax_node : syntax_node }
        (** The unit literal expression `()`. *)
end

(** Alias for `Literal.t`. *)
type literal = Literal.t

(** An exception declaration header.

    This is used both for top-level `exception` items and `let exception ... in`
    expressions. The public CST currently exposes the declared name directly and
    retains any richer payload shape in the underlying `syntax_node`.

    Examples:

    ```ocaml,norun
    exception Panic
    exception Panic of string
    ```
*)
type exception_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

(** Expression syntax.

    This is the main term-level grammar family. It covers evaluated expressions,
    control-flow forms, object syntax, record and collection literals, local
    module constructs, and first-class modules.
*)
type expression =
  | Path of path_expression
      (** A value or constructor path used as an expression, such as `x`,
          `M.value`, or `List.map`. *)
  | Operator of operator_expression
      (** An operator identifier used as an expression, such as `(+)` or `(@@)`. *)
  | Literal of literal
      (** A literal expression such as `"hello"`, `42`, `true`, or `()`. *)
  | Unreachable of unreachable_expression
      (** An unreachable expression written as `.`.

          Example: `| None -> .`.
      *)
  | Attribute of attribute
      (** An expression carrying an attached attribute, such as `expr [@inline]`. *)
  | Extension of extension
      (** A PPX extension parsed in expression position, such as `[%sql "..."]`. *)
  | Object of object_expression
      (** An object expression.

          Example:

          ```ocaml,norun
          object
            method size = 0
          end
          ```
      *)
  | PolyVariant of poly_variant_expression
      (** A polymorphic variant expression such as `` `Ok value `` or `` `Done ``. *)
  | FirstClassModule of first_class_module_expression
      (** A packed first-class module expression.

          Examples:

          ```ocaml,norun
          (module M)
          (module M : S)
          ```
      *)
  | LetModule of let_module_expression
      (** A local module binding expression.

          Example: `let module M = N in body`.
      *)
  | LetException of let_exception_expression
      (** A local exception declaration expression.

          Example: `let exception Panic of string in body`.
      *)
  | Assert of assert_expression
      (** An assertion expression: `assert cond`. *)
  | Lazy of lazy_expression
      (** A lazy expression: `lazy expr`. *)
  | While of while_expression
      (** A `while ... do ... done` loop. *)
  | For of for_expression
      (** A `for ... = ... to|downto ... do ... done` loop. *)
  | Apply of apply_expression
      (** A function application, including labeled and optional arguments. *)
  | MethodCall of method_call_expression
      (** An object method call such as `obj#run`. *)
  | New of new_expression
      (** An object instantiation such as `new queue`. *)
  | Prefix of prefix_expression
      (** A prefix operator application such as `!cell` or `~-x`. *)
  | FieldAccess of field_access_expression
      (** A field access such as `record.field`. *)
  | Index of index_expression
      (** An indexing expression such as `arr.(i)` or `s.[i]`. *)
  | ObjectUpdate of object_update_expression
      (** An object update literal such as `< x = 1; y = 2 >`. *)
  | InstanceVariableAssign of instance_variable_assign_expression
      (** An instance-variable assignment inside object syntax, such as
          `count <- count + 1`. *)
  | Assign of assign_expression
      (** A general assignment expression such as `r := 1`,
          `arr.(i) <- x`, or `record.field <- y`. *)
  | Infix of infix_expression
      (** An infix operator expression such as `a + b` or `x |> f`. *)
  | Typed of typed_expression
      (** A type-constrained expression such as `(expr : t)` or
          `let value : t = expr`.

          When lifted from a binding annotation, `syntax_node` is the enclosing
          `let` binding because the parser does not currently emit a dedicated
          annotation expression node.
      *)
  | Polymorphic of polymorphic_expression
      (** An explicitly polymorphic binding annotation such as
          `let id : 'a. 'a -> 'a = fun x -> x`.

          This is reconstructed from the surrounding binding or typed
          expression syntax when the annotated type is a quoted `CoreType.Poly`.
      *)
  | Coerce of coerce_expression
      (** A coercion expression such as `(expr :> t)` or `(expr : s :> t)`. *)
  | Sequence of sequence_expression
      (** A sequence expression such as `e1; e2`. *)
  | Tuple of tuple_expression
      (** A tuple expression such as `(a, b, c)`. *)
  | List of list_expression
      (** A list expression such as `[a; b; c]`. *)
  | Array of array_expression
      (** An array expression such as `[| a; b; c |]`. *)
  | Record of record_expression
      (** A record literal or record update expression. *)
  | LocalOpen of local_open_expression
      (** A local module open expression.

          Covered forms include `M.(expr)` and `let open M in expr`.
      *)
  | Fun of fun_expression
      (** A `fun` expression with explicit parameters. *)
  | Function of function_expression
      (** A `function` expression made of match cases. *)
  | LetOperator of let_operator_expression
      (** A binding-operator expression such as
          `let* x = expr in body` or
          `let* x = expr and* y = expr in body`. *)
  | Let of let_expression
      (** A `let ... in ...` or `let rec ... in ...` expression. *)
  | Match of match_expression
      (** A `match ... with ...` expression. *)
  | Try of try_expression
      (** A `try ... with ...` expression. *)
  | If of if_expression
      (** An `if ... then ... else ...` expression. *)
  | Parenthesized of parenthesized_expression
      (** A parenthesized expression used for grouping. *)

(** Payload for `Expression.Path`.

    Covers path expressions such as `x`, `M.value`, and `List.map`.
*)
and path_expression = {
  syntax_node : syntax_node;
  path : Ident.t;
}

(** Payload for `Expression.Operator`.

    This is used when the expression itself is an operator name rather than an
    infix application.
*)
and operator_expression = {
  syntax_node : syntax_node;
  operator_tokens : Token.t list;
}

(** Payload for `Expression.Unreachable`.

    Covers the standalone `.` expression used in impossible branches.
*)
and unreachable_expression = {
  syntax_node : syntax_node;
  dot_token : Token.t;
}

(** Payload for `Expression.Object`.

    `self_pattern` is present for forms such as `object (self) ... end`.
*)
and object_expression = {
  syntax_node : syntax_node;
  self_pattern : pattern option;
  members : object_member list;
}

(** Members inside an object expression body. *)
and object_member =
  | Method of object_method
      (** A `method` declaration inside `object ... end`. *)
  | Value of object_value
      (** A `val` declaration inside `object ... end`. *)
  | Inherit of object_inherit
      (** An `inherit expr` member. *)
  | Extension of extension
      (** A PPX extension member inside `object ... end`.

          Example: `[%%foo]`.
      *)
  | Initializer of object_initializer
      (** An `initializer expr` member. *)

(** Payload for `object_member` methods.

    This covers concrete methods, virtual methods, private methods, and
    overriding methods such as `method!`.
*)
and object_method = {
  syntax_node : syntax_node;
  attributes : attribute list;
  name_token : Token.t;
  body : expression option;
  type_ : core_type option;
  is_private : bool;
  is_virtual : bool;
  is_override : bool;
}

(** Payload for `object_member` values.

    This covers object fields declared with `val`, including `mutable`,
    `virtual`, and overriding forms.
*)
and object_value = {
  syntax_node : syntax_node;
  attributes : attribute list;
  name_token : Token.t;
  value : expression option;
  type_ : core_type option;
  is_mutable : bool;
  is_virtual : bool;
  is_override : bool;
}

(** Payload for `object_member` inheritance clauses. *)
and object_inherit = {
  syntax_node : syntax_node;
  attributes : attribute list;
  expression : expression;
}

(** Payload for `object_member` initializers.

    Covers `initializer expr`.
*)
and object_initializer = {
  syntax_node : syntax_node;
  body : expression option;
}

(** Payload for `Expression.PolyVariant`.

    The `payload` is present for tagged values such as `` `Ok 1 `` and absent
    for bare tags like `` `Done ``.
*)
and poly_variant_expression = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : expression option;
}

(** Payload for `Expression.FirstClassModule`.

    Covers packed module expressions such as `(module M)` and
    `(module M : S)`.
*)
and first_class_module_expression = {
  syntax_node : syntax_node;
  module_expression : module_expression;
  module_type : module_type option;
}

(** Payload for `Expression.LetModule`.

    Covers local module bindings such as `let module M = struct ... end in body`.
*)
and let_module_expression = {
  syntax_node : syntax_node;
  module_name_token : Token.t;
  module_expression : module_expression;
  body : expression;
}

(** Payload for `Expression.LetException`.

    Covers local exception bindings such as
    `let exception Panic of string in body`.
*)
and let_exception_expression = {
  syntax_node : syntax_node;
  exception_declaration : exception_declaration;
  body : expression;
}

(** Payload for `Expression.Assert`.

    Covers `assert expr`.
*)
and assert_expression = {
  syntax_node : syntax_node;
  asserted : expression;
}

(** Payload for `Expression.Lazy`.

    Covers `lazy expr`.
*)
and lazy_expression = {
  syntax_node : syntax_node;
  body : expression;
}

(** Payload for `Expression.While`.

    Covers `while cond do body done`.
*)
and while_expression = {
  syntax_node : syntax_node;
  condition : expression;
  body : expression;
}

(** Payload for `Expression.For`.

    `direction_token` preserves whether the loop used `to` or `downto`.
*)
and for_expression = {
  syntax_node : syntax_node;
  iterator_token : Token.t;
  start_expr : expression;
  direction_token : Token.t;
  end_expr : expression;
  body : expression;
}

(** A single function-application argument. *)
and apply_argument =
  | Positional of expression
      (** An unlabeled argument such as `x` in `f x`. *)
  | Labeled of labeled_apply_argument
      (** A labeled argument such as `~label:value`. *)
  | Optional of optional_apply_argument
      (** An optional argument such as `?state:value`. *)

(** Payload for labeled application arguments.

    Covers call-site arguments such as `~limit:10`.
*)
and labeled_apply_argument = {
  syntax_node : syntax_node;
  label_token : Token.t;
  value : expression option;
}

(** Payload for optional application arguments.

    Covers call-site arguments such as `?state:(Some s)` and shorthand forms
    like `?state`.
*)
and optional_apply_argument = {
  syntax_node : syntax_node;
  label_token : Token.t;
  value : expression option;
}

(** Payload for `Expression.Apply`.

    The public CST stores a single application step, so chained application such
    as `f x y` appears as nested `Apply` nodes.
*)
and apply_expression = {
  syntax_node : syntax_node;
  callee : expression;
  argument : apply_argument;
}

(** Payload for `Expression.MethodCall`.

    Covers `receiver#method_name`.
*)
and method_call_expression = {
  syntax_node : syntax_node;
  receiver : expression;
  method_name : Token.t;
}

(** Payload for `Expression.New`.

    Covers `new Class_name`.
*)
and new_expression = {
  syntax_node : syntax_node;
  class_path : Ident.t;
}

(** Payload for `Expression.Prefix`.

    Covers prefix operators such as `!cell`, `~-x`, and similar unary forms.
*)
and prefix_expression = {
  syntax_node : syntax_node;
  operator_token : Token.t;
  operand : expression;
}

(** Payload for `Expression.FieldAccess`.

    Covers `receiver.field`.
*)
and field_access_expression = {
  syntax_node : syntax_node;
  receiver : expression;
  field_name : Token.t;
}

(** Payload for `Expression.Index`.

    Covers both array indexing `arr.(i)` and string indexing `s.[i]`.
*)
and index_expression = {
  syntax_node : syntax_node;
  collection : expression;
  index : expression;
}

(** Payload for `Expression.ObjectUpdate`.

    Covers object update syntax written between `<` and `>`.
*)
and object_update_expression = {
  syntax_node : syntax_node;
  fields : record_expression_field list;
}

(** Payload for `Expression.InstanceVariableAssign`.

    This form is reserved for assignments where the target is an object instance
    variable name and the operator is `<-`.
*)
and instance_variable_assign_expression = {
  syntax_node : syntax_node;
  name_token : Token.t;
  operator_token : Token.t;
  value : expression;
}

(** Payload for `Expression.Assign`.

    This covers assignment expressions that are not reduced to
    `InstanceVariableAssign`.
*)
and assign_expression = {
  syntax_node : syntax_node;
  target : expression;
  operator_token : Token.t;
  value : expression;
}

(** Payload for `Expression.Infix`.

    Covers binary operator applications such as `a + b` and `x |> f`.
*)
and infix_expression = {
  syntax_node : syntax_node;
  left : expression;
  operator_token : Token.t;
  right : expression;
}

(** Payload for `Expression.Typed`.

    Covers `(expr : t)` and reconstructed binding annotations such as
    `let value : t = expr`.
*)
and typed_expression = {
  syntax_node : syntax_node;
  expression : expression;
  type_ : core_type;
}

(** Payload for `Expression.Polymorphic`.

    Covers explicit quoted polymorphic constraints such as:

    ```ocaml,norun
    let id : 'a. 'a -> 'a = fun x -> x
    ```

    The `type_` payload is expected to be a `CoreType.Poly`.
*)
and polymorphic_expression = {
  syntax_node : syntax_node;
  expression : expression;
  type_ : core_type;
}

(** Payload for `Expression.Coerce`.

    `from_type` is present for the longer `(expr : from :> to)` spelling and
    absent for the shorter `(expr :> to)` spelling.
*)
and coerce_expression = {
  syntax_node : syntax_node;
  expression : expression;
  from_type : core_type option;
  to_type : core_type;
}

(** Payload for `Expression.Sequence`.

    Covers `left; right`.
*)
and sequence_expression = {
  syntax_node : syntax_node;
  left : expression;
  right : expression;
}

(** Payload for `Expression.Tuple`.

    Covers tuple expressions such as `(a, b)` and `(a, b, c)`.
*)
and tuple_expression = {
  syntax_node : syntax_node;
  elements : expression list;
}

(** Payload for `Expression.List`.

    Covers list expressions such as `[a; b; c]`.
*)
and list_expression = {
  syntax_node : syntax_node;
  elements : expression list;
}

(** Payload for `Expression.Array`.

    Covers array expressions such as `[| a; b; c |]`.
*)
and array_expression = {
  syntax_node : syntax_node;
  elements : expression list;
}

(** Record expression syntax.

    This distinguishes plain record literals from record updates with `with`.
*)
and record_expression =
  | Literal of record_literal_expression
      (** A record literal such as `{ x = 1; y = 2 }`. *)
  | Update of record_update_expression
      (** A record update such as `{ base with x = 1 }`. *)

(** Payload for `record_expression` literals. *)
and record_literal_expression = {
  syntax_node : syntax_node;
  fields : record_expression_field list;
}

(** Payload for `record_expression` updates. *)
and record_update_expression = {
  syntax_node : syntax_node;
  base : expression;
  fields : record_expression_field list;
}

(** A single field inside a record literal, record update, or object update.

    When `value` is `None`, the source used punning syntax such as `{ field }`.
*)
and record_expression_field = {
  syntax_node : syntax_node;
  field_path : Ident.t;
  value : expression option;
}

(** Payload for `Expression.LocalOpen`.

    `via_let_open` is `true` for `let open M in expr` and `false` for
    `M.(expr)`.
*)
and local_open_expression = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  body : expression;
  via_let_open : bool;
}

(** Payload for `Expression.Fun`.

    Covers `fun` expressions with explicit parameter lists, such as
    `fun x ~label ?opt -> body`.
*)
and fun_expression = {
  syntax_node : syntax_node;
  parameters : Parameter.t list;
  body : expression;
}

(** Payload for `Expression.Function`.

    Covers `function` expressions whose body is a list of cases.
*)
and function_expression = {
  syntax_node : syntax_node;
  cases : match_case list;
}

(** A single `let` binding.

    This shape is used both for item-level bindings and for nested `and`
    bindings inside `let ... in ...` expressions. `binding_name` is extracted
    only when the binding pattern has a simple recoverable name.
*)
and let_binding = {
  syntax_node : syntax_node;
  attributes : attribute list;
  binding_pattern : pattern;
  binding_name : Token.t option;
  parameters : Parameter.t list;
  value : expression;
  is_recursive : bool;
}

(** A single binding-operator clause inside a `let*`/`let+`-style expression.

    `keyword_token` preserves whether the clause started with `let` or `and`,
    while `operator_token` keeps the operator suffix such as `*`, `+`, or `=`.

    These clauses are reconstructed from the surrounding `LET_EXPR` token and
    child-node stream because the parser does not yet emit dedicated clause
    nodes for binding operators.
*)
and binding_operator_binding = {
  keyword_token : Token.t;
  operator_token : Token.t;
  binding_pattern : pattern;
  bound_value : expression;
}

(** Payload for `Expression.LetOperator`.

    Covers binding-operator expressions such as:

    ```ocaml,norun
    let* x = read () in
    let* x = read () and* y = load () in
    ```
*)
and let_operator_expression = {
  syntax_node : syntax_node;
  binding : binding_operator_binding;
  and_bindings : binding_operator_binding list;
  body : expression;
}

(** Payload for `Expression.Let`.

    The first binding is split into `binding_pattern` and `bound_value`, while
    any trailing `and` bindings are exposed in `and_bindings`.
*)
and let_expression = {
  syntax_node : syntax_node;
  binding_pattern : pattern;
  bound_value : expression;
  and_bindings : let_binding list;
  body : expression;
  is_recursive : bool;
}

(** Payload for `Expression.Match`.

    Covers `match scrutinee with ...`.
*)
and match_expression = {
  syntax_node : syntax_node;
  scrutinee : expression;
  cases : match_case list;
}

(** Payload for `Expression.Try`.

    Covers `try body with ...`.
*)
and try_expression = {
  syntax_node : syntax_node;
  body : expression;
  cases : match_case list;
}

(** A single match or handler case.

    Covers both plain cases like `| p -> e` and guarded cases like
    `| p when cond -> e`.
*)
and match_case = {
  syntax_node : syntax_node;
  pattern : pattern;
  guard : expression option;
  body : expression;
}

(** Payload for `Expression.If`.

    Covers `if cond then a else b`, with `else_branch = None` for branchless
    `if` expressions.
*)
and if_expression = {
  syntax_node : syntax_node;
  condition : expression;
  then_branch : expression;
  else_branch : expression option;
}

(** Payload for `Expression.Parenthesized`.

    Covers grouping parentheses around an expression.
*)
and parenthesized_expression = {
  syntax_node : syntax_node;
  inner : expression;
}

(** Class expression syntax.

    These nodes cover the grammar accepted to the right of `=` in class
    bindings. They keep class-specific structure distinct from ordinary term
    expressions, while still reusing ordinary `expression` nodes for method
    bodies, field initializers, and `let`-bound values inside the class.
*)
and class_expression =
  | Path of Ident.t
      (** A named class constructor reference such as `c`, `Widget.t`, or
          `Driver.make`.

          Examples:

          ```ocaml,norun
          class direct = c
          class service = Driver.make
          ```
      *)
  | Structure of class_structure
      (** An `object ... end` class structure body.

          Example:

          ```ocaml,norun
          object
            val mutable state = 0
            method run = state
          end
          ```
      *)
  | Fun of class_fun_expression
      (** A function-style class expression.

          Example:

          ```ocaml,norun
          fun x -> object method value = x end
          ```
      *)
  | Apply of class_apply_expression
      (** A class application such as `builder x` or `make ~clock`. *)
  | Let of class_let_expression
      (** A let-bound class expression.

          Example:

          ```ocaml,norun
          let state = ref 0 in object method run = !state end
          ```
      *)
  | Constraint of class_constraint_expression
      (** A class expression constrained by a class type.

          Example:

          ```ocaml,norun
          (object method run = 1 end : object method run : int end)
          ```
      *)
  | LocalOpen of local_open_class_expression
      (** A locally opened class expression such as `M.(builder)` or
          `let open M in object end`.
      *)
  | Parenthesized of parenthesized_class_expression
      (** A parenthesized class expression used for grouping. *)
  | Attribute of {
      syntax_node : syntax_node;
      class_expression : class_expression;
      attribute : attribute;
    }
      (** A class expression with an attached attribute. *)
  | Extension of extension
      (** A PPX extension parsed in class-expression position.

          Example: `[%driver]`.
      *)

(** The structured payload of `ClassExpression.Structure`. *)
and class_structure = {
  syntax_node : syntax_node;
  self_pattern : pattern option;
  fields : class_field list;
}

(** Fields inside an `object ... end` class structure. *)
and class_field =
  | Method of class_method
      (** A `method` declaration inside a class structure. *)
  | Value of class_value
      (** A `val` declaration inside a class structure. *)
  | Inherit of class_inherit
      (** An `inherit class_expr` field. *)
  | Constraint of class_constraint
      (** A `constraint t = u` field. *)
  | Initializer of class_initializer
      (** An `initializer expr` field. *)
  | Attribute of {
      syntax_node : syntax_node;
      field : class_field;
      attribute : attribute;
    }
      (** A class field with an attached attribute.

          Example: `method run = 1 [@@foo]`.
      *)
  | Extension of extension
      (** A PPX extension parsed as a class field.

          Example: `[%%foo]`.
      *)

(** Payload for `ClassField.Method`.

    This covers concrete methods, virtual methods, private methods, and
    overriding methods such as `method!`.
*)
and class_method = {
  syntax_node : syntax_node;
  name_token : Token.t;
  body : expression option;
  type_ : core_type option;
  is_private : bool;
  is_virtual : bool;
  is_override : bool;
}

(** Payload for `ClassField.Value`.

    This covers class fields declared with `val`, including `mutable`,
    `virtual`, and overriding forms.
*)
and class_value = {
  syntax_node : syntax_node;
  name_token : Token.t;
  value : expression option;
  type_ : core_type option;
  is_mutable : bool;
  is_virtual : bool;
  is_override : bool;
}

(** Payload for `ClassField.Inherit`. *)
and class_inherit = {
  syntax_node : syntax_node;
  class_expression : class_expression;
}

(** Payload for `ClassField.Constraint`.

    Example: `constraint t = int`.
*)
and class_constraint = {
  syntax_node : syntax_node;
  left : core_type;
  right : core_type;
}

(** Payload for `ClassField.Initializer`.

    Covers `initializer expr`.
*)
and class_initializer = {
  syntax_node : syntax_node;
  body : expression option;
}

(** Payload for `ClassExpression.Apply`. *)
and class_apply_expression = {
  syntax_node : syntax_node;
  callee : class_expression;
  argument : apply_argument;
}

(** Payload for `ClassExpression.Fun`. *)
and class_fun_expression = {
  syntax_node : syntax_node;
  parameters : Parameter.t list;
  body : class_expression;
}

(** Payload for `ClassExpression.Let`. *)
and class_let_expression = {
  syntax_node : syntax_node;
  binding_pattern : pattern;
  bound_value : expression;
  and_bindings : let_binding list;
  body : class_expression;
  is_recursive : bool;
}

(** Payload for `ClassExpression.Constraint`. *)
and class_constraint_expression = {
  syntax_node : syntax_node;
  class_expression : class_expression;
  class_type : class_type;
}

(** Payload for `ClassExpression.LocalOpen`. *)
and local_open_class_expression = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  class_expression : class_expression;
  via_let_open : bool;
}

(** Payload for `ClassExpression.Parenthesized`. *)
and parenthesized_class_expression = {
  syntax_node : syntax_node;
  inner : class_expression;
}

(** Module expression syntax.

    These nodes cover the term-level side of the module language, including
    `struct ... end`, functors, functor application, extension nodes such as
    `[%ext]`, and unpacking first-class modules with `val`.
*)
and module_expression =
  | Path of Ident.t
      (** A module path such as `M`, `Stdlib.List`, or `Driver.Sqlite`. *)
  | Structure of {
      syntax_node : syntax_node;
      item_syntax_nodes : syntax_node list;
    }
      (** A raw `struct ... end` expression.

          The contained items are preserved as raw `syntax_node`s rather than a
          separate public structure-item tree.
      *)
  | Functor of {
      syntax_node : syntax_node;
      parameters : functor_parameter list;
      body : module_expression;
    }
      (** A functor expression.

          Example: `functor (X : S) -> struct ... end`.
      *)
  | Apply of {
      syntax_node : syntax_node;
      callee : module_expression;
      argument : module_expression;
    }
      (** A functor application such as `F(X)` or `F(X)(Y)`. *)
  | ApplyUnit of {
      syntax_node : syntax_node;
      callee : module_expression;
    }
      (** A unit functor application such as `F()` or `F()()`.

          This preserves the stock parsetree distinction between ordinary
          module application and generative application with an empty unit
          argument list.
      *)
  | Constraint of {
      syntax_node : syntax_node;
      module_expression : module_expression;
      module_type : module_type;
    }
      (** A module expression constrained by a module type.

          Examples:

          ```ocaml,norun
          module M : S = struct end
          (module Impl : S)
          ```

          Declaration-site constraints such as `module M : S = ...` are
          reconstructed from the enclosing declaration node, because the
          successful `Ceibo` parse stores the ascription there instead of on a
          dedicated module-expression node.
      *)
  | Unpack of {
      syntax_node : syntax_node;
      expression : expression;
      module_type : module_type option;
    }
      (** A first-class module unpacking expression.

          Examples:

          ```ocaml,norun
          (val m)
          (val m : S)
          ```
      *)
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : module_expression;
    }
      (** A parenthesized module expression used for grouping. *)
  | Attribute of {
      syntax_node : syntax_node;
      module_expression : module_expression;
      attribute : attribute;
    }
      (** A module expression with an attached attribute. *)
  | Extension of extension
      (** A PPX extension parsed in module-expression position.

          Example: `[%driver]`.
      *)

(** Namespace view over `expression`.

    The constructors mirror `expression` exactly, so the grammar coverage and
    examples documented on `expression` apply here unchanged.
*)
module Expression : sig
  type t = expression =
    | Path of path_expression
    | Operator of operator_expression
    | Literal of literal
    | Unreachable of unreachable_expression
    | Attribute of attribute
    | Extension of extension
    | Object of object_expression
    | PolyVariant of poly_variant_expression
    | FirstClassModule of first_class_module_expression
    | LetModule of let_module_expression
    | LetException of let_exception_expression
    | Assert of assert_expression
    | Lazy of lazy_expression
    | While of while_expression
    | For of for_expression
    | Apply of apply_expression
    | MethodCall of method_call_expression
    | New of new_expression
    | Prefix of prefix_expression
    | FieldAccess of field_access_expression
    | Index of index_expression
    | ObjectUpdate of object_update_expression
    | InstanceVariableAssign of instance_variable_assign_expression
    | Assign of assign_expression
    | Infix of infix_expression
    | Typed of typed_expression
    | Polymorphic of polymorphic_expression
    | Coerce of coerce_expression
    | Sequence of sequence_expression
    | Tuple of tuple_expression
    | List of list_expression
    | Array of array_expression
    | Record of record_expression
    | LocalOpen of local_open_expression
    | Fun of fun_expression
    | Function of function_expression
    | LetOperator of let_operator_expression
    | Let of let_expression
    | Match of match_expression
    | Try of try_expression
    | If of if_expression
    | Parenthesized of parenthesized_expression

  val syntax_node : t -> syntax_node
end

(** Namespace view over `object_member`. *)
module ObjectMember : sig
  type t = object_member =
    | Method of object_method
    | Value of object_value
    | Inherit of object_inherit
    | Extension of extension
    | Initializer of object_initializer

  val syntax_node : t -> syntax_node
end

(** Namespace view over `class_expression`.

    The constructors mirror `class_expression` exactly, so the grammar
    coverage and examples documented on `class_expression` apply here
    unchanged.
*)
module ClassExpression : sig
  type t = class_expression =
    | Path of Ident.t
    | Structure of class_structure
    | Fun of class_fun_expression
    | Apply of class_apply_expression
    | Let of class_let_expression
    | Constraint of class_constraint_expression
    | LocalOpen of local_open_class_expression
    | Parenthesized of parenthesized_class_expression
    | Attribute of {
        syntax_node : syntax_node;
        class_expression : class_expression;
        attribute : attribute;
      }
    | Extension of extension

  val syntax_node : t -> syntax_node
end

(** Namespace view over `class_field`. *)
module ClassField : sig
  type t = class_field =
    | Method of class_method
    | Value of class_value
    | Inherit of class_inherit
    | Constraint of class_constraint
    | Initializer of class_initializer
    | Attribute of {
        syntax_node : syntax_node;
        field : class_field;
        attribute : attribute;
      }
    | Extension of extension

  val syntax_node : t -> syntax_node
end

(** Namespace view over `module_expression`.

    The constructors mirror `module_expression` exactly, so the grammar
    coverage and examples documented on `module_expression` apply here
    unchanged.
*)
module ModuleExpression : sig
  type t = module_expression =
    | Path of Ident.t
    | Structure of {
        syntax_node : syntax_node;
        item_syntax_nodes : syntax_node list;
      }
    | Functor of {
        syntax_node : syntax_node;
        parameters : functor_parameter list;
        body : module_expression;
      }
    | Apply of {
        syntax_node : syntax_node;
        callee : module_expression;
        argument : module_expression;
      }
    | ApplyUnit of {
        syntax_node : syntax_node;
        callee : module_expression;
      }
    | Constraint of {
        syntax_node : syntax_node;
        module_expression : module_expression;
        module_type : module_type;
      }
    | Unpack of {
        syntax_node : syntax_node;
        expression : expression;
        module_type : module_type option;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : module_expression;
      }
    | Attribute of {
        syntax_node : syntax_node;
        module_expression : module_expression;
        attribute : attribute;
      }
    | Extension of extension

  val syntax_node : t -> syntax_node
end

(** Namespace view over `pattern`.

    The constructors mirror `pattern` exactly, so the grammar coverage and
    examples documented on `pattern` apply here unchanged.
*)
module Pattern : sig
  type t = pattern =
    | Identifier of identifier_pattern
    | Wildcard of wildcard_pattern
    | Attribute of attributed_pattern
    | Extension of extension
    | Literal of pattern_literal
    | Lazy of lazy_pattern
    | Exception of exception_pattern
    | Range of range_pattern
    | Operator of operator_pattern
    | FirstClassModule of first_class_module_pattern
    | PolyVariant of poly_variant_pattern
    | PolyVariantInherit of poly_variant_inherit_pattern
    | Constructor of constructor_pattern
    | Tuple of tuple_pattern
    | List of list_pattern
    | Array of array_pattern
    | Record of record_pattern
    | Cons of cons_pattern
    | Or of or_pattern
    | Alias of alias_pattern
    | Typed of typed_pattern
    | Effect of effect_pattern
    | LocalOpen of local_open_pattern
    | Parenthesized of parenthesized_pattern

  val syntax_node : t -> syntax_node
end

(** Helper view over `infix_expression`.

    This module is convenient when callers specifically want to work with infix
    operators and use helper accessors such as `operator`.
*)
module InfixExpression : sig
  type t = infix_expression = {
    syntax_node : syntax_node;
    left : expression;
    operator_token : Token.t;
    right : expression;
  }

  val syntax_node : t -> syntax_node
  val left : t -> Expression.t
  val operator_token : t -> Token.t
  val operator : t -> string
  val right : t -> Expression.t
end

(** Helper view over `record_expression`.

    The constructors mirror `record_expression` exactly, so the literal/update
    distinction documented above applies here unchanged.
*)
module RecordExpression : sig
  type t = record_expression =
    | Literal of record_literal_expression
    | Update of record_update_expression

  val syntax_node : t -> syntax_node
end

(** A type variable as it appears in a declaration parameter list.

    Examples include `'a` in `type 'a t = ...` and `_` in `type _ t = ...`.
*)
module TypeVariable : sig
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
  }

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t
  val name : t -> string
  val text : t -> string
end

(** A single type parameter in a type or class declaration.

    This keeps the parameter's declared variance and injectivity flags together
    with the written variable token.

    Examples include `+'a` in `type +'a t = ...`, `!'a`-style injective
    parameters, and `_` in `type _ t = ...`.
*)
module TypeParameterVariance : sig
  type t =
    | Covariant of {
        marker_token : Token.t;
      }
        (** A `+` variance marker, as in `type +'a t = ...`. *)
    | Contravariant of {
        marker_token : Token.t;
      }
        (** A `-` variance marker, as in `type -'a sink = ...`. *)

  val marker_token : t -> Token.t
end

module TypeParameter : sig
  type t = {
    syntax_node : syntax_node;
    variance : TypeParameterVariance.t option;
    is_injective : bool;
    type_variable : TypeVariable.t option;
  }

  val syntax_node : t -> syntax_node
  val variance : t -> TypeParameterVariance.t option
  val is_injective : t -> bool
  val type_variable : t -> TypeVariable.t option
end

(** A field inside a record type definition.

    Covers entries such as `name : string`, `mutable count : int`, and
    `count : int [@deprecated]`.
*)
module RecordField : sig
  type t = {
    syntax_node : syntax_node;
    field_name : Token.t;
    field_type : core_type;
    is_mutable : bool;
    attributes : attribute list;
  }

  val syntax_node : t -> syntax_node
  val field_name_token : t -> Token.t
  val field_type : t -> core_type
  val name : t -> string
  val is_mutable : t -> bool
  val attributes : t -> attribute list
end

(** Structured argument forms for regular variant constructors.

    These correspond to the `of ...` payload on constructors such as
    `Pair of int * string` and `Person of { name : string }`.
*)
module ConstructorArguments : sig
  type t =
    | Tuple of core_type list
        (** Positional constructor arguments.

            Examples:

            ```ocaml,norun
            type t = Pair of int * string
            type t = Wrapped of (int * string)
            ```
        *)
    | Record of RecordField.t list
        (** Inline record constructor arguments.

            Example:

            ```ocaml,norun
            type t = Person of { name : string; age : int }
            ```
        *)
end

(** A constructor inside a regular variant type definition.

    Examples:

    ```ocaml,norun
    type t = A | B of int
    type t = Pair of int * string
    type t = Person of { name : string; age : int }
    ```
*)
module VariantConstructor : sig
  type t = {
    syntax_node : syntax_node;
    constructor_name : Token.t;
    arguments : ConstructorArguments.t option;
    payload_type : core_type option;
  }

  val syntax_node : t -> syntax_node
  val constructor_name_token : t -> Token.t
  val arguments : t -> ConstructorArguments.t option
  val payload_type : t -> core_type option
  val name : t -> string
end

(** Helper view over `poly_variant_tag`.

    The structure matches `poly_variant_tag` exactly and is typically used while
    inspecting polymorphic variant type definitions.
*)
module PolyVariantTag : sig
  type t = poly_variant_tag = {
    syntax_node : syntax_node;
    tag_name : Token.t;
    payload_type : core_type option;
  }

  val syntax_node : t -> syntax_node
  val tag_name_token : t -> Token.t
  val payload_type : t -> core_type option
  val name : t -> string
end

(** Helper view over `poly_variant_bound`. *)
module PolyVariantBound : sig
  type t = poly_variant_bound =
    | Exact
    | UpperBound of {
        marker_token : Token.t;
      }
    | LowerBound of {
        marker_token : Token.t;
      }

  val marker_token : t -> Token.t option
end

(** Helper view over `row_field`.

    This distinguishes explicit variant tags from inherited rows such as
    `color` in `[ color | `Yellow ]`.
*)
module RowField : sig
  type t = row_field =
    | Tag of poly_variant_tag
    | Inherit of {
        syntax_node : syntax_node;
        type_ : core_type;
      }

  val syntax_node : t -> syntax_node
  val tag : t -> PolyVariantTag.t option
  val inherited_type : t -> core_type option
end

(** Helper view over `poly_variant`. *)
module PolyVariant : sig
  type t = poly_variant = {
    syntax_node : syntax_node;
    kind : poly_variant_bound;
    fields : row_field list;
  }

  val syntax_node : t -> syntax_node
  val kind : t -> PolyVariantBound.t
  val fields : t -> RowField.t list
  val tags : t -> PolyVariantTag.t list
end

(** The right-hand side of a `type` declaration.

    This is intentionally a broad summary layer over the successful parse. The
    most common declaration shapes are modeled directly, while rarer or more
    complex forms fall back to `Other`.
*)
module TypeDefinition : sig
  type t =
    | Abstract
        (** An abstract declaration with no manifest.

            Example:

            ```ocaml,norun
            type t
            ```
        *)
    | Alias of {
        syntax_node : syntax_node;
        manifest : core_type;
      }
        (** A manifest alias.

            Examples:

            ```ocaml,norun
            type t = int
            type 'a t = 'a list
            ```
        *)
    | Extensible of {
        syntax_node : syntax_node;
      }
        (** An extensible variant declaration introduced with `= ..`.

            Example:

            ```ocaml,norun
            type t = ..
            ```
        *)
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type : module_type;
      }
        (** A manifest first-class module type.

            Example: `type handler = (module Handler with type t = int)`.
        *)
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }
        (** An object type definition such as `type t = < run : unit -> unit >`. *)
    | Record of RecordField.t list
        (** A record type definition such as
            `type t = { name : string; mutable count : int }`. *)
    | Variant of VariantConstructor.t list
        (** A regular algebraic variant definition such as
            `type t = A | B of int`. *)
    | PolyVariant of PolyVariant.t
        (** A polymorphic variant definition such as
            `type t = [ `A | `B of int ]` or
            `type t = [ color | `Yellow ]`.
        *)
    | Other of syntax_node
        (** A successfully parsed type definition whose exact grammar is not yet
            given a dedicated public constructor.

            This is where consumers should expect richer shapes such as
            unsupported `private` or GADT-style definitions to remain
            accessible only through the raw `syntax_node`.
        *)
end

(** A `type` declaration item.

    Examples:

    ```ocaml,norun
    type t = int
    type ('a, 'b) pair = 'a * 'b
    type t := string
    ```
*)
module TypeDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    type_name : Ident.t;
    type_params : TypeParameter.t list;
    type_definition : TypeDefinition.t;
    constraints : type_constraint list;
    is_destructive_substitution : bool;
  }

  val syntax_node : t -> syntax_node
  val type_name : t -> Ident.t
  val type_params : t -> TypeParameter.t list
  val type_definition : t -> TypeDefinition.t
  val constraints : t -> TypeConstraint.t list
  (** `true` for interface destructive substitutions such as `type t := string`. *)
  val is_destructive_substitution : t -> bool
  val name_token : t -> Token.t
end

(** A `type ... += ...` extension item.

    Examples:

    ```ocaml,norun
    type _ Effect.t += Yield : unit Effect.t
    type message += Ack
    ```
*)
module TypeExtension : sig
  type t = {
    syntax_node : syntax_node;
    type_name : Ident.t;
    type_params : TypeParameter.t list;
    constructors : VariantConstructor.t list;
  }

  val syntax_node : t -> syntax_node
  val type_name : t -> Ident.t
  val type_params : t -> TypeParameter.t list
  val constructors : t -> VariantConstructor.t list
  val name_token : t -> Token.t
end

(** Helper view over `let_binding`.

    This is useful both for top-level `let` items and for nested `and` bindings
    collected from expression forms.
*)
module LetBinding : sig
  type t = let_binding = {
    syntax_node : syntax_node;
    attributes : attribute list;
    binding_pattern : pattern;
    binding_name : Token.t option;
    parameters : Parameter.t list;
    value : expression;
    is_recursive : bool;
  }

  val syntax_node : t -> syntax_node
  val attributes : t -> attribute list
  val binding_pattern : t -> Pattern.t
  val binding_name_token : t -> Token.t option
  val name : t -> string
  val parameters : t -> Parameter.t list
  val value : t -> Expression.t
  val value_syntax_node : t -> syntax_node
  val is_recursive : t -> bool
  val is_function : t -> bool
end

(** A single module binding.

    This covers plain `module` items and the individual bindings nested under a
    recursive module item. `is_recursive` reports whether the binding belongs to
    a `module rec` group, while `is_destructive_substitution` distinguishes
    interface substitutions such as `module Name := Path`.

    Examples:

    ```ocaml,norun
    module M = N
    module Alias := Stdlib.List
    module F (X : S) : T = struct end
    ```
*)
module ModuleDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    module_name : Token.t;
    functor_parameters : functor_parameter list;
    module_type : module_type option;
    module_expression : module_expression option;
    is_destructive_substitution : bool;
    is_recursive : bool;
  }

  val syntax_node : t -> syntax_node
  val module_name_token : t -> Token.t
  val functor_parameters : t -> functor_parameter list
  val module_type : t -> module_type option
  val module_expression : t -> module_expression option
  (** `true` for destructive substitutions such as `module Alias := M`. *)
  val is_destructive_substitution : t -> bool
  val is_recursive : t -> bool
  val name : t -> string
end

(** A recursive module structure item.

    The grouped `declarations` preserve source order for bindings introduced by
    `module rec ... and ...`.

    Example:

    ```ocaml,norun
    module rec A : S = X
    and B : T = Y
    ```
*)
module RecursiveModuleDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    declarations : ModuleDeclaration.t list;
  }

  val syntax_node : t -> syntax_node
  val declarations : t -> ModuleDeclaration.t list
end

(** A `module type` declaration item.

    Examples:

    ```ocaml,norun
    module type S
    module type S = sig type t end
    module type Alias := Source
    ```
*)
module ModuleTypeDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    module_type_name : Token.t;
    module_type : module_type option;
    is_destructive_substitution : bool;
  }

  val syntax_node : t -> syntax_node
  val module_type_name_token : t -> Token.t
  val module_type : t -> module_type option
  (** `true` for destructive substitutions such as `module type Alias := S`. *)
  val is_destructive_substitution : t -> bool
  val name : t -> string
end

(** An `open` statement item.

    Covers both `open M` and `open! M`.
*)
module OpenStatement : sig
  type t = {
    syntax_node : syntax_node;
    module_path : Ident.t;
    bang_token : Token.t option;
  }

  val syntax_node : t -> syntax_node
  val module_path : t -> Ident.t
  val bang_token : t -> Token.t option
  val has_bang : t -> bool
end

(** A `val` declaration item.

    Examples:

    ```ocaml,norun
    val make : unit -> t
    val ( + ) : int -> int -> int
    ```
*)
type value_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
  type_ : core_type;
}

(** An `external` declaration item.

    Example:

    ```ocaml,norun
    external strlen : string -> int = "caml_strlen"
    ```
*)
type external_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
  type_ : core_type;
  primitive_name_tokens : Token.t list;
}

(** A `class` declaration item.

    `class_type` is present for signature-style declarations such as
    `class c : object ... end`. `class_body` is present for implementation
    bindings such as `class c = object ... end`.
*)
type class_declaration = {
  syntax_node : syntax_node;
  type_params : TypeParameter.t list;
  class_name : Token.t;
  class_type : class_type option;
  class_body : class_expression option;
}

(** A `class type` declaration item.

    Example:

    ```ocaml,norun
    class type service = object method run : unit -> unit end
    ```
*)
type class_type_declaration = {
  syntax_node : syntax_node;
  type_params : TypeParameter.t list;
  class_type_name : Token.t;
  class_type_body : class_type;
}

(** The payload of an `include` item. *)
type include_target =
  | ModuleExpression of module_expression
      (** An included module expression such as `include M` or `include F(X)`. *)
  | ModuleType of module_type
      (** An included module type such as `include S` inside a signature. *)

(** An `include` item. *)
type include_statement = {
  syntax_node : syntax_node;
  target : include_target;
}

(** Top-level items collected from a source file. *)
module Item : sig
  type t =
    | TypeDeclaration of TypeDeclaration.t
        (** A `type` declaration item. *)
    | TypeExtension of TypeExtension.t
        (** A `type ... += ...` extension item. *)
    | LetBinding of LetBinding.t
        (** A `let` or `let rec` item. *)
    | Expression of Expression.t
        (** A standalone expression item, typically in implementation files. *)
    | Attribute of attribute
        (** A floating attribute item such as `[@@@warning "-32"]`. *)
    | Extension of extension
        (** A floating extension item. *)
    | ClassDeclaration of class_declaration
        (** A `class` declaration item. *)
    | ClassTypeDeclaration of class_type_declaration
        (** A `class type` declaration item. *)
    | ModuleDeclaration of ModuleDeclaration.t
        (** A non-recursive `module` declaration item. *)
    | RecursiveModuleDeclaration of RecursiveModuleDeclaration.t
        (** A `module rec ... and ...` item. *)
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
        (** A `module type` declaration item. *)
    | OpenStatement of OpenStatement.t
        (** An `open` item. *)
    | ValueDeclaration of value_declaration
        (** A `val` declaration item. *)
    | ExternalDeclaration of external_declaration
        (** An `external` declaration item. *)
    | IncludeStatement of include_statement
        (** An `include` item. *)
    | ExceptionDeclaration of exception_declaration
        (** An `exception` declaration item. *)

  val syntax_node : t -> syntax_node
end

(** A parsed implementation source file.

    `items` keeps the original item ordering, while `let_bindings` and
    `expressions` provide flattened convenience views collected from the tree.
*)
type implementation = {
  syntax_node : syntax_node;
  items : Item.t list;
  let_bindings : LetBinding.t list;
  expressions : Expression.t list;
}

(** A parsed interface source file.

    The same convenience views as `implementation` are exposed for symmetry,
    even though many interfaces will naturally have fewer nested expressions.
*)
type interface = {
  syntax_node : syntax_node;
  items : Item.t list;
  let_bindings : LetBinding.t list;
  expressions : Expression.t list;
}

(** A parsed source file, distinguished by whether the grammar was an
    implementation or an interface. *)
type t =
  | Implementation of implementation
      (** An implementation file such as an `.ml`. *)
  | Interface of interface
      (** An interface file such as an `.mli`. *)

(** Alias for the full-file CST root. *)
type source_file = t

(** Namespace helpers for `source_file`. *)
module SourceFile : sig
  type t = source_file

  val syntax_node : t -> syntax_node
  val items : t -> Item.t list
  val let_bindings : t -> LetBinding.t list
  val expressions : t -> Expression.t list
  val kind : t -> [ `Implementation | `Interface ]
end

(** Returns the root `syntax_node` for the parsed source file. *)
val syntax_node_of_source_file : source_file -> syntax_node
