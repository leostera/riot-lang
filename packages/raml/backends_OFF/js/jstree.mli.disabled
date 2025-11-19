open Std

(** {1 JsTree - JavaScript Abstract Syntax Tree}

    JsTree is a 1:1 representation of JavaScript syntax.

    Pipeline: TypedTree → Lambda → Jambda → JsTree → JS

    JsTree is the final IR before code generation. It represents JavaScript code
    structurally, making it easy to:
    - Pretty-print with correct syntax
    - Apply JS-specific optimizations (constant folding, dead code elimination)
    - Generate source maps

    Unlike Jambda (which is still functional/high-level), JsTree is imperative
    and matches JavaScript semantics exactly. *)

(** {2 JavaScript Identifiers} *)

type js_ident = string
(** JavaScript identifier - already mangled/escaped *)

(** {2 JavaScript Literals} *)

type js_literal =
  | JsNum of float  (** Number literal: 42, 3.14, NaN, Infinity *)
  | JsStr of string  (** String literal: "hello" *)
  | JsBool of bool  (** Boolean: true, false *)
  | JsNull  (** null *)
  | JsUndefined  (** undefined *)

(** {2 JavaScript Operators} *)

type js_unary_op =
  | JsNot (* ! *)
  | JsNeg (* - *)
  | JsTypeof (* typeof *)
  | JsVoid (* void *)

type js_binary_op =
  (* Arithmetic *)
  | JsAdd
  | JsSub
  | JsMul
  | JsDiv
  | JsMod
  (* Comparison *)
  | JsEq
  | JsNeq
  | JsStrictEq
  | JsStrictNeq
  | JsLt
  | JsLe
  | JsGt
  | JsGe
  (* Logical *)
  | JsAnd
  | JsOr
  (* Bitwise *)
  | JsBitAnd
  | JsBitOr
  | JsBitXor
  | JsLShift
  | JsRShift
  | JsURShift

(** {2 JavaScript Expressions} *)

type js_expr =
  | JsLit of js_literal  (** Literal value *)
  | JsId of js_ident  (** Identifier *)
  | JsArray of js_expr list  (** Array literal: [1, 2, 3] *)
  | JsObj of (string * js_expr) list  (** Object literal: { x: 1, y: 2 } *)
  | JsFun of {
      name : js_ident option;
          (** Optional function name (for named functions) *)
      params : js_ident list;
      body : js_block;
    }  (** Function expression: function(x, y) { ... } *)
  | JsArrow of { params : js_ident list; body : js_arrow_body }
      (** Arrow function: (x, y) => expr or (x, y) => { ... } *)
  | JsCall of { func : js_expr; args : js_expr list }
      (** Function call: f(a, b) *)
  | JsNew of { constructor : js_expr; args : js_expr list }
      (** Constructor call: new Foo(a, b) *)
  | JsMember of js_expr * string  (** Member access: obj.field *)
  | JsIndex of js_expr * js_expr  (** Computed member: obj[key] *)
  | JsUnary of js_unary_op * js_expr  (** Unary operation: !x, -x, typeof x *)
  | JsBinary of js_binary_op * js_expr * js_expr
      (** Binary operation: x + y, x === y *)
  | JsCond of js_expr * js_expr * js_expr
      (** Conditional: cond ? then : else *)
  | JsAssign of js_expr * js_expr  (** Assignment: x = value *)
  | JsSeq of js_expr list
      (** Sequence: (a, b, c) - evaluates all, returns last *)

(** {2 JavaScript Arrow Function Bodies} *)

and js_arrow_body =
  | JsArrowExpr of js_expr  (** Expression body: x => x + 1 *)
  | JsArrowBlock of js_block  (** Block body: x => { return x + 1; } *)

(** {2 JavaScript Statements} *)

and js_stmt =
  | JsExprStmt of js_expr  (** Expression statement: f(); *)
  | JsBlock of js_block  (** Block: { ... } *)
  | JsReturn of js_expr option  (** Return: return expr; *)
  | JsIf of js_expr * js_stmt * js_stmt option
      (** If statement: if (cond) { ... } else { ... } *)
  | JsWhile of js_expr * js_stmt  (** While loop: while (cond) { ... } *)
  | JsFor of {
      init : js_stmt option;
      test : js_expr option;
      update : js_expr option;
      body : js_stmt;
    }  (** For loop: for (init; test; update) { ... } *)
  | JsSwitch of {
      expr : js_expr;
      cases : (js_expr * js_stmt list) list;  (** case expr: stmts *)
      default : js_stmt list option;  (** default: stmts *)
    }  (** Switch statement *)
  | JsBreak  (** break; *)
  | JsContinue  (** continue; *)
  | JsThrow of js_expr  (** throw expr; *)
  | JsTry of {
      body : js_block;
      catch : (js_ident * js_block) option;
      finally : js_block option;
    }  (** Try-catch-finally *)
  | JsVarDecl of js_var_kind * js_ident * js_expr option
      (** Variable declaration: var/let/const x = expr; *)
  | JsFunDecl of { name : js_ident; params : js_ident list; body : js_block }
      (** Function declaration: function f(x) { ... } *)

(** {2 JavaScript Variable Kinds} *)

and js_var_kind =
  | JsVar (* var - function scoped *)
  | JsLet (* let - block scoped *)
  | JsConst (* const - block scoped, immutable *)

(** {2 JavaScript Blocks} *)

and js_block = js_stmt list

(** {2 JavaScript Modules} *)

type js_import =
  | JsImportNamed of (js_ident * js_ident option) list * string
      (** import { a, b as c } from "module" *)
  | JsImportDefault of js_ident * string  (** import x from "module" *)
  | JsImportNamespace of js_ident * string  (** import * as x from "module" *)

type js_export =
  | JsExportNamed of js_ident list  (** export { a, b } *)
  | JsExportDefault of js_expr  (** export default expr *)
  | JsExportStmt of js_stmt  (** export const x = 1; *)

type js_module_item =
  | JsImport of js_import
  | JsStatement of js_stmt
  | JsExport of js_export

type js_module = js_module_item list

(** {2 Translation from Jambda} *)

val translate_from_jambda : Jambda.jambda -> js_expr
(** Translate Jambda expression to JavaScript expression *)

val translate_module_from_jambda : Jambda.jambda_module -> js_module
(** Translate Jambda module to JavaScript module *)

(** {2 Code Generation} *)

val expr_to_string : js_expr -> string
(** Generate JavaScript code for an expression *)

val stmt_to_string : js_stmt -> string
(** Generate JavaScript code for a statement *)

val module_to_string : js_module -> string
(** Generate JavaScript code for a module *)
