open Std

(** {1 WasmIR - WebAssembly Intermediate Representation}

    WasmIR is a stack-based IR that closely matches WebAssembly's execution
    model.

    Pipeline: TypedTree → Lambda → WasmIR → WasmAST → Wasm binary/text

    Key features:
    - Stack-based operations (push/pop implicit)
    - Explicit memory management (load/store/alloc)
    - Structured control flow (block/loop/if)
    - Function tables for indirect calls *)

module Identifier = Typechecker.Identifier

(** {2 WebAssembly Value Types} *)

type wasm_type =
  | I32  (** 32-bit integer *)
  | I64  (** 64-bit integer *)
  | F32  (** 32-bit float *)
  | F64  (** 64-bit float *)
  | FuncRef  (** Function reference (for function tables) *)
  | AnyRef  (** Any reference (GC proposal) *)

(** {2 Memory Operations} *)

type mem_op =
  | Load of { ty : wasm_type; offset : int; align : int }
  | Store of { ty : wasm_type; offset : int; align : int }

(** {2 Binary/Unary Operations} *)

type wasm_binop =
  | Add
  | Sub
  | Mul
  | Div_s
  | Div_u
  | Rem_s
  | Rem_u
  | And
  | Or
  | Xor
  | Shl
  | Shr_s
  | Shr_u
  | Eq
  | Ne
  | Lt_s
  | Lt_u
  | Le_s
  | Le_u
  | Gt_s
  | Gt_u
  | Ge_s
  | Ge_u

type wasm_unop =
  | Clz
  | Ctz
  | Popcnt (* Count leading/trailing zeros, popcount *)
  | Neg
  | Abs
  | Sqrt (* Numeric operations *)

(** {2 WasmIR Instructions}

    These are stack-based operations. The stack is implicit. *)

type wasm_instr =
  (* Constants *)
  | Const of wasm_const  (** Push constant onto stack *)
  (* Variables *)
  | GetLocal of Identifier.t  (** Push local variable value *)
  | SetLocal of Identifier.t  (** Pop value, store in local *)
  | TeeLocal of Identifier.t
      (** Peek value, store in local (leaves value on stack) *)
  | GetGlobal of string  (** Push global variable value *)
  | SetGlobal of string  (** Pop value, store in global *)
  (* Memory operations *)
  | MemoryOp of mem_op  (** Load/Store from linear memory *)
  | MemorySize  (** Push current memory size (in pages) *)
  | MemoryGrow  (** Grow memory by N pages *)
  (* Arithmetic/Logic *)
  | BinOp of wasm_type * wasm_binop
      (** Binary operation: pop 2 values, push result *)
  | UnOp of wasm_type * wasm_unop
      (** Unary operation: pop 1 value, push result *)
  (* Control flow *)
  | Block of {
      label : string;
      result_type : wasm_type option;
      body : wasm_instr list;
    }  (** Block (can break to end) *)
  | Loop of { label : string; body : wasm_instr list }
      (** Loop (can continue to start) *)
  | If of {
      result_type : wasm_type option;
      then_ : wasm_instr list;
      else_ : wasm_instr list option;
    }  (** If-then-else: pop condition, execute branch *)
  | Br of string  (** Unconditional branch to label *)
  | BrIf of string  (** Conditional branch: pop condition, branch if non-zero *)
  | BrTable of { labels : string list; default : string }
      (** Switch: pop index, branch to labels[index] or default *)
  | Return  (** Return from function *)
  (* Function calls *)
  | Call of string  (** Direct function call *)
  | CallIndirect of wasm_func_type
      (** Indirect call via function table: pop index, call table[index] *)
  (* Stack manipulation *)
  | Drop  (** Pop and discard top of stack *)
  | Select
      (** Pop condition, then 2 values: push first if condition else second *)
  (* Type conversions *)
  | Convert of { from : wasm_type; to_ : wasm_type }
      (** Convert between types (i32→f32, i64→i32, etc.) *)

(** {2 WebAssembly Constants} *)

and wasm_const =
  | ConstI32 of int32
  | ConstI64 of int64
  | ConstF32 of float
  | ConstF64 of float

(** {2 WebAssembly Function Types} *)

and wasm_func_type = {
  params : wasm_type list;
  results : wasm_type list; (* Wasm supports multi-value return *)
}

(** {2 WebAssembly Functions} *)

type wasm_func = {
  name : string;
  func_type : wasm_func_type;
  locals : (Identifier.t * wasm_type) list; (* Local variables *)
  body : wasm_instr list;
}

(** {2 WebAssembly Globals} *)

type wasm_global = {
  name : string;
  ty : wasm_type;
  mutable_ : bool;
  init : wasm_const;
}

(** {2 WebAssembly Module} *)

type wasm_module = {
  types : wasm_func_type list;  (** Type section (function signatures) *)
  funcs : wasm_func list;  (** Function definitions *)
  globals : wasm_global list;  (** Global variables *)
  memory : int option;
      (** Memory size in pages (64KB each), None = no memory *)
  table : int option;  (** Function table size, None = no table *)
  exports : (string * wasm_export) list;  (** Exported items *)
  imports : (string * string * wasm_import) list;
      (** Imports: (module, name, type) *)
  start : string option;  (** Start function (called on module init) *)
}

and wasm_export =
  | ExportFunc of string (* Export function *)
  | ExportGlobal of string (* Export global *)
  | ExportMemory (* Export memory *)
  | ExportTable (* Export function table *)

and wasm_import =
  | ImportFunc of wasm_func_type
  | ImportGlobal of wasm_type * bool (* type, mutable *)
  | ImportMemory of int (* min pages *)
  | ImportTable of int (* size *)

(** {2 Translation from Lambda IR} *)

val translate_from_lambda : Lambda.Ir.lambda -> wasm_module
(** Translate Lambda IR to WasmIR module.

    Key transformations:
    - Function definitions → Wasm functions
    - Closures → Function table entries + environment passing
    - Let bindings → Local variables
    - Pattern matching → Branching + field access
    - Primitives → Wasm operations
    - Memory allocation for heap values *)

(** {2 Pretty Printing} *)

val wasm_type_to_string : wasm_type -> string
val wasm_instr_to_string : wasm_instr -> string
val wasm_func_to_string : wasm_func -> string
val wasm_module_to_string : wasm_module -> string
