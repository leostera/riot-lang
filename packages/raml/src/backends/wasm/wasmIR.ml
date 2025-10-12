open Std

module Identifier = Typechecker.Identifier

type wasm_type = I32 | I64 | F32 | F64 | FuncRef | AnyRef

type mem_op =
  | Load of { ty : wasm_type; offset : int; align : int }
  | Store of { ty : wasm_type; offset : int; align : int }

type wasm_binop =
  | Add | Sub | Mul | Div_s | Div_u | Rem_s | Rem_u
  | And | Or | Xor | Shl | Shr_s | Shr_u
  | Eq | Ne | Lt_s | Lt_u | Le_s | Le_u | Gt_s | Gt_u | Ge_s | Ge_u

type wasm_unop = Clz | Ctz | Popcnt | Neg | Abs | Sqrt

type wasm_const = 
  | ConstI32 of int32
  | ConstI64 of int64
  | ConstF32 of float
  | ConstF64 of float

type wasm_func_type = {
  params : wasm_type list;
  results : wasm_type list;
}

type wasm_instr =
  | Const of wasm_const
  | GetLocal of Identifier.t
  | SetLocal of Identifier.t
  | TeeLocal of Identifier.t
  | GetGlobal of string
  | SetGlobal of string
  | MemoryOp of mem_op
  | MemorySize
  | MemoryGrow
  | BinOp of wasm_type * wasm_binop
  | UnOp of wasm_type * wasm_unop
  | Block of { label : string; result_type : wasm_type option; body : wasm_instr list }
  | Loop of { label : string; body : wasm_instr list }
  | If of { result_type : wasm_type option; then_ : wasm_instr list; else_ : wasm_instr list option }
  | Br of string
  | BrIf of string
  | BrTable of { labels : string list; default : string }
  | Return
  | Call of string
  | CallIndirect of wasm_func_type
  | Drop
  | Select
  | Convert of { from : wasm_type; to_ : wasm_type }

type wasm_func = {
  name : string;
  func_type : wasm_func_type;
  locals : (Identifier.t * wasm_type) list;
  body : wasm_instr list;
}

type wasm_global = { 
  name : string; 
  ty : wasm_type; 
  mutable_ : bool; 
  init : wasm_const;
}

type wasm_export =
  | ExportFunc of string
  | ExportGlobal of string
  | ExportMemory
  | ExportTable

type wasm_import =
  | ImportFunc of wasm_func_type
  | ImportGlobal of wasm_type * bool
  | ImportMemory of int
  | ImportTable of int

type wasm_module = {
  types : wasm_func_type list;
  funcs : wasm_func list;
  globals : wasm_global list;
  memory : int option;
  table : int option;
  exports : (string * wasm_export) list;
  imports : (string * string * wasm_import) list;
  start : string option;
}

let wasm_type_to_string = function
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"
  | FuncRef -> "funcref"
  | AnyRef -> "anyref"

let wasm_instr_to_string = function
  | Const (ConstI32 i) -> format "i32.const %ld" i
  | Const (ConstI64 i) -> format "i64.const %Ld" i
  | Const (ConstF32 f) -> format "f32.const %f" f
  | Const (ConstF64 f) -> format "f64.const %f" f
  | GetLocal id -> format "local.get $%s" (Identifier.name id)
  | SetLocal id -> format "local.set $%s" (Identifier.name id)
  | BinOp (I32, Add) -> "i32.add"
  | BinOp (I32, Sub) -> "i32.sub"
  | Return -> "return"
  | _ -> "???"

let wasm_func_to_string (func : wasm_func) =
  format "(func $%s)" func.name

let wasm_module_to_string _module =
  "(module)"

let translate_from_lambda (expr : Lambda.Ir.lambda) : wasm_module =
  let locals_ref = ref [] in
  
  let rec collect_locals expr =
    match expr with
    | Lambda.Ir.Let { id; value; body; _ } ->
        if not (List.exists (fun (local_id, _) -> Identifier.equal local_id id) !locals_ref) then
          locals_ref := (id, I32) :: !locals_ref;
        collect_locals value;
        collect_locals body
    | Lambda.Ir.Prim (_, args) -> List.iter collect_locals args
    | _ -> ()
  in
  
  let rec translate_expr expr : wasm_instr list =
    match expr with
    | Lambda.Ir.Const c -> translate_const c
    | Lambda.Ir.Var id -> [GetLocal id]
    | Lambda.Ir.Prim (op, args) -> translate_prim op args
    | Lambda.Ir.Let { id; value; body; _ } ->
        let value_instrs = translate_expr value in
        let body_instrs = translate_expr body in
        value_instrs @ [SetLocal id] @ body_instrs
    | _ -> 
        [Const (ConstI32 42l)]
  
  and translate_const = function
    | Lambda.Ir.Const_int i -> [Const (ConstI32 (Int32.of_int i))]
    | Lambda.Ir.Const_float f -> [Const (ConstF64 f)]
    | Lambda.Ir.Const_string _s -> [Const (ConstI32 0l)]
    | Lambda.Ir.Const_block (_tag, _fields) -> [Const (ConstI32 0l)]
  
  and translate_prim op args =
    let arg_instrs = List.concat (List.map translate_expr args) in
    let op_instr = match op with
      | Lambda.Ir.Pint_add -> [BinOp (I32, Add)]
      | Lambda.Ir.Pint_sub -> [BinOp (I32, Sub)]
      | Lambda.Ir.Pint_mul -> [BinOp (I32, Mul)]
      | Lambda.Ir.Pint_div -> [BinOp (I32, Div_s)]
      | Lambda.Ir.Pint_mod -> [BinOp (I32, Rem_s)]
      | Lambda.Ir.Pint_neg -> [Const (ConstI32 0l)] @ arg_instrs @ [BinOp (I32, Sub)]
      | _ -> [Const (ConstI32 0l)]
    in
    arg_instrs @ op_instr
  in
  
  collect_locals expr;
  let body = translate_expr expr in
  let func_type = { params = []; results = [I32] } in
  let main_func = {
    name = "main";
    func_type;
    locals = List.rev !locals_ref;
    body = body @ [Return];
  } in
  
  {
    types = [func_type];
    funcs = [main_func];
    globals = [];
    memory = Some 1;
    table = None;
    exports = [("main", ExportFunc "main")];
    imports = [];
    start = None;
  }
