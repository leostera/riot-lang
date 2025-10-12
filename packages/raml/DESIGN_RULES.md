# RAML Design Rules

## Critical: NO GLOBAL MUTABLE STATE

**The compiler MUST be usable as a library that can be called multiple times in the same process.**

### ❌ FORBIDDEN

```ocaml
(* Global mutable state - NEVER DO THIS *)
let type_cache = Hashtbl.create 100

let current_level = ref 0

let unique_id_counter = ref 0

let global_env = ref Env.empty

(* This breaks when called multiple times! *)
let type_check source =
  incr unique_id_counter;
  current_level := 0;
  ...
```

### ✅ CORRECT

```ocaml
(* Pass state explicitly through compilation context *)
type context = {
  type_cache: (string, type_expr) Collections.HashMap.t;
  current_level: int;
  unique_id_counter: int;
  env: env;
}

let create_context () = {
  type_cache = Collections.HashMap.create ();
  current_level = 0;
  unique_id_counter = 0;
  env = Env.empty;
}

(* Thread context through compilation *)
let type_check ~ctx source =
  let ctx = { ctx with current_level = ctx.current_level + 1 } in
  ...
```

### ✅ LOCAL MUTABLE STATE IS FINE

```ocaml
(* Mutable state during a single function call is acceptable *)
let optimize_lambda expr =
  let changed = ref false in
  let rec walk expr =
    match expr with
    | Lconst _ -> expr
    | Lapply (f, args) ->
        (* Local optimization state *)
        if can_inline f then begin
          changed := true;
          inline_function f args
        end else
          Lapply (walk f, List.map walk args)
    | ...
  in
  let result = walk expr in
  (result, !changed)
```

### ✅ PROCESS-LOCAL STATE FOR PARALLELISM

```ocaml
(* Each process/actor has its own state - isolated by design *)
open Miniriot

type worker_state = {
  module_cache: (string, typed_module) Collections.HashMap.t;
  (* Process-local mutable state is fine since processes are isolated *)
}

let rec worker_loop state =
  let selector msg =
    match msg with
    | WorkerMsg msg -> `select msg
    | _ -> `skip
  in
  match receive ~selector () with
  | TypeCheckModule (name, source) ->
      let result = TypeChecker.check ~ctx:state source in
      let state = { state with 
        module_cache = Collections.HashMap.insert 
          state.module_cache name result 
      } in
      send (self ()) (WorkerMsg (TypeCheckResult result));
      worker_loop state
```

## Implications for OCaml Compiler Port

The OCaml compiler has significant global mutable state that MUST be eliminated:

### Examples from OCaml compiler to fix:

```ocaml
(* typing/ident.ml - Global counter *)
let current_stamp = ref 0  (* ❌ GLOBAL MUTABLE *)

(* Fix: Pass through context *)
type context = {
  ...
  stamp_counter: int;
}
```

```ocaml
(* typing/btype.ml - Global type variable counters *)
let current_level = ref 0  (* ❌ GLOBAL MUTABLE *)

(* Fix: Pass through context *)
type type_context = {
  ...
  current_level: int;
}
```

```ocaml
(* typing/env.ml - Global environment cache *)
let persistent_structures = ref String.Map.empty  (* ❌ GLOBAL MUTABLE *)

(* Fix: Pass through context *)
type compilation_context = {
  ...
  persistent_structures: (string, structure) Collections.HashMap.t;
}
```

## Compilation Context Pattern

All compilation phases should accept and thread through a context:

```ocaml
(* raml.ml - Top-level API *)
type compilation_context = {
  (* Type checker state *)
  type_env: TypeChecker.Env.t;
  stamp_counter: int;
  type_level: int;
  
  (* Module system state *)
  loaded_modules: (string, typed_module) Collections.HashMap.t;
  persistent_structures: (string, structure) Collections.HashMap.t;
  
  (* Lambda state *)
  lambda_ids: int;
  
  (* Backend state *)
  backend_config: Backend.config;
}

let create_context () = {
  type_env = TypeChecker.Env.empty;
  stamp_counter = 0;
  type_level = 0;
  loaded_modules = Collections.HashMap.create ();
  persistent_structures = Collections.HashMap.create ();
  lambda_ids = 0;
  backend_config = Backend.default_config;
}

(* All phases accept and return context *)
let compile source =
  let ctx = create_context () in
  
  (* Parse - no state needed *)
  let cst = Syn.Parser.parse source |> Result.expect ~msg:"Parse failed" in
  
  (* Type check - threads context *)
  let (typed_tree, ctx) = 
    TypeChecker.check ~ctx cst 
    |> Result.expect ~msg:"Type check failed" 
  in
  
  (* Translate to Lambda - threads context *)
  let (lambda_ir, ctx) = 
    Passes.Lambda.translate ~ctx typed_tree 
  in
  
  (* Optimize - may use local mutable state *)
  let lambda_ir = Passes.optimize lambda_ir in
  
  (* Generate code - threads backend config *)
  let code = 
    Backend.ByteCode.compile ~ctx lambda_ir 
  in
  
  code
```

## Testing Multiple Compilations

Every test should verify that multiple compilations work:

```ocaml
(* tests/test_no_global_state.ml *)
let test_multiple_compilations () =
  let source1 = "let x = 1" in
  let source2 = "let y = 2" in
  
  (* Compile twice - must not interfere *)
  let result1 = Raml.compile source1 in
  let result2 = Raml.compile source2 in
  
  (* Both should succeed independently *)
  assert (Result.is_ok result1);
  assert (Result.is_ok result2);
  
  (* Compile first source again - must produce same result *)
  let result1_again = Raml.compile source1 in
  assert (result1 = result1_again)
```

## Context Threading Patterns

### Pattern 1: Immutable Updates

```ocaml
let type_expr ctx expr =
  (* Create fresh type variable - update counter *)
  let ty_var = new_type_var ctx.type_level ctx.stamp_counter in
  let ctx = { ctx with stamp_counter = ctx.stamp_counter + 1 } in
  
  (* Continue with updated context *)
  match expr with
  | Let (x, e1, e2) ->
      let (ty1, ctx) = type_expr ctx e1 in
      let ctx = { ctx with type_env = Env.add ctx.type_env x ty1 } in
      let (ty2, ctx) = type_expr ctx e2 in
      (ty2, ctx)
  | ...
```

### Pattern 2: Context Monad (Optional, for complex threading)

```ocaml
(* If context threading becomes too verbose *)
module Ctx = struct
  type 'a t = context -> ('a * context)
  
  let return x ctx = (x, ctx)
  
  let bind m f ctx =
    let (x, ctx) = m ctx in
    f x ctx
  
  let run m = m (create_context ())
  
  let get ctx = (ctx, ctx)
  
  let set ctx' _ctx = ((), ctx')
  
  let modify f ctx = ((), f ctx)
end

let ( let* ) = Ctx.bind

(* Use with do-notation style *)
let type_expr expr =
  let* ctx = Ctx.get in
  let ty_var = new_type_var ctx.type_level ctx.stamp_counter in
  let* () = Ctx.modify (fun ctx -> 
    { ctx with stamp_counter = ctx.stamp_counter + 1 }
  ) in
  match expr with
  | Let (x, e1, e2) ->
      let* ty1 = type_expr e1 in
      let* () = Ctx.modify (fun ctx ->
        { ctx with type_env = Env.add ctx.type_env x ty1 }
      ) in
      type_expr e2
  | ...
```

### Pattern 3: Scoped State Restoration

```ocaml
(* When you need to restore state after a scope *)
let with_increased_level ctx f =
  let old_level = ctx.type_level in
  let ctx = { ctx with type_level = ctx.type_level + 1 } in
  let (result, ctx) = f ctx in
  ({ ctx with type_level = old_level }, result)

let type_poly_expr ctx expr =
  with_increased_level ctx (fun ctx ->
    type_expr ctx expr
  )
```

## Summary

1. ✅ **Pass all state through explicit context parameter**
2. ✅ **Return updated context from functions that modify state**
3. ✅ **Local mutable variables during recursion are fine**
4. ✅ **Process-local state in actors is fine (isolated by design)**
5. ❌ **NEVER use global refs, global hashtables, or any module-level mutable state**

**Goal:** `Raml.compile` can be called 1000 times in the same process without any state leaking between calls.
