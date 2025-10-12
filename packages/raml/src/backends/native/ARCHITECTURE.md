# Native Backend Architecture

The Native backend provides a **NativeLambda** IR that corresponds to OCaml's **Clambda** (Closed Lambda).

## Purpose

NativeLambda is an intermediate representation between Lambda IR and native assembly that makes closure conversion and memory layout decisions explicit.

## Pipeline

```
Lambda IR → NativeLambda → Assembly (ARM64/x86_64/RISC-V)
```

## Key Differences from Lambda IR

### 1. Explicit Closures

**Lambda IR (implicit closures):**
```ocaml
Function { params = [x]; body = Add(x, y) }  (* y is free variable *)
```

**NativeLambda (explicit closures):**
```ocaml
Uclosure {
  functions = [{ label = "fun_1"; params = [x]; body = ... }];
  free_vars = [y]  (* Captured environment explicit! *)
}
```

### 2. Direct vs Generic Application

**Lambda IR (all applications look the same):**
```ocaml
Apply { func = f; args = [x, y] }
```

**NativeLambda (direct vs indirect explicit):**
```ocaml
Udirect_apply ("fun_label", [x; y])     (* Direct call - no indirection *)
Ugeneric_apply (closure_var, [x; y])    (* Indirect call through closure *)
```

### 3. Memory Layout Decisions

**Lambda IR (abstract):**
```ocaml
Const_block of int * constant list  (* Abstract representation *)
```

**NativeLambda (explicit layout):**
```ocaml
Pmakeblock (tag, Mutable/Immutable)  (* Explicit heap allocation *)
Pfield n                              (* Field access at offset n *)
Psetfield (n, is_ptr)                (* Field mutation with GC info *)
```

## Ulambda Type

The `ulambda` type is the core of NativeLambda:

```ocaml
type ulambda =
  | Uvar of variable                                 (* Variable reference *)
  | Uconst of constant                               (* Constants *)
  | Udirect_apply of function_label * ulambda list   (* Direct function call *)
  | Ugeneric_apply of ulambda * ulambda list         (* Closure application *)
  | Uclosure of closure                              (* Closure creation *)
  | Uoffset of ulambda * int                         (* Pointer arithmetic *)
  | Ulet of variable * ulambda * ulambda             (* Let binding *)
  | Uletrec of (variable * ulambda) list * ulambda   (* Recursive bindings *)
  | Uprim of primitive * ulambda list                (* Primitives *)
  | Uswitch of ulambda * ulambda_switch              (* Pattern match (compiled) *)
  | Uifthenelse of ulambda * ulambda * ulambda       (* Conditional *)
  | Usequence of ulambda * ulambda                   (* Sequencing *)
  | Uwhile of ulambda * ulambda                      (* While loop *)
  | Ufor of variable * ulambda * ulambda * direction_flag * ulambda  (* For loop *)
  | Uassign of variable * ulambda                    (* Assignment *)
  | Utrywith of ulambda * variable * ulambda         (* Exception handling *)
  | ...
```

## Variables

Variables in NativeLambda have explicit IDs for renaming:

```ocaml
type variable = {
  var_name : string;   (* Original name *)
  var_id : int;        (* Unique identifier *)
}
```

This allows alpha-renaming and SSA-like transformations.

## Closure Representation

```ocaml
type closure_function = {
  label : function_label;     (* Unique label for direct calls *)
  arity : int;                (* Number of parameters *)
  params : variable list;     (* Parameter variables *)
  body : ulambda;             (* Function body *)
  dbg : string option;        (* Debug info *)
}

type closure = {
  functions : closure_function list;  (* Mutually recursive functions *)
  free_vars : variable list;          (* Captured environment *)
}
```

**Why a list of functions?** Mutually recursive functions can share the same closure environment:

```ocaml
let rec f x = g x
and g y = f y
```

Both `f` and `g` are in the same closure with no free variables.

## Primitives

NativeLambda has many low-level primitives that Lambda IR doesn't expose:

### Memory Operations
- `Pmakeblock (tag, mutability)` - Heap allocation
- `Pfield n` - Load field at offset n
- `Psetfield (n, is_ptr)` - Store field (with GC barrier if pointer)
- `Poffsetref n` - In-place increment
- `Pduprecord n` - Copy record

### Integer/Float Operations
- `Paddint`, `Psubint`, `Pmulint`, `Pdivint`, `Pmodint`
- `Paddfloat`, `Psubfloat`, `Pmulfloat`, `Pdivfloat`
- `Pintcomp`, `Pfloatcomp` - Comparisons

### Bitwise Operations
- `Pandint`, `Porint`, `Pxorint`
- `Plslint`, `Plsrint`, `Pasrint` - Shifts

### String/Bytes Operations
- `Pstringlength`
- `Pstringrefu`, `Pstringsetu` - Unsafe access
- `Pstringrefs`, `Pstringsets` - Safe access
- `Pstring_load_16/32/64`, `Pbytes_load_16/32/64`

### Boxed Integers
- `Pbintofint`, `Pintofbint` - Boxing/unboxing
- Operations for `int32`, `int64`, `nativeint`

### Advanced Features
- `Pccall` - C function calls
- `Praise` - Exception raising
- `Plazyforce` - Force lazy values
- `Pbigarrayref/set` - Bigarray operations

## Pattern Match Compilation

Lambda IR has high-level `Match` nodes. NativeLambda compiles these to:

```ocaml
type ulambda_switch = {
  us_index_consts : int array;       (* Index for constant cases *)
  us_actions_consts : ulambda array; (* Actions for constants *)
  us_index_blocks : int array;       (* Index for block cases *)
  us_actions_blocks : ulambda array; (* Actions for blocks *)
}
```

This is a **decision tree** representation that's efficient for code generation.

## Comparison with OCaml's Clambda

| Feature | OCaml Clambda | RAML NativeLambda |
|---------|---------------|-------------------|
| **Purpose** | Native code IR | Native code IR |
| **Closures** | Explicit | Explicit |
| **Applications** | Direct/Generic split | Direct/Generic split |
| **Variables** | Ident.t | variable record |
| **Memory ops** | Very detailed | Simplified subset |
| **Source** | From Lambda | From Lambda |
| **Target** | CMM (imperative IR) | Assembly (direct) |

**Key difference:** We skip CMM and go directly to assembly, so NativeLambda is our lowest-level IR.

## Transformation: Lambda → NativeLambda

The `from_lambda` function performs:

1. **Closure conversion** - Identify free variables, create closures
2. **Application splitting** - Distinguish direct vs generic calls
3. **Variable renaming** - Assign unique IDs
4. **Primitive lowering** - Map high-level to low-level primitives
5. **Pattern match compilation** - Convert Match to Uswitch (TODO)

## Usage in Native Backends

ARM64/x86_64 backends will use NativeLambda:

```ocaml
(* Before: Lambda IR → Assembly *)
let compile_lambda lam = 
  compile_to_asm lam  (* Had to handle closures in backend *)

(* After: Lambda IR → NativeLambda → Assembly *)
let compile_lambda lam =
  let ulam = NativeLambda.from_lambda lam in
  compile_to_asm ulam  (* Closures already explicit! *)
```

## Benefits

1. **Shared closure conversion** - Don't reimplement for each architecture
2. **Cleaner backends** - Assembly generation doesn't deal with high-level concepts
3. **Better optimization opportunities** - Can optimize at NativeLambda level
4. **Easier debugging** - Can pretty-print NativeLambda IR

## Future Work

- [ ] Implement pattern match compilation (Match → Uswitch)
- [ ] Add closure optimization (eliminate unused free variables)
- [ ] Implement tail call optimization
- [ ] Add inline expansion at this level
- [ ] Integrate with ARM64/x86_64 code generation
