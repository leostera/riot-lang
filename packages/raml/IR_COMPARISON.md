# IR Comparison: Lambda vs NativeLambda

## Quick Reference

| Feature | Lambda IR | NativeLambda |
|---------|-----------|--------------|
| **Closures** | Implicit (Function node) | Explicit (Uclosure with free_vars) |
| **Function calls** | All use Apply | Udirect_apply vs Ugeneric_apply |
| **Variables** | Identifier.t (variant) | variable record with unique ID |
| **Memory ops** | High-level (Pmakeblock) | Low-level (with mutability, GC info) |
| **Pattern matching** | High-level Match | Compiled to Uswitch (decision tree) |
| **Used by** | All backends | Native backends only (ARM64, x86_64) |

## Example Transformation

### Input: OCaml Code
```ocaml
let f x = x + 1
let g = f 42
```

### Lambda IR
```ocaml
LetRec (
  [("f", Function {
    params = [x];
    body = Prim (Pint_add, [Var x; Const (Const_int 1)])
  })],
  Let ("g",
    Apply { func = Var "f"; args = [Const (Const_int 42)] },
    Var "g"
  )
)
```

### NativeLambda
```ocaml
Uletrec (
  [(f/1, Uclosure {
    functions = [{
      label = "fun_f";
      arity = 1;
      params = [x/2];
      body = Uprim (Paddint, [Uvar x/2; Uconst (Const_int 1)])
    }];
    free_vars = []  (* No captured variables *)
  })],
  Ulet (g/3,
    Udirect_apply ("fun_f", [Uconst (Const_int 42)]),  (* Direct call! *)
    Uvar g/3
  )
)
```

**Key differences:**
1. Closure has explicit `label = "fun_f"` for direct calls
2. Variables have unique IDs (f/1, x/2, g/3)
3. Application is `Udirect_apply` because we know the function
4. `free_vars = []` explicit (no captured environment)

## Closure Example

### Input: Closure with Free Variable
```ocaml
let x = 10
let f y = x + y
```

### Lambda IR
```ocaml
Let ("x", Const (Const_int 10),
  Let ("f",
    Function {
      params = [y];
      body = Prim (Pint_add, [Var x; Var y])  (* x is free! *)
    },
    Var "f"
  )
)
```

### NativeLambda (After Free Variable Analysis)
```ocaml
Ulet (x/1, Uconst (Const_int 10),
  Ulet (f/2,
    Uclosure {
      functions = [{
        label = "fun_f";
        arity = 1;
        params = [y/3];
        body = Uprim (Paddint, [Uvar x/1; Uvar y/3])
      }];
      free_vars = [x/1]  (* x is captured! *)
    },
    Uvar f/2
  )
)
```

**Key difference:** `free_vars = [x/1]` makes it explicit that the closure captures `x`.

## Memory Layout Example

### Input: Record Creation
```ocaml
type point = { x : int; y : int }
let p = { x = 10; y = 20 }
```

### Lambda IR
```ocaml
Let ("p",
  Prim (Pmakeblock 0, [
    Const (Const_int 10);
    Const (Const_int 20)
  ]),
  Var "p"
)
```

### NativeLambda
```ocaml
Ulet (p/1,
  Uprim (Pmakeblock (0, Immutable), [  (* Tag 0, immutable *)
    Uconst (Const_int 10);
    Uconst (Const_int 20)
  ]),
  Uvar p/1
)
```

**Key difference:** `Pmakeblock (0, Immutable)` makes mutability explicit for GC optimization.

## When to Use Each IR

### Use Lambda IR when:
- Implementing language-level optimizations (constant folding, etc.)
- Working with all backends (native + VM)
- Pattern matching is still high-level

### Use NativeLambda when:
- Generating native code (ARM64, x86_64, RISC-V)
- Need explicit closure information
- Working with memory layout
- Implementing calling conventions

## Pipeline Summary

```
Source → Parser → TypedTree → Lambda → [Lambda optimizations]
                                  ↓
                    ┌─────────────┴─────────────┐
                    ↓                           ↓
            NativeLambda                   Jambda/WasmIR
        (Closure conversion)           (VM-specific lowering)
                    ↓                           ↓
            ARM64/x86_64/RISC-V            JS/Wasm output
```
