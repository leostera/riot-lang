# RAML Type Checker Examples

## ✅ Working Features

### Constants
```ocaml
let x = 42              (* Type: int *)
let s = "hello"         (* Type: string *)
let u = ()              (* Type: unit *)
```

### Functions
```ocaml
let identity = fun x -> x                (* Type: 'a -> 'a *)
let const_42 = fun x -> 42               (* Type: 'a -> int *)
```

### Function Application
```ocaml
let result = (fun x -> x) 42             (* Type: int, Value: 42 *)
```

### Tuples
```ocaml
let pair = (1, 2)                        (* Type: int * int *)
let triple = (1, "hello", true)          (* Type: int * string * bool *)
```

## Test Commands

```bash
# Check a file
tusk run raml -- check examples/simple.ml

# Verbose output (shows inferred types)
tusk run raml -- check examples/simple.ml --verbose

# Compile to executable
tusk run raml -- compile --target aarch64-apple-darwin examples/simple.ml
./a.out
echo $?  # Check exit code
```

## Current Limitations

- No binary operators yet (+, -, *, etc.)
- No match expressions  
- No recursive let bindings
- No type annotations
- Function bodies in let bindings must parse correctly with Syn

## Next Steps

1. Binary operations (`1 + 2`)
2. Match expressions (`match x with | p -> e`)
3. Recursive bindings (`let rec`)
4. Type constraints (`(e : t)`)
