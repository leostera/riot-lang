# TODO

## Working Loop

1. Build the latest `tusk` from the stable global binary when build-system / parser / lint runtime changes are involved:
   - `rm -f _build/tusk.lock`
   - `timeout 240 tusk build tusk-cli`
2. Use `./tusk` for follow-up verification after rebuilding `tusk-cli`:
   - `rm -f _build/tusk.lock`
   - `timeout 180 ./tusk test syn:cst_tests`
   - `rm -f _build/tusk.lock`
   - `timeout 180 ./tusk test tusk-fix:runner_tests`
3. When touching only a single package, use the narrower build when possible:
   - `rm -f _build/tusk.lock`
   - `timeout 60 tusk build syn`
   - `rm -f _build/tusk.lock`
   - `timeout 60 tusk build tusk-fix`
4. Read this file from top to bottom and pick the next unchecked item that is unblocked by the current `Syn.Cst` surface.
5. After implementing a task:
   - run the narrowest relevant verification first
   - then rerun the full `syn` and `tusk-fix` test slices
   - when smoke-checking lint output, prefer `rm -f _build/tusk.lock && timeout 180 tusk run tusk -- fix --check --limit 10 <file>`
   - if the change affects the `tusk` binary surface, rebuild `tusk-cli`
6. Mark a task complete only after the relevant tests have passed.
7. Commit often, with one logical slice per commit.

## Verification Commands

- `rm -f _build/tusk.lock && timeout 240 tusk build tusk-cli`
- `rm -f _build/tusk.lock && timeout 180 ./tusk test syn:cst_tests`
- `rm -f _build/tusk.lock && timeout 180 ./tusk test tusk-fix:runner_tests`
- `rm -f _build/tusk.lock && timeout 120 ./tusk fix --list-rules`
- `rm -f _build/tusk.lock && timeout 120 ./tusk fix --list-diagnostics`
- `rm -f _build/tusk.lock && timeout 180 tusk run tusk -- fix --check --limit 10 <file>`

## Main goals

- [ ] Implement all the lints in the sections below (and i mean ALL of them!)
- [ ] Simplify rules to not have duplicated text/hints fields -- (right now they have id, message, description, title, and explain strings, they should really just need id + short description + long explanation)
      - [ ] Explanations should be written out with examples and not with a structured "why this rule exists" -- it exists because something in the language is harder without them, so we should explain how things would look without the rule, the issues that arise from it, and how this rule helps prevent that.
- [ ] Make sure the Syn.Cst tree represents the entirety of the OCaml grammar (see ./packages/syn/docs/ocaml_grammar.ebnf)
- [ ] Then lets do a learning pass by exploring how Rust's clippy does a few things:
      - [ ] we should group built-in lints by category like rust's clippy does (https://doc.rust-lang.org/stable/clippy/lints.html)
      - [ ] we should also learn from how clippy lets your build new lints (https://doc.rust-lang.org/stable/clippy/development/adding_lints.html)


## CST-Driven Lint Rules

- [x] Type names should use `snake_case` instead of `camelCase` (`snake-case-type-names`)
- [x] Warn on type variable names like `'a` or `'b` in type definitions; suggest descriptive names like `'value` and `'error` (`descriptive-type-variables`)
- [x] Function names should use `snake_case` instead of `camelCase` (`snake-case-function-names`)
- [x] Module names should be ClassCased and not Jiraffe_cased (`class-case-module-names`)
- [x] Variable names should use `snake_case` instead of `camelCase` (`snake-case-variable-names`)
- [x] Variable names should not contain `'`; prefer `x2` over `x'` (`no-prime-variables`)
- [x] Argument names should use `snake_case` instead of `camelCase` (`snake-case-argument-names`)
- [x] Prefer multiline strings like `{| ... |}` over concatenated string literals (`prefer-multiline-string-literals`)
- [x] Warn against custom operators (`no-custom-operators`)
- [x] Named arguments should come first, then arguments with defaults, then positional arguments (`ordered-argument-kinds`)
- [x] Prefer `t`-first functions when named arguments are present (`t-first-named-arguments`)
- [x] Keep named arguments alphabetically sorted (`alphabetized-named-arguments`)
- [x] Record field names should use `snake_case` (`snake-case-record-fields`)
- [x] Constructor names should be `ClassCased` (`class-case-constructors`)
- [x] Polyvariant constructors should be `snake_case` (`snake-case-polyvariant-tags`)
- [x] Avoid single-letter function names like `f` or `g` (`avoid-single-letter-function-names`)
- [x] Avoid single-letter type names except for `t` (`avoid-single-letter-type-names`)
- [x] Prefer function sigantures in the form of `let foo : <sign> = fn x y z -> ...` rather than inlineed in params like `let foo (x : int) (y : bool) ...` (`no-inline-parameter-type-annotations`)
- [x] Prefer function definitions with explicit params like `let foo x y = x + y` and `let foo = fn x y -> x + y` instead of `let foo = function | x -> x +1` -- we want to discourage those inlined functions and ideally nudge towards `let foo = fn ... -> ...` since it makes adding a signature easier later (`no-function-shorthand`)
- [x] Warn about depth of parenthesized expressions! ~5 parens is too much (`limit-parenthesis-depth`)

- [x] Warn about functions with many params (complex check) in a few ways: (`limit-function-parameters`)
      1. if there are only unnamed params, 5 is too much
      2. if there are only named params, 8 is too much
      3. if there are mixed params, 10 is too much

      The main idea for the full explanation is that if you see many params there's likely a hidden record of sorts that probbaly has a name and wants to be named. Think like instead of getting a ~purchased_at + ~quantity + ~item, you want a PurchaseOrder.t type which becomes a single param in your function collapsing 3 params!

- [ ] when using inline closed polymorphic variants like ``[ `a | `b ] list``, prefer giving them a _name_ like ``type x = [ `a  `b ]`` -- this allows you to reuse them and also easier on the eyes when types get compllicated

- [x] Prefer pipeline over very nested function calls so `(foo (bar (baz (hex 1))))` into `hex 1 |> baz |> bar |> foo` (`prefer-pipelines-for-nested-calls`)

- [x] Redundant `else ()` can be removed (`no-redundant-else-unit`)

- [x] Warn about using `open!` (`no-open-bang`)

- [x] Match on bool being redundant like `match foo () with | true -> ... | ....` shoul be `if foo () then ... else ...` -- ths check can inspect both branches and suggest accordingly, if the branches are `true -> ...` and `_ -> ()` then we just suggest `if foo() then ...` without an else. If the branch matches on `false` we can suggest `if not foo () then ...`, if both branches have code (and not just return a `()`) then we can suggest the full `if cond then .. else ..` (`prefer-if-over-bool-match`)

- [x] `try f x with | e -> raise e` is just `f x` (`no-redundant-reraise`)

- [x] Useless binding `let y = f x in y` is just `f x` (`no-useless-let-return`)

- [x] Unnecessary `rec` in `let rec f x = x + 1` (`no-unnecessary-rec`)

- [x] Many `open` statmenets get confusing and can shadow symbols, warn about this if we have more than 2 opens (`limit-open-statements`)

- [x] `let () = foo () in ..` this shoudl just be `foo ();` (`prefer-sequences-over-let-unit`)

- [x] Needles `(fun x -> foo x)` due to eta-expansion, they can just put `foo` in there (`no-eta-expansion`)
- [x] Needless or redundant parenthesis (`no-redundant-parentheses`)
- [x] Needless or redundant begin/end blocks (`no-redundant-begin-end`)
- [x] Prefer `fun x  -> match x  with | ....` over `function | A -> ... | B ...` (`no-function-shorthand`)

- [ ] bool positional parameters in functions: suggest a named param or an enum

- [ ] tuples are a _bad idea_, just use records, but specially two rules:
      1. if a tuple has more than 3 elements of the same type = prefer a record 
      2. if a tuple has more than 4 elements of any type, prefer a record

- [x] warn about public types with mutable fields -- its okay ot have private mutable fields, but once they're public you risk anyone mutating them under your feet! (`no-public-mutable-fields`)

- [  ] `let open Foo in [...]` : prefer `Foo.[...]` unless its multiple let opens, like
        ```
        let open A in
        let open B in
        let open C in
        [ .... ]
        ````

        this applies also to record construction, list construction, and parenthesized expressions like 
        `Module.{ field = value }` over `{ Module.field = value }`

        basically prefer scoped module qualification syntax over inline qualified field syntax
- [x] Prefer `Module.(var.field)` for access to record fields instead of `var.Module.field` (`prefer-scoped-field-access`)

- [x]  warn about nested matches (3 matches trigger a warning) (`limit-nested-match-depth`)
- [x]  warn about _exn functions (`no-exn-suffix-functions`)



- [x] Avoid function names like `f` or `g` (`avoid-single-letter-function-names`)
- [x] Avoid type single-letter type names except its `t` (`avoid-single-letter-type-names`)
- [ ] If a module has a single type definition, prefer it be called `t`

- [x] useless booleans comparisons in conditionals like `if is_ready = true then ...` that should be flagged and recommended to rewrite as `if is_ready then ...` -- same for `if flag <> false then ...` and prefer `if flag then  ...` -- same for `if b = false then ...` prefer `if not b then ...` (`no-boolean-comparisons-in-conditionals`)

## Built-in rules about packages 

- [ ] package names should be in `kebab-case`: use `my-pkg` and not `my_pkg` or `MyPkg`
- [ ] package names should start with a letter
- [ ] package names should not have trailing dashes or underscores
- [ ] subdirs and file names should be in snake_case (as Tusk will translate their names to ClassCase): hello_world.ml and not Hello_world.ml or HelloWorld.ml
- [ ] warn about modules without .mli files!

## Package-provided lint rules

Miniriot:
- [ ] If a while or for loop doesn't immediately have a `yield ()` at the beginning of it, we should warn about it

Std:
- [ ] `ignore (List.map f xs)` or `ignore (Iter.map f iter)` is likely a bug and should be `List.iter f xs` or `Iter.iter f iter` -- i fogret the xact iterators api but you'll figure it out -- the same applies with `let _ = Mod.map f xs` 

- [x] if someone uses `<>` we should tell them its `!=` now (`std:prefer-bang-equal-inequality`)
- [x] Double List.rev (`std:no-double-list-rev`)

- [ ] `List.length x == 0` or `List.length > 0` prefer `List.is_empty` (if we don't have it then lets add it)

- [ ] Constants like 3.14 should use Std.Math.PI instead

- [ ] Common functions like .map for option or result should be highly preferred over custom matching, so we can have a set of rules like:
     - [ ] `match x with | Some y -> Some (f y) | None -> None`  -- Prefer Option.map x f
     - [ ] `match x with | Ok y -> Ok (f y) | err -> err`  -- Prefer Result.map x f

- [ ] Exceptions for flow control: if we have a `try` that returns a None or an explicit `(Error _)` we should suggest using Result.protect` instead (or something like that)

- [ ] `x_of_y` functions (int of string, string of bool, etc) should be replaced by X.from_y or for string, Y.to_string: 
    * string_of_int -> Int.to_string
    * int_of_string -> Int.parse
    * float_of_int  -> Float.from_int int
    * 
