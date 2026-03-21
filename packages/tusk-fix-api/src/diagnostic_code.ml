open Std
open Std.Collections

type t =
  | DirectUnixUsage
  | DirectSysUsage
  | DirectStdlibUsage
  | DirectPervasivesUsage
  | CamelCaseTypeName
  | ShortTypeVariableName
  | CamelCaseFunctionName
  | JiraffeCaseModuleName
  | CamelCaseVariableName
  | PrimeVariableName
  | QualifiedRecordField
  | ConcatenatedStringLiteral
  | CustomOperatorDefinition
  | CamelCaseArgumentName
  | NamedArgumentOrder
  | TFirstNamedArgs
  | SortedNamedArguments
  | CamelCaseRecordFieldName
  | ConstructorNameStyle
  | PolyvariantNameStyle
  | PackageProvided of package_entry

and package_entry = {
  package_name : string;
  local_id : string;
  rule_id : string;
  title : string;
  body : string;
  message : string;
}

type entry = {
  code : t;
  title : string;
  body : string;
}

let package_codes : (string, package_entry) Std.Collections.HashMap.t =
  Std.Collections.HashMap.create ()

let package_code_id entry =
  entry.package_name ^ ":" ^ entry.local_id

let to_id = function
  | DirectUnixUsage -> "F0001"
  | DirectSysUsage -> "F0002"
  | DirectStdlibUsage -> "F0003"
  | DirectPervasivesUsage -> "F0004"
  | CamelCaseTypeName -> "F0101"
  | ShortTypeVariableName -> "F0102"
  | CamelCaseFunctionName -> "F0103"
  | JiraffeCaseModuleName -> "F0104"
  | CamelCaseVariableName -> "F0105"
  | PrimeVariableName -> "F0106"
  | QualifiedRecordField -> "F0107"
  | ConcatenatedStringLiteral -> "F0108"
  | CustomOperatorDefinition -> "F0109"
  | CamelCaseArgumentName -> "F0110"
  | NamedArgumentOrder -> "F0111"
  | TFirstNamedArgs -> "F0112"
  | SortedNamedArguments -> "F0113"
  | CamelCaseRecordFieldName -> "F0114"
  | ConstructorNameStyle -> "F0115"
  | PolyvariantNameStyle -> "F0116"
  | PackageProvided entry -> package_code_id entry

let of_id = function
  | "F0001" -> Some DirectUnixUsage
  | "F0002" -> Some DirectSysUsage
  | "F0003" -> Some DirectStdlibUsage
  | "F0004" -> Some DirectPervasivesUsage
  | "F0101" -> Some CamelCaseTypeName
  | "F0102" -> Some ShortTypeVariableName
  | "F0103" -> Some CamelCaseFunctionName
  | "F0104" -> Some JiraffeCaseModuleName
  | "F0105" -> Some CamelCaseVariableName
  | "F0106" -> Some PrimeVariableName
  | "F0107" -> Some QualifiedRecordField
  | "F0108" -> Some ConcatenatedStringLiteral
  | "F0109" -> Some CustomOperatorDefinition
  | "F0110" -> Some CamelCaseArgumentName
  | "F0111" -> Some NamedArgumentOrder
  | "F0112" -> Some TFirstNamedArgs
  | "F0113" -> Some SortedNamedArguments
  | "F0114" -> Some CamelCaseRecordFieldName
  | "F0115" -> Some ConstructorNameStyle
  | "F0116" -> Some PolyvariantNameStyle
  | code -> (
      match Std.Collections.HashMap.get package_codes code with
      | Some entry -> Some (PackageProvided entry)
      | None -> None)

let register_package_code entry =
  ignore (Std.Collections.HashMap.insert package_codes (package_code_id entry) entry)

let register_package_codes entries =
  List.iter register_package_code entries

let clear_package_codes () =
  Std.Collections.HashMap.clear package_codes

let title = function
  | DirectUnixUsage -> "Direct Unix usage"
  | DirectSysUsage -> "Direct Sys usage"
  | DirectStdlibUsage -> "Direct Stdlib usage"
  | DirectPervasivesUsage -> "Direct Pervasives usage"
  | CamelCaseTypeName -> "Type names should be snake_case"
  | ShortTypeVariableName -> "Prefer descriptive type variable names"
  | CamelCaseFunctionName -> "Function names should be snake_case"
  | JiraffeCaseModuleName -> "Module names should be ClassCase"
  | CamelCaseVariableName -> "Variable names should be snake_case"
  | PrimeVariableName -> "Avoid prime-suffixed variable names"
  | QualifiedRecordField -> "Prefer Module.{ field = value }"
  | ConcatenatedStringLiteral -> "Prefer multiline string literals"
  | CustomOperatorDefinition -> "Avoid custom operators"
  | CamelCaseArgumentName -> "Argument names should be snake_case"
  | NamedArgumentOrder -> "Keep named arguments first"
  | TFirstNamedArgs -> "Prefer t-first positional arguments"
  | SortedNamedArguments -> "Sort named arguments alphabetically"
  | CamelCaseRecordFieldName -> "Record fields should be snake_case"
  | ConstructorNameStyle -> "Constructors should be ClassCase"
  | PolyvariantNameStyle -> "Polymorphic variants should be snake_case"
  | PackageProvided entry -> entry.title

let body = function
  | DirectUnixUsage ->
      {|
Direct calls into Unix bypass Riot's scheduling and portability boundaries.

Why this rule exists:
- Riot code runs on top of a cooperative actor runtime.
- A blocking Unix call can stall the scheduler and delay unrelated actors.
- Direct Unix usage also hard-codes platform details into packages that should stay platform-agnostic.

What to do instead:
- Prefer package-owned Riot abstractions when they exist.
- Push true OS boundaries down into the packages that are supposed to own them, like kernel.
- If you really need a Unix boundary, introduce it deliberately instead of sprinkling Unix calls through application code.

Examples:
  Bad:    let home = Unix.getenv "HOME"
  Bad:    let now = Unix.gettimeofday ()
  Better: move the OS interaction behind a package-owned API and call that API from the rest of the system.

This rule exists to keep scheduler-sensitive code honest. A direct Unix call may work today and still be the wrong architectural seam.
|}
  | DirectSysUsage ->
      {|
Direct Sys usage reaches into process-global runtime state instead of going through Riot-owned boundaries.

Why this rule exists:
- Sys exposes host and runtime details directly from OCaml.
- That makes portability and policy decisions leak into packages that should not own them.
- It also makes it harder to keep behavior consistent across the ecosystem.

What to do instead:
- Prefer Riot wrappers for system information and runtime behavior.
- Keep process-global and platform-global logic in boundary-owning packages.

Examples:
  Bad:    let args = Sys.argv
  Bad:    let is_win = Sys.win32
  Better: depend on a Riot-owned API that exposes the specific system fact you need.

This rule keeps packages from silently depending on ambient runtime state.
|}
  | DirectStdlibUsage ->
      {|
Code outside the runtime boundary should go through Riot's Std layer instead of referencing Stdlib directly.

Why this rule exists:
- Riot is trying to provide a coherent programming stack, not just a pile of packages.
- Routing code through Std gives the ecosystem one owned surface instead of ad hoc direct references into Stdlib.
- That leaves room for better defaults, portability adjustments, and package-wide conventions.

What to do instead:
- Replace Stdlib references with Std when the Riot surface already owns that API.
- If Std does not yet expose something important, that is usually a signal to extend Std deliberately rather than bypass it forever.

Examples:
  Bad:    open Stdlib
  Bad:    let cmp = Stdlib.compare a b
  Better: open Std
  Better: let cmp = Std.compare a b

This rule is about keeping the ecosystem designed, not accidental.
|}
  | DirectPervasivesUsage ->
      {|
Pervasives is the historical pre-Stdlib module and should not appear in modern Riot code.

Why this rule exists:
- Pervasives is legacy OCaml surface area.
- Riot code should point at the current owned surface, not historic compatibility layers.

What to do instead:
- Replace direct Pervasives references with Std.

Examples:
  Bad:    let cmp = Pervasives.compare a b
  Better: let cmp = Std.compare a b

This rule exists mostly for consistency and modernization.
|}
  | CamelCaseTypeName ->
      {|
Type names should use snake_case.

Why this rule exists:
- Riot code treats types like values and record fields: lower-case, underscore-separated names read best.
- Mixing lower-case camelCase into type names makes signatures visually noisy.

Examples:
  Bad:    type userProfile = ...
  Better: type user_profile = ...

Keep type names boring and predictable.
|}
  | ShortTypeVariableName ->
      {|
Avoid one-letter type variable names like 'a and 'b in real type definitions.

Why this rule exists:
- Short type variables are compact but not descriptive.
- In public APIs they force the reader to reverse-engineer intent from context.

What to do instead:
- Use names that communicate role.
- Prefer names like 'value, 'error, 'state, or 'msg when those roles matter.

Examples:
  Bad:    type ('a, 'b) resultish = ...
  Better: type ('value, 'error) resultish = ...
|}
  | CamelCaseFunctionName ->
      {|
Function names should use snake_case.

Why this rule exists:
- Snake case is the dominant value/function naming style across Riot.
- camelCase function names stick out immediately and make APIs feel imported rather than native.

Examples:
  Bad:    let parseUser input = ...
  Better: let parse_user input = ...
|}
  | JiraffeCaseModuleName ->
      {|
Module names should use ClassCase without underscores.

Why this rule exists:
- Mixed styles like Foo_bar are harder to scan than either FooBar or foo_bar.
- Riot uses ClassCase for modules and snake_case for values. Mixing the two in one identifier makes the boundary blurry.

Examples:
  Bad:    module Foo_bar = struct ... end
  Better: module FooBar = struct ... end
|}
  | CamelCaseVariableName ->
      {|
Variable names should use snake_case.

Why this rule exists:
- Local bindings should follow the same style as function names.
- snake_case keeps identifiers visually consistent across patterns, lets, and record fields.

Examples:
  Bad:    let currentUser = ...
  Better: let current_user = ...
|}
  | PrimeVariableName ->
      {|
Avoid apostrophes in variable names.

Why this rule exists:
- Prime-suffixed names are compact but vague.
- Names like x' or state' force the reader to guess whether the binding is an update, a copy, or just a temporary.

What to do instead:
- Use a descriptive suffix like _next, _updated, or a numeric suffix like x2 when that is genuinely the best name.

Examples:
  Bad:    let state' = ...
  Better: let next_state = ...
  Better: let state2 = ...
|}
  | QualifiedRecordField ->
      {|
Prefer Module.{ field = value } over { Module.field = value }.

Why this rule exists:
- Local opens keep the module qualification in one place.
- Repeating the module name in every record field makes the literal harder to read.

Examples:
  Bad:    { User.name = name; User.id = id }
  Better: User.{ name = name; id = id }
|}
  | ConcatenatedStringLiteral ->
      {concat|
Prefer a multiline string literal when a string is built entirely by concatenating string literals.

Why this rule exists:
- Repeated ^ between literal fragments hides the final text.
- Multiline literals are easier to edit, copy, and reason about as one unit.

Examples:
  Bad:    "hello " ^ "there " ^ "friend"
  Better: {|
hello there friend
|}
|concat}
  | CustomOperatorDefinition ->
      {|
Avoid defining custom operators.

Why this rule exists:
- Custom operators are hard to search for and hard to learn.
- They compress too much meaning into punctuation.
- APIs built from named functions are usually clearer in diffs, code review, and diagnostics.

What to do instead:
- Use a descriptive function name.
- If the behavior is domain-specific, make the domain explicit in the function name.
|}
  | CamelCaseArgumentName ->
      {|
Argument names should use snake_case.

Why this rule exists:
- Named and positional parameters should read like the rest of the value-level language.
- camelCase arguments look like a different style system inside otherwise consistent functions.

Examples:
  Bad:    let create ~userId ~displayName = ...
  Better: let create ~user_id ~display_name = ...
|}
  | NamedArgumentOrder ->
      {|
Function parameters should be ordered as:
1. labeled arguments
2. optional arguments with defaults
3. positional arguments

Why this rule exists:
- A stable order makes APIs easier to skim.
- Putting positional arguments first tends to bury the configurable surface of the function.
|}
  | TFirstNamedArgs ->
      {|
When a function takes t alongside named arguments, keep t as the first positional argument.

Why this rule exists:
- Riot APIs often treat t as the receiver/state value.
- Keeping t first among positional arguments makes pipeline and method-like usage more predictable.

Examples:
  Better: let render ~width ~height t = ...
  Worse:  let render ~width ~height other t = ...
|}
  | SortedNamedArguments ->
      {|
Named arguments should be kept in alphabetical order.

Why this rule exists:
- Alphabetical order removes needless bikeshedding.
- It also makes it easier to spot missing, duplicated, or newly inserted arguments in code review.
|}
  | CamelCaseRecordFieldName ->
      {|
Record field names should use snake_case.

Why this rule exists:
- Fields are part of the value-level API surface.
- camelCase field names create friction with pattern matching, updates, and named arguments that are otherwise snake_case.

Examples:
  Bad:    type t = { userName : string }
  Better: type t = { user_name : string }
|}
  | ConstructorNameStyle ->
      {|
Constructors should use ClassCase.

Why this rule exists:
- Constructors are type-level names and should read like modules.
- Underscored constructor names like Foo_bar sit awkwardly between value and module naming styles.

Examples:
  Bad:    type t = Foo_bar | Bar_baz
  Better: type t = FooBar | BarBaz
|}
  | PolyvariantNameStyle ->
      {|
Polymorphic variant tags should use snake_case.

Why this rule exists:
- Polyvariants are frequently used as value-like tags.
- snake_case keeps them aligned with the rest of Riot's value-level naming style.

Examples:
  Bad:    `polyVar
  Bad:    `Poly_var
  Better: `poly_var
|}
  | PackageProvided entry -> entry.body

let rule_id = function
  | DirectUnixUsage
  | DirectSysUsage
  | DirectStdlibUsage
  | DirectPervasivesUsage ->
      "no-stdlib"
  | CamelCaseTypeName -> "snake-case-type-names"
  | ShortTypeVariableName -> "descriptive-type-variables"
  | CamelCaseFunctionName -> "snake-case-function-names"
  | JiraffeCaseModuleName -> "class-case-module-names"
  | CamelCaseVariableName -> "snake-case-variable-names"
  | PrimeVariableName -> "no-prime-variables"
  | QualifiedRecordField -> "qualified-record-field-style"
  | ConcatenatedStringLiteral -> "multiline-string-style"
  | CustomOperatorDefinition -> "custom-operator-style"
  | CamelCaseArgumentName -> "snake-case-argument-names"
  | NamedArgumentOrder -> "argument-order-style"
  | TFirstNamedArgs -> "t-first-arg-style"
  | SortedNamedArguments -> "named-arg-sort-style"
  | CamelCaseRecordFieldName -> "record-field-name-style"
  | ConstructorNameStyle -> "constructor-name-style"
  | PolyvariantNameStyle -> "polyvariant-name-style"
  | PackageProvided entry -> entry.rule_id

let message = function
  | DirectStdlibUsage ->
      "Direct usage of Stdlib is discouraged. Use Std instead."
  | DirectPervasivesUsage ->
      "Direct usage of Pervasives is discouraged. Use Std instead."
  | DirectUnixUsage ->
      "Direct usage of Unix is discouraged. Use package-owned Riot abstractions instead."
  | DirectSysUsage ->
      "Direct usage of Sys is discouraged. Use package-owned Riot abstractions instead."
  | CamelCaseTypeName ->
      "Type names should use snake_case instead of camelCase."
  | ShortTypeVariableName ->
      "Avoid one-letter type variable names like 'a or 'b in type definitions."
  | CamelCaseFunctionName ->
      "Function names should use snake_case instead of camelCase."
  | JiraffeCaseModuleName ->
      "Module names should use ClassCase without underscores."
  | CamelCaseVariableName ->
      "Variable names should use snake_case instead of camelCase."
  | PrimeVariableName ->
      "Avoid apostrophes in variable names; prefer a descriptive suffix."
  | QualifiedRecordField ->
      "Prefer Module.{ field = value } over { Module.field = value }."
  | ConcatenatedStringLiteral ->
      "Prefer a multiline string literal over concatenating string literals."
  | CustomOperatorDefinition ->
      "Avoid defining custom operators; prefer descriptive function names."
  | CamelCaseArgumentName ->
      "Argument names should use snake_case instead of camelCase."
  | NamedArgumentOrder ->
      "Place labeled arguments first, then optional defaults, then positional arguments."
  | TFirstNamedArgs ->
      "When a function takes t with named arguments, keep t first among positional arguments."
  | SortedNamedArguments ->
      "Keep named arguments alphabetically sorted."
  | CamelCaseRecordFieldName ->
      "Record field names should use snake_case instead of camelCase."
  | ConstructorNameStyle ->
      "Constructors should use ClassCase without underscores."
  | PolyvariantNameStyle ->
      "Polymorphic variant tags should use snake_case."
  | PackageProvided entry -> entry.message

let no_stdlib_code_for_module = function
  | "Unix" -> Some DirectUnixUsage
  | "Sys" -> Some DirectSysUsage
  | "Stdlib" -> Some DirectStdlibUsage
  | "Pervasives" -> Some DirectPervasivesUsage
  | _ -> None

let explain code =
  match of_id code with
  | Some code ->
      Some {
        code;
        title = title code;
        body = body code;
      }
  | None -> None

let format_explanation entry =
  to_id entry.code ^ " - " ^ entry.title ^ "\n\n" ^ entry.body ^ "\n"
