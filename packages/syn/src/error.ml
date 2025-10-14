open Std

type id =
  | E0001_MalformedTypeVariable
  | E0002_MissingLetBindingPattern
  | E0003_MissingLetBindingEquals
  | E0004_MissingLetBindingExpr
  | E0005_UnexpectedStructureItem
  | E0006_UnexpectedSignatureItem
  | E0007_InvalidPattern
  | E0008_InvalidExpression
  | E0009_InvalidConstant
  | E0010_InvalidTypeExpression
  | E0011_MissingLetKeyword
  | E0012_MissingTypeKeyword

let id_to_string = function
  | E0001_MalformedTypeVariable -> "E0001"
  | E0002_MissingLetBindingPattern -> "E0002"
  | E0003_MissingLetBindingEquals -> "E0003"
  | E0004_MissingLetBindingExpr -> "E0004"
  | E0005_UnexpectedStructureItem -> "E0005"
  | E0006_UnexpectedSignatureItem -> "E0006"
  | E0007_InvalidPattern -> "E0007"
  | E0008_InvalidExpression -> "E0008"
  | E0009_InvalidConstant -> "E0009"
  | E0010_InvalidTypeExpression -> "E0010"
  | E0011_MissingLetKeyword -> "E0011"
  | E0012_MissingTypeKeyword -> "E0012"

let id_of_string = function
  | "E0001" -> Some E0001_MalformedTypeVariable
  | "E0002" -> Some E0002_MissingLetBindingPattern
  | "E0003" -> Some E0003_MissingLetBindingEquals
  | "E0004" -> Some E0004_MissingLetBindingExpr
  | "E0005" -> Some E0005_UnexpectedStructureItem
  | "E0006" -> Some E0006_UnexpectedSignatureItem
  | "E0007" -> Some E0007_InvalidPattern
  | "E0008" -> Some E0008_InvalidExpression
  | "E0009" -> Some E0009_InvalidConstant
  | "E0010" -> Some E0010_InvalidTypeExpression
  | "E0011" -> Some E0011_MissingLetKeyword
  | "E0012" -> Some E0012_MissingTypeKeyword
  | _ -> None

let name = function
  | E0001_MalformedTypeVariable -> "malformed-type-variable"
  | E0002_MissingLetBindingPattern -> "missing-let-binding-pattern"
  | E0003_MissingLetBindingEquals -> "missing-let-binding-equals"
  | E0004_MissingLetBindingExpr -> "missing-let-binding-expr"
  | E0005_UnexpectedStructureItem -> "unexpected-structure-item"
  | E0006_UnexpectedSignatureItem -> "unexpected-signature-item"
  | E0007_InvalidPattern -> "invalid-pattern"
  | E0008_InvalidExpression -> "invalid-expression"
  | E0009_InvalidConstant -> "invalid-constant"
  | E0010_InvalidTypeExpression -> "invalid-type-expression"
  | E0011_MissingLetKeyword -> "missing-let-keyword"
  | E0012_MissingTypeKeyword -> "missing-type-keyword"

let explain = function
  | E0001_MalformedTypeVariable ->
      {|Type variables must be written as 'a, 'b, etc. with no space or comments between the quote and name.|}
  | E0002_MissingLetBindingPattern ->
      {|
The left side of a let-expression allows you to pattern match on values to assign them to variables, but also to deconstruct them and access their inner values.

   ```ocaml
   let variable = 42 in
   let { x; _ } = point in
   let Some x = optional in
   (* ... *)
   ```

Beware that pattern matching on only one of many possible values will lead to runtime errors!
|}
  | E0003_MissingLetBindingEquals ->
      {|Every let binding needs an = between the pattern and the expression.|}
  | E0004_MissingLetBindingExpr ->
      {|Every let binding needs a value on the right side of the =.|}
  | E0005_UnexpectedStructureItem ->
      {|Structure items are top-level declarations like let, type, or module.|}
  | E0006_UnexpectedSignatureItem ->
      {|Signature items are declarations in .mli files like val, type, or module.|}
  | E0007_InvalidPattern ->
      {|
The left side of a let-expression allows you to pattern match on values to assign them to variables, but also to deconstruct them and access their inner values.

   ```ocaml
   let variable = 42 in
   let { x; _ } = point in
   let Some x = optional in
   (* ... *)
   ```

Beware that pattern matching on only one of many possible values will lead to runtime errors!
|}
  | E0008_InvalidExpression ->
      {|Expected a value, function call, or operator expression.|}
  | E0009_InvalidConstant ->
      {|Constants must be integers, floats, strings, or characters.|}
  | E0010_InvalidTypeExpression ->
      {|Expected a type like int, string, 'a, or a type constructor.|}
  | E0011_MissingLetKeyword ->
      {|Internal parser error - this is likely a bug in the parser.|}
  | E0012_MissingTypeKeyword ->
      {|Internal parser error - this is likely a bug in the parser.|}

