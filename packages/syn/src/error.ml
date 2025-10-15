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
  | E0013_MissingTypeDeclEquals
  | E0014_UnclosedDelimiter
  | E0015_MissingTypeName
  | E0016_EmptyCharLiteral
  | E0017_MultiCharLiteral
  | E0018_UnclosedCharLiteral
  | E0019_UnclosedTypeParams
  | E0020_MissingBinaryOperand
  | E0021_ConsecutiveBinaryOperators
  | E0022_InvalidTypeParameter
  | E0023_UppercaseTypeVariable
  | E0024_UppercaseTypeName
  | E0025_BracketedTypeParameters
  | E0026_ListDoubleSemicolon
  | E0027_IfMissingThen
  | E0028_MatchMissingScrutinee
  | E0029_MatchMissingWith
  | E0030_MatchMissingPattern
  | E0031_MatchGuardMissingExpr

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
  | E0013_MissingTypeDeclEquals -> "E0013"
  | E0014_UnclosedDelimiter -> "E0014"
  | E0015_MissingTypeName -> "E0015"
  | E0016_EmptyCharLiteral -> "E0016"
  | E0017_MultiCharLiteral -> "E0017"
  | E0018_UnclosedCharLiteral -> "E0018"
  | E0019_UnclosedTypeParams -> "E0019"
  | E0020_MissingBinaryOperand -> "E0020"
  | E0021_ConsecutiveBinaryOperators -> "E0021"
  | E0022_InvalidTypeParameter -> "E0022"
  | E0023_UppercaseTypeVariable -> "E0023"
  | E0024_UppercaseTypeName -> "E0024"
  | E0025_BracketedTypeParameters -> "E0025"
  | E0026_ListDoubleSemicolon -> "E0026"
  | E0027_IfMissingThen -> "E0027"
  | E0028_MatchMissingScrutinee -> "E0028"
  | E0029_MatchMissingWith -> "E0029"
  | E0030_MatchMissingPattern -> "E0030"
  | E0031_MatchGuardMissingExpr -> "E0031"

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
  | "E0013" -> Some E0013_MissingTypeDeclEquals
  | "E0014" -> Some E0014_UnclosedDelimiter
  | "E0015" -> Some E0015_MissingTypeName
  | "E0016" -> Some E0016_EmptyCharLiteral
  | "E0017" -> Some E0017_MultiCharLiteral
  | "E0018" -> Some E0018_UnclosedCharLiteral
  | "E0019" -> Some E0019_UnclosedTypeParams
  | "E0020" -> Some E0020_MissingBinaryOperand
  | "E0021" -> Some E0021_ConsecutiveBinaryOperators
  | "E0022" -> Some E0022_InvalidTypeParameter
  | "E0023" -> Some E0023_UppercaseTypeVariable
  | "E0024" -> Some E0024_UppercaseTypeName
  | "E0025" -> Some E0025_BracketedTypeParameters
  | "E0026" -> Some E0026_ListDoubleSemicolon
  | "E0027" -> Some E0027_IfMissingThen
  | "E0028" -> Some E0028_MatchMissingScrutinee
  | "E0029" -> Some E0029_MatchMissingWith
  | "E0030" -> Some E0030_MatchMissingPattern
  | "E0031" -> Some E0031_MatchGuardMissingExpr
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
  | E0013_MissingTypeDeclEquals -> "missing-type-decl-equals"
  | E0014_UnclosedDelimiter -> "unclosed-delimiter"
  | E0015_MissingTypeName -> "missing-type-name"
  | E0016_EmptyCharLiteral -> "empty-char-literal"
  | E0017_MultiCharLiteral -> "multi-char-literal"
  | E0018_UnclosedCharLiteral -> "unclosed-char-literal"
  | E0019_UnclosedTypeParams -> "unclosed-type-params"
  | E0020_MissingBinaryOperand -> "missing-binary-operand"
  | E0021_ConsecutiveBinaryOperators -> "consecutive-binary-operators"
  | E0022_InvalidTypeParameter -> "invalid-type-parameter"
  | E0023_UppercaseTypeVariable -> "uppercase-type-variable"
  | E0024_UppercaseTypeName -> "uppercase-type-name"
  | E0025_BracketedTypeParameters -> "bracketed-type-parameters"
  | E0026_ListDoubleSemicolon -> "list-double-semicolon"
  | E0027_IfMissingThen -> "if-missing-then"
  | E0028_MatchMissingScrutinee -> "match-missing-scrutinee"
  | E0029_MatchMissingWith -> "match-missing-with"
  | E0030_MatchMissingPattern -> "match-missing-pattern"
  | E0031_MatchGuardMissingExpr -> "match-guard-missing-expr"

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
  | E0013_MissingTypeDeclEquals ->
      {|Type declarations require an = between the type name and its definition.|}
  | E0014_UnclosedDelimiter ->
      {|Delimiters like (, [, {, begin must be properly closed with ), ], }, end respectively.|}
  | E0015_MissingTypeName ->
      {|Type declarations require a name after the type keyword.|}
  | E0016_EmptyCharLiteral ->
      {|Character literals cannot be empty. Use a space character ' ' if you need a space.|}
  | E0017_MultiCharLiteral ->
      {|Character literals can only contain a single character. Use a string "..." for multiple characters.|}
  | E0018_UnclosedCharLiteral ->
      {|Character literals must be closed with a single quote '.|}
  | E0019_UnclosedTypeParams ->
      {|Type parameter lists must be closed with a closing parenthesis ). For example: type ('a, 'b) t = ...|}
  | E0020_MissingBinaryOperand ->
      {|Binary operators like +, -, *, =, etc. require both a left and right operand.

For example:
  ```ocaml
  let x = 1 + 2   (* correct *)
  let y = 1 +     (* missing right operand *)
  let z = + 2     (* missing left operand *)
  ```

If you want to create a partially applied operator function, use parentheses:
  ```ocaml
  let add_one = (+) 1
  ```
|}
  | E0021_ConsecutiveBinaryOperators ->
      {|Consecutive binary operators are not allowed. Each operator needs both operands.

For example:
  ```ocaml
  let x = 1 + + 2  (* error: consecutive + operators *)
  let y = 1 + 2    (* correct *)
  ```

If you intended to write a positive or negative number, attach the sign directly to the number:
  ```ocaml
  let x = 1 + (-2)  (* correct: adding negative 2 *)
  ```
|}
  | E0022_InvalidTypeParameter ->
      {|Type parameters must start with a single quote followed by a lowercase letter.

Valid type parameters:
  ```ocaml
  type 'a t           (* correct: single lowercase type variable *)
  type ('a, 'b) t     (* correct: multiple type variables *)
  type _ t            (* correct: wildcard type parameter *)
  ```

Invalid type parameters:
  ```ocaml
  type __ t           (* error: use _ instead of __ *)
  type @ t            (* error: @ is not valid *)
  type ! t            (* error: ! is not valid *)
  ```
|}
  | E0023_UppercaseTypeVariable ->
      {|Type variables must use lowercase letters.

Type variables in OCaml always start with a single quote followed by a lowercase identifier:
  ```ocaml
  type 'a t           (* correct *)
  type 'myvar t       (* correct *)
  type 'A t           (* error: must be lowercase *)
  type 'MyType t      (* error: must be lowercase *)
  ```

Fix: change 'A to 'a, or 'MyType to 'my_type
|}
  | E0024_UppercaseTypeName ->
      {|Type names must start with a lowercase letter.

In OCaml, type names use lowercase identifiers:
  ```ocaml
  type my_type = ...     (* correct *)
  type point = ...       (* correct *)
  type MyType = ...      (* error: must be lowercase *)
  type Point = ...       (* error: must be lowercase *)
  ```

Fix: change MyType to my_type or myType
Note: Uppercase identifiers are reserved for modules and constructors.
|}
  | E0025_BracketedTypeParameters ->
      {|OCaml uses parentheses for type parameters, not angle brackets.

OCaml syntax uses ('a, 'b) style type parameters, not <A, B> like other languages:
  ```ocaml
  (* Correct OCaml syntax *)
  type ('a, 'b) map = ...
  type 'a list = ...
  type ('key, 'value) hashtbl = ...
  
  (* Common mistakes from other languages *)
  type <A, B> map = ...        (* error: use ('a, 'b) instead *)
  type List<T> = ...           (* error: use 'a list instead *)
  type Map<K, V> = ...         (* error: use ('k, 'v) map instead *)
  ```

Fix: replace <A, B> with ('a, 'b) and use lowercase type variables.
|}
  | E0026_ListDoubleSemicolon ->
      {|List elements must be separated by a single semicolon.

In OCaml lists, elements are separated by semicolons (;), not commas:
  ```ocaml
  (* Correct syntax *)
  let x = [1; 2; 3]
  let y = ["a"; "b"; "c"]
  
  (* Common mistakes *)
  let z = [1;; 2]      (* error: double semicolon *)
  let w = [;;]         (* error: empty element with double semicolon *)
  let v = [;]          (* error: semicolon without elements *)
  ```

Fix: use a single semicolon (;) between list elements.
Note: Double semicolons (;;) are used as top-level statement terminators in the REPL, not in lists.
|}
  | E0027_IfMissingThen ->
      {|If-expressions require the 'then' keyword after the condition.

In OCaml, if-expressions have the syntax: if condition then expr1 else expr2
  ```ocaml
  (* Correct syntax *)
  let x = if true then 1 else 2
  let y = if x > 0 then "positive" else "negative"
  
  (* Common mistakes *)
  let z = if true 1 else 2        (* error: missing 'then' *)
  let w = if x > 0 "pos" else "neg" (* error: missing 'then' *)
  ```

Fix: add the 'then' keyword after the condition and before the then-branch.
|}
  | E0028_MatchMissingScrutinee ->
      {|Match-expressions require an expression to match on (the scrutinee).

In OCaml, match-expressions have the syntax: match expr with | pattern -> result
  ```ocaml
  (* Correct syntax *)
  let x = match value with 0 -> "zero" | _ -> "other"
  let y = match Some 1 with Some n -> n | None -> 0
  
  (* Missing scrutinee *)
  let z = match with 0 -> "zero"  (* error: missing expression after 'match' *)
  ```

Fix: add an expression after 'match' and before 'with'.
|}
  | E0029_MatchMissingWith ->
      {|Match-expressions require the 'with' keyword after the scrutinee.

In OCaml, match-expressions have the syntax: match expr with | pattern -> result
  ```ocaml
  (* Correct syntax *)
  let x = match value with 0 -> "zero" | _ -> "other"
  
  (* Missing 'with' *)
  let y = match value 0 -> "zero"  (* error: missing 'with' keyword *)
  ```

Fix: add 'with' keyword after the expression being matched.
|}
  | E0030_MatchMissingPattern ->
      {|Match cases require a pattern before the arrow (->).

In OCaml, each match case has the syntax: | pattern -> result
  ```ocaml
  (* Correct syntax *)
  let x = match n with 0 -> "zero" | n -> "other"
  let y = match opt with Some x -> x | None -> 0
  
  (* Missing pattern *)
  let z = match n with -> "result"  (* error: missing pattern before '->' *)
  ```

Fix: add a pattern before the '->' arrow.
|}
  | E0031_MatchGuardMissingExpr ->
      {|Match guards require an expression after the 'when' keyword.

In OCaml, match guards have the syntax: | pattern when condition -> result
  ```ocaml
  (* Correct syntax *)
  let x = match n with x when x > 0 -> "positive" | _ -> "not positive"
  
  (* Missing guard expression *)
  let y = match n with x when -> "result"  (* error: missing condition after 'when' *)
  ```

Fix: add a boolean expression after 'when'.
|}
