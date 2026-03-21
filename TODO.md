# TODO

## CST-Driven Lint Rules

- [x] Type names should use `snake_case` instead of `camelCase` (`type-name-style`)
- [x] Warn on type variable names like `'a` or `'b` in type definitions; suggest descriptive names like `'value` and `'error`
- [ ] Function names should use `snake_case` instead of `camelCase`
- [ ] Module names should not be Jiraffe_cased
- [ ] Variable names should use `snake_case` instead of `camelCase`
- [ ] Variable names should not contain `'`; prefer `x2` over `x'`
- [ ] Prefer `Module.{ field = value }` over `{ Module.field = value }`
- [ ] Prefer multiline strings like `{| ... |}` over concatenated string literals
- [ ] Disallow custom operators
- [ ] Argument names should use `snake_case` instead of `camelCase`
- [ ] Named arguments should come first, then arguments with defaults, then positional arguments
- [ ] Prefer `t`-first functions when named arguments are present
- [ ] Keep named arguments alphabetically sorted
- [ ] Record field names should use `snake_case`
- [ ] Constructor names should be `ClassCased`
- [ ] Polyvariant constructors should be `snake_cased`
- [ ] Prefer function sigantures in the form of `let foo : <sign> = fn x y z -> ...` rather than inlineed in params like `let foo (x : int) (y : bool) ...`
- [ ] Prefer function definitions with explicit params like `let foo x y = x + y` and `let foo = fn x y -> x + y` instead of `let foo = function | x -> x +1` -- we want to discourage those inlined functions and ideally nudge towards `let foo = fn ... -> ...` since it makes adding a signature easier later
- [ ] Warn about depth of parenthesized expressions! ~5 parens is too much
- [ ] Prefer `Module.(var.field)` for access to record fields instead of `var.Module.field`

- [ ] Warn about functions with many params (complex check) in a few ways:
      1. if there are only unnamed params, 5 is too much
      2. if there are only named params, 8 is too much
      3. if there are mixed params, 10 is too much

      The main idea for the full explanation is that if you see many params there's likely a hidden record of sorts that probbaly has a name and wants to be named. Think like instead of getting a ~purchased_at + ~quantity + ~item, you want a PurchaseOrder.t type which becomes a single param in your function collapsing 3 params!

- [ ] when using inline closed polymorphic variants like ``[ `a | `b ] list``, prefer giving them a _name_ like ``type x = [ `a  `b ]`` -- this allows you to reuse them and also easier on the eyes when types get compllicated

