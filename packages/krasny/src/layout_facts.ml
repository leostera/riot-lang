open Std

module Layout = Layout_policy

let expr = fun
  ?flat_width
  ?(pressure = Layout.Flat)
  ?(has_leading_comment = false)
  ?(has_trailing_comment = false)
  ?(contains_hardline = false)
  ?(item_count = 0)
  ?callee_class
  () ->
  Layout.make_facts
    ?flat_width
    ~pressure
    ~has_leading_comment
    ~has_trailing_comment
    ~contains_hardline
    ~item_count
    ~syntax_family:Layout.Expr
    ?callee_class
    ()

let type_expr = fun
  ?flat_width
  ?(pressure = Layout.Flat)
  ?(has_leading_comment = false)
  ?(has_trailing_comment = false)
  ?(contains_hardline = false)
  ?(item_count = 0)
  () ->
  Layout.make_facts
    ?flat_width
    ~pressure
    ~has_leading_comment
    ~has_trailing_comment
    ~contains_hardline
    ~item_count
    ~syntax_family:Layout.Type_expr
    ()

let pattern = fun
  ?flat_width
  ?(pressure = Layout.Flat)
  ?(has_leading_comment = false)
  ?(has_trailing_comment = false)
  ?(contains_hardline = false)
  ?(item_count = 0)
  () ->
  Layout.make_facts
    ?flat_width
    ~pressure
    ~has_leading_comment
    ~has_trailing_comment
    ~contains_hardline
    ~item_count
    ~syntax_family:Layout.Pattern
    ()

let application = fun
  ~force_parent_break
  ~arg_count
  ~callee_class
  ~flat_width
  ~has_multiline_args
  ~has_heavy_nested_apply
  () ->
  let single_constructor_payload =
    Int.equal arg_count 1 && callee_class = Layout.Constructor_like
  in
  let pressure =
    if force_parent_break then
      Layout.Strong [ Layout.Parent_requires_block ]
    else if has_heavy_nested_apply then
      Layout.Strong [ Layout.Heavy_nested_apply ]
    else if has_multiline_args && single_constructor_payload then
      Layout.Soft [ Layout.Child_is_block ]
    else if has_multiline_args && Int.(arg_count > 1) then
      Layout.Strong [ Layout.Child_is_block ]
    else if has_multiline_args then
      Layout.Soft [ Layout.Child_is_block ]
    else
      Layout.Flat
  in
  expr ?flat_width ~pressure ~item_count:arg_count ~callee_class ()
