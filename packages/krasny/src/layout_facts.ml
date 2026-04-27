open Std

module Layout = Layout_policy

let expr = fun
  ?flat_width
  ?(pressure = Layout.Flat)
  ?(has_leading_comment = false)
  ?(has_trailing_comment = false)
  ?(contains_hardline = false)
  ?(item_count = 0)
  ?(suffix_width = 0)
  ?callee_class
  () ->
  Layout.make_facts
    ?flat_width
    ~pressure
    ~has_leading_comment
    ~has_trailing_comment
    ~contains_hardline
    ~item_count
    ~suffix_width
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
  ?(suffix_width = 0)
  () ->
  Layout.make_facts
    ?flat_width
    ~pressure
    ~has_leading_comment
    ~has_trailing_comment
    ~contains_hardline
    ~item_count
    ~suffix_width
    ~syntax_family:Layout.Type_expr
    ()

let pattern = fun
  ?flat_width
  ?(pressure = Layout.Flat)
  ?(has_leading_comment = false)
  ?(has_trailing_comment = false)
  ?(contains_hardline = false)
  ?(item_count = 0)
  ?(suffix_width = 0)
  () ->
  Layout.make_facts
    ?flat_width
    ~pressure
    ~has_leading_comment
    ~has_trailing_comment
    ~contains_hardline
    ~item_count
    ~suffix_width
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

let binding_rhs = fun
  ?flat_width
  ?(suffix_width = 0)
  ~force_body_break
  ~has_leading_comment
  ~is_pipeline
  ~is_assignment
  ~inline_body
  ~single_constructor_payload
  ~known_width_overflow
  ~is_multiline
  () ->
  let pressure =
    if force_body_break then
      Layout.Strong [ Layout.Parent_requires_block ]
    else if is_pipeline then
      Layout.Strong [ Layout.Pipeline_body ]
    else if is_assignment then
      Layout.Strong [ Layout.Assignment_body ]
    else if inline_body then
      Layout.Soft [ Layout.Inline_rhs_body ]
    else if single_constructor_payload then
      Layout.Soft [ Layout.Single_constructor_payload ]
    else if known_width_overflow then
      Layout.Strong [ Layout.Known_width_overflow ]
    else if is_multiline then
      Layout.Strong [ Layout.Child_is_block ]
    else
      Layout.Flat
  in
  expr ?flat_width ~pressure ~has_leading_comment ~suffix_width ()
