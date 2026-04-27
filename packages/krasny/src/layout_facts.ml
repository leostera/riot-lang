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
  ?(suffix_width = 0)
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
  expr ?flat_width ~pressure ~item_count:arg_count ~suffix_width ~callee_class ()

let infix_chain = fun ?flat_width ~item_count () ->
  expr
    ?flat_width
    ~pressure:Layout.Flat
    ~item_count
    ()

let record_expr = fun ?flat_width ~allow_inline ~item_count () ->
  let pressure =
    if allow_inline then
      Layout.Flat
    else
      Layout.Strong [ Layout.Child_is_block ]
  in
  expr ?flat_width ~pressure ~item_count ()

let record_type = fun
  ?flat_width
  ~allow_inline
  ~has_leading_comment
  ~has_trailing_comment
  ~item_count
  () ->
  let pressure =
    if allow_inline then
      Layout.Flat
    else
      Layout.Strong [ Layout.Child_is_block ]
  in
  type_expr ?flat_width ~pressure ~has_leading_comment ~has_trailing_comment ~item_count ()

let parenthesized_expr = fun ~has_leading_comment ~is_multiline ~break_after_separator () ->
  let pressure =
    if is_multiline then
      Layout.Strong [ Layout.Child_is_block ]
    else if break_after_separator then
      Layout.Strong [ Layout.Parent_requires_block ]
    else
      Layout.Flat
  in
  expr ~flat_width:0 ~pressure ~has_leading_comment ()

let type_after_separator = fun ?flat_width ~suffix_width () ->
  type_expr
    ?flat_width
    ~suffix_width
    ()

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
