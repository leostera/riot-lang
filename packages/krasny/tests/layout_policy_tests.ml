open Std

module Layout = Krasny.Layout_policy

let trace_direct = fun family ctx ~flat_width decision ->
  Layout.trace_line_from_width
    family
    ctx
    ~flat_width
    decision

let test_layout_trace_snapshots_application_width_overflow = fun ctx ->
  let render_ctx = Layout.make_context ~width:100 ~column:18 ~indent:2 () in
  let flat_width = Some 120 in
  let decision =
    Layout.decide_application
      render_ctx
      ~flat_width
      ~suffix_width:0
      ~arg_count:3
      ~callee_class:Layout.Ordinary
      ~force_parent_break:false
      ~has_multiline_args:false
      ~has_heavy_nested_apply:false
  in
  let actual = trace_direct Layout.Application render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Application column=18 width=100 flat=120 -> Hang(2) [Width_overflow(flat=120, remaining=82)]|}

let test_layout_trace_snapshots_long_infix_reasons = fun ctx ->
  let render_ctx = Layout.make_context ~width:100 ~column:0 ~indent:0 () in
  let operator = { Layout.text = "&&"; always_breaks_pipeline = false; breaks_when_long = true } in
  let flat_width = Some 30 in
  let decision = Layout.decide_infix_chain render_ctx operator ~flat_width ~item_count:8 in
  let actual = trace_direct (Layout.Infix_chain operator) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Infix_chain(&&) column=0 width=100 flat=30 -> Vertical [Long_infix_chain(operator=&&, terms=8)]|}

let test_layout_trace_snapshots_unknown_widths_and_comments = fun ctx ->
  let render_ctx = Layout.make_context ~width:80 ~column:12 ~indent:2 () in
  let flat_width = None in
  let decision =
    Layout.decide_let_binding_rhs
      render_ctx
      ~flat_width
      ~suffix_width:0
      ~force_body_break:false
      ~has_leading_comment:true
      ~is_pipeline:false
      ~is_assignment:false
      ~inline_body:false
      ~inline_body_handles_width_overflow:false
      ~single_constructor_payload:false
      ~known_width_overflow:false
      ~is_multiline:false
  in
  let actual =
    trace_direct (Layout.Binding_rhs Layout.Let_binding) render_ctx ~flat_width decision
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Binding_rhs(Let_binding) column=12 width=80 flat=unknown -> Block [Has_leading_comment]|}

let test_layout_trace_snapshots_let_rhs_width_vetoes_inline_body = fun ctx ->
  let render_ctx = Layout.make_context ~role:Layout.Let_rhs ~width:100 ~column:58 ~indent:0 () in
  let flat_width = Some 42 in
  let decision =
    Layout.decide_let_binding_rhs
      render_ctx
      ~flat_width
      ~suffix_width:0
      ~force_body_break:false
      ~has_leading_comment:false
      ~is_pipeline:false
      ~is_assignment:false
      ~inline_body:true
      ~inline_body_handles_width_overflow:false
      ~single_constructor_payload:false
      ~known_width_overflow:true
      ~is_multiline:false
  in
  let actual =
    trace_direct (Layout.Binding_rhs Layout.Let_binding) render_ctx ~flat_width decision
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Binding_rhs(Let_binding) column=58 width=100 flat=42 -> Block [Known_width_overflow]|}

let test_layout_trace_snapshots_let_rhs_fun_wrapper_handles_width_overflow = fun ctx ->
  let render_ctx = Layout.make_context ~role:Layout.Let_rhs ~width:100 ~column:58 ~indent:0 () in
  let flat_width = Some 42 in
  let decision =
    Layout.decide_let_binding_rhs
      render_ctx
      ~flat_width
      ~suffix_width:0
      ~force_body_break:false
      ~has_leading_comment:false
      ~is_pipeline:false
      ~is_assignment:false
      ~inline_body:true
      ~inline_body_handles_width_overflow:true
      ~single_constructor_payload:false
      ~known_width_overflow:true
      ~is_multiline:false
  in
  let actual =
    trace_direct (Layout.Binding_rhs Layout.Let_binding) render_ctx ~flat_width decision
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Binding_rhs(Let_binding) column=58 width=100 flat=42 -> Inline [Inline_rhs_body]|}

let test_layout_trace_snapshots_separated_list_width_overflow = fun ctx ->
  let render_ctx = Layout.make_context ~width:20 ~column:8 ~indent:2 () in
  let flat_width = Some 18 in
  let decision = Layout.decide_separated Layout.List render_ctx ~flat_width ~allow_inline:true in
  let actual = trace_direct (Layout.Separated Layout.List) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Separated(List) column=8 width=20 flat=18 -> Block [Width_overflow(flat=18, remaining=12)]|}

let test_layout_trace_snapshots_separated_array_child_pressure = fun ctx ->
  let render_ctx = Layout.make_context ~width:100 ~column:0 ~indent:0 () in
  let flat_width = Some 18 in
  let decision = Layout.decide_separated Layout.Array render_ctx ~flat_width ~allow_inline:false in
  let actual = trace_direct (Layout.Separated Layout.Array) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Separated(Array) column=0 width=100 flat=18 -> Block [Child_is_block]|}

let test_layout_trace_snapshots_tuple_width_overflow = fun ctx ->
  let render_ctx = Layout.make_context ~width:20 ~column:8 ~indent:2 () in
  let flat_width = Some 18 in
  let decision = Layout.decide_tuple render_ctx ~flat_width ~has_nonfinal_fun_item:false in
  let actual = trace_direct (Layout.Separated Layout.Tuple) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Separated(Tuple) column=8 width=20 flat=18 -> Block [Width_overflow(flat=18, remaining=12)]|}

let test_layout_trace_snapshots_tuple_unknown_width_stays_inline = fun ctx ->
  let render_ctx = Layout.make_context ~width:20 ~column:8 ~indent:2 () in
  let flat_width = None in
  let decision = Layout.decide_tuple render_ctx ~flat_width ~has_nonfinal_fun_item:false in
  let actual = trace_direct (Layout.Separated Layout.Tuple) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Separated(Tuple) column=8 width=20 flat=unknown -> Inline []|}

let test_layout_trace_snapshots_tuple_nonfinal_function_item = fun ctx ->
  let render_ctx = Layout.make_context ~width:100 ~column:0 ~indent:0 () in
  let flat_width = Some 30 in
  let decision = Layout.decide_tuple render_ctx ~flat_width ~has_nonfinal_fun_item:true in
  let actual = trace_direct (Layout.Separated Layout.Tuple) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Separated(Tuple) column=0 width=100 flat=30 -> Block [Child_is_block]|}

let test_layout_trace_snapshots_if_condition_overflow = fun ctx ->
  let render_ctx = Layout.make_context ~width:40 ~column:3 ~indent:0 () in
  let flat_width = Some 34 in
  let decision = Layout.decide_if_condition render_ctx ~flat_width ~suffix_width:6 in
  let actual =
    trace_direct (Layout.Keyword_clause Layout.If_condition) render_ctx ~flat_width decision
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Keyword_clause(If_condition) column=3 width=40 flat=34 -> Block [Width_overflow(flat=34, remaining=37)]|}

let test_layout_trace_snapshots_if_condition_unknown_width_stays_inline = fun ctx ->
  let render_ctx = Layout.make_context ~width:40 ~column:3 ~indent:0 () in
  let flat_width = None in
  let decision = Layout.decide_if_condition render_ctx ~flat_width ~suffix_width:6 in
  let actual =
    trace_direct (Layout.Keyword_clause Layout.If_condition) render_ctx ~flat_width decision
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Keyword_clause(If_condition) column=3 width=40 flat=unknown -> Inline []|}

let test_layout_trace_snapshots_type_separator_width_overflow = fun ctx ->
  let render_ctx = Layout.make_context ~width:40 ~column:10 ~indent:2 () in
  let flat_width = Some 35 in
  let decision =
    Layout.decide_type_after_separator Layout.Colon render_ctx ~flat_width ~suffix_width:2
  in
  let actual = trace_direct (Layout.After_separator Layout.Colon) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: After_separator(Colon) column=10 width=40 flat=35 -> Break_after_separator [Width_overflow(flat=35, remaining=30)]|}

let test_layout_trace_snapshots_parenthesized_separator_break = fun ctx ->
  let render_ctx = Layout.make_context ~width:100 ~column:0 ~indent:0 () in
  let flat_width = Some 0 in
  let decision =
    Layout.decide_parenthesized_expr
      render_ctx
      ~has_leading_comment:false
      ~is_multiline:false
      ~break_after_separator:true
  in
  let actual = trace_direct (Layout.Delimited Layout.Parens) render_ctx ~flat_width decision in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Delimited(Parens) column=0 width=100 flat=0 -> Block [Parent_requires_block]|}

let test_layout_trace_snapshots_record_type_leading_comment = fun ctx ->
  let render_ctx = Layout.make_context ~width:100 ~column:0 ~indent:0 () in
  let flat_width = Some 20 in
  let decision =
    Layout.decide_record_type
      render_ctx
      ~flat_width
      ~allow_inline:true
      ~has_leading_comment:true
      ~has_trailing_comment:false
      ~item_count:1
  in
  let actual =
    trace_direct (Layout.Separated Layout.Record_fields) render_ctx ~flat_width decision
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Separated(Record_fields) column=0 width=100 flat=20 -> Block [Has_leading_comment]|}

let test_layout_trace_snapshots_let_binding_pipeline_body = fun ctx ->
  let render_ctx = Layout.make_context ~role:Layout.Let_rhs ~width:100 ~column:4 ~indent:2 () in
  let flat_width = Some 20 in
  let decision =
    Layout.decide_let_binding_rhs
      render_ctx
      ~flat_width
      ~suffix_width:0
      ~force_body_break:false
      ~has_leading_comment:false
      ~is_pipeline:true
      ~is_assignment:false
      ~inline_body:false
      ~inline_body_handles_width_overflow:false
      ~single_constructor_payload:false
      ~known_width_overflow:false
      ~is_multiline:false
  in
  let actual =
    trace_direct (Layout.Binding_rhs Layout.Let_binding) render_ctx ~flat_width decision
  in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:{|krasny layout: Binding_rhs(Let_binding) column=4 width=100 flat=20 -> Block [Pipeline_body]|}

let tests =
  Test.[
    case
      "layout trace snapshots application width overflow"
      test_layout_trace_snapshots_application_width_overflow;
    case "layout trace snapshots long infix reasons" test_layout_trace_snapshots_long_infix_reasons;
    case
      "layout trace snapshots unknown widths and comments"
      test_layout_trace_snapshots_unknown_widths_and_comments;
    case
      "layout trace snapshots let rhs width vetoes inline body"
      test_layout_trace_snapshots_let_rhs_width_vetoes_inline_body;
    case
      "layout trace snapshots let rhs fun wrapper handles width overflow"
      test_layout_trace_snapshots_let_rhs_fun_wrapper_handles_width_overflow;
    case
      "layout trace snapshots separated list width overflow"
      test_layout_trace_snapshots_separated_list_width_overflow;
    case
      "layout trace snapshots separated array child pressure"
      test_layout_trace_snapshots_separated_array_child_pressure;
    case
      "layout trace snapshots tuple width overflow"
      test_layout_trace_snapshots_tuple_width_overflow;
    case
      "layout trace snapshots tuple unknown width stays inline"
      test_layout_trace_snapshots_tuple_unknown_width_stays_inline;
    case
      "layout trace snapshots tuple nonfinal function item"
      test_layout_trace_snapshots_tuple_nonfinal_function_item;
    case
      "layout trace snapshots if condition overflow"
      test_layout_trace_snapshots_if_condition_overflow;
    case
      "layout trace snapshots if condition unknown width stays inline"
      test_layout_trace_snapshots_if_condition_unknown_width_stays_inline;
    case
      "layout trace snapshots type separator width overflow"
      test_layout_trace_snapshots_type_separator_width_overflow;
    case
      "layout trace snapshots parenthesized separator break"
      test_layout_trace_snapshots_parenthesized_separator_break;
    case
      "layout trace snapshots record type leading comment"
      test_layout_trace_snapshots_record_type_leading_comment;
    case
      "layout trace snapshots let binding pipeline body"
      test_layout_trace_snapshots_let_binding_pipeline_body;
  ]

let main ~args:_ = Test.Cli.main ~name:"krasny:layout_policy" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
