open Std

module Layout = Krasny.Layout_policy

let trace_decision = fun family ctx facts ->
  let decision = Layout.decide family ctx facts in
  Layout.trace_line family ctx facts decision

let trace_direct = fun family ctx ~flat_width decision ->
  Layout.trace_line_from_width
    family
    ctx
    ~flat_width
    decision

let tests = [
  Test.case
    "layout trace snapshots application width overflow"
    (fun ctx ->
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
        ~expected:{|krasny layout: Application column=18 width=100 flat=120 -> Hang(2) [Width_overflow(flat=120, remaining=82)]|});
  Test.case
    "layout trace snapshots long infix reasons"
    (fun ctx ->
      let render_ctx = Layout.make_context ~width:100 ~column:0 ~indent:0 () in
      let operator = { Layout.text = "&&"; always_breaks_pipeline = false; breaks_when_long = true }
      in
      let facts = Layout.make_facts ~flat_width:30 ~item_count:8 ~syntax_family:Layout.Expr () in
      let actual = trace_decision (Layout.Infix_chain operator) render_ctx facts in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{|krasny layout: Infix_chain(&&) column=0 width=100 flat=30 -> Vertical [Long_infix_chain(operator=&&, terms=8)]|});
  Test.case
    "layout trace snapshots unknown widths and comments"
    (fun ctx ->
      let render_ctx = Layout.make_context ~width:80 ~column:12 ~indent:2 () in
      let facts = Layout.make_facts ~has_leading_comment:true ~syntax_family:Layout.Expr () in
      let actual = trace_decision (Layout.Binding_rhs Layout.Let_binding) render_ctx facts in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{|krasny layout: Binding_rhs(Let_binding) column=12 width=80 flat=unknown -> Block [Has_leading_comment]|});
  Test.case
    "layout trace snapshots separated list width overflow"
    (fun ctx ->
      let render_ctx = Layout.make_context ~width:20 ~column:8 ~indent:2 () in
      let flat_width = Some 18 in
      let decision =
        Layout.decide_separated Layout.List render_ctx ~flat_width ~allow_inline:true
      in
      let actual = trace_direct (Layout.Separated Layout.List) render_ctx ~flat_width decision in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{|krasny layout: Separated(List) column=8 width=20 flat=18 -> Block [Width_overflow(flat=18, remaining=12)]|});
  Test.case
    "layout trace snapshots separated array child pressure"
    (fun ctx ->
      let render_ctx = Layout.make_context ~width:100 ~column:0 ~indent:0 () in
      let flat_width = Some 18 in
      let decision =
        Layout.decide_separated Layout.Array render_ctx ~flat_width ~allow_inline:false
      in
      let actual = trace_direct (Layout.Separated Layout.Array) render_ctx ~flat_width decision in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{|krasny layout: Separated(Array) column=0 width=100 flat=18 -> Block [Child_is_block]|});
  Test.case
    "layout trace snapshots if condition overflow"
    (fun ctx ->
      let render_ctx = Layout.make_context ~width:40 ~column:3 ~indent:0 () in
      let flat_width = Some 34 in
      let decision = Layout.decide_if_condition render_ctx ~flat_width ~suffix_width:6 in
      let actual =
        trace_direct (Layout.Keyword_clause Layout.If_condition) render_ctx ~flat_width decision
      in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{|krasny layout: Keyword_clause(If_condition) column=3 width=40 flat=34 -> Block [Width_overflow(flat=34, remaining=37)]|});
  Test.case
    "layout trace snapshots if condition unknown width stays inline"
    (fun ctx ->
      let render_ctx = Layout.make_context ~width:40 ~column:3 ~indent:0 () in
      let flat_width = None in
      let decision = Layout.decide_if_condition render_ctx ~flat_width ~suffix_width:6 in
      let actual =
        trace_direct (Layout.Keyword_clause Layout.If_condition) render_ctx ~flat_width decision
      in
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual
        ~expected:{|krasny layout: Keyword_clause(If_condition) column=3 width=40 flat=unknown -> Inline []|});
]

let main ~args:_ = Test.Cli.main ~name:"krasny:layout_policy" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
