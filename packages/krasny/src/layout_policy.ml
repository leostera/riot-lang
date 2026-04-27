open Std

type reason =
  | Has_leading_comment
  | Has_trailing_comment
  | Contains_hardline
  | Contains_section_doc
  | Child_is_block
  | Width_overflow of { flat_width: int; remaining: int }
  | Long_infix_chain of { operator: string; terms: int }
  | Heavy_nested_apply
  | Pipeline_body
  | Assignment_body
  | Inline_rhs_body
  | Known_width_overflow
  | Single_constructor_payload
  | Parent_requires_block
  | Opaque_payload

type role =
  | Top_expr
  | Let_rhs
  | Match_case_body
  | If_condition
  | If_branch
  | Function_body of { force_apply_break: bool }
  | Apply_callee
  | Apply_arg of { index: int; broken_parent: bool }
  | Record_field_value
  | Type_after_colon
  | Type_after_equals
  | Pattern_atom
  | Delimited_body

type delimited_kind =
  | Parens
  | Brackets
  | Braces
  | Begin_end
  | Sig_end
  | Struct_end

type separated_kind =
  | Tuple
  | List
  | Array
  | Record_fields
  | Variant_rows

type operator_class = { text: string; always_breaks_pipeline: bool; breaks_when_long: bool }

type binding_kind =
  | Let_binding
  | Record_field
  | Method_definition

type keyword_kind =
  | If_condition
  | If_then
  | If_else
  | Match_case
  | Try_case
  | Fun_body

type separator_kind =
  | Colon
  | Coerce
  | Equals
  | Arrow
  | With

type family =
  | Delimited of delimited_kind
  | Separated of separated_kind
  | Application
  | Infix_chain of operator_class
  | Binding_rhs of binding_kind
  | Keyword_clause of keyword_kind
  | After_separator of separator_kind
  | Top_level_join

type mode =
  | Inline
  | Hang of int
  | Vertical
  | Block
  | Isolate_child_blocks
  | Break_after_separator

type callee_class =
  | Constructor_like
  | Ordinary
  | Operator_like
  | Unknown_callee

type style = {
  continuation_indent: int;
  long_infix_chain_terms: int;
}

type context = { width: int; column: int; indent: int; role: role; style: style }

type decision = {
  mode: mode;
  reasons: reason list;
}

let default_style = {
  continuation_indent = 2;
  long_infix_chain_terms = 8;
}

let make_context = fun ?(role = Top_expr) ?(style = default_style) ~width ~column ~indent () ->
  {
    width;
    column;
    indent;
    role;
    style;
  }

let has_reason = fun expected reasons ->
  List.exists
    (fun reason ->
      match (expected, reason) with
      | (Has_leading_comment, Has_leading_comment)
      | (Has_trailing_comment, Has_trailing_comment)
      | (Contains_hardline, Contains_hardline)
      | (Contains_section_doc, Contains_section_doc)
      | (Child_is_block, Child_is_block)
      | (Heavy_nested_apply, Heavy_nested_apply)
      | (Pipeline_body, Pipeline_body)
      | (Assignment_body, Assignment_body)
      | (Inline_rhs_body, Inline_rhs_body)
      | (Known_width_overflow, Known_width_overflow)
      | (Single_constructor_payload, Single_constructor_payload)
      | (Parent_requires_block, Parent_requires_block)
      | (Opaque_payload, Opaque_payload) -> true
      | (Width_overflow _, Width_overflow _)
      | (Long_infix_chain _, Long_infix_chain _) -> true
      | _ -> false)
    reasons

let remaining_width = fun ctx -> Int.max 0 (Int.sub ctx.width ctx.column)

let fits_flat = fun ctx ?(prefix = 0) ?(suffix_width = 0) flat_width ->
  match flat_width with
  | None -> false
  | Some width -> Int.(ctx.column + prefix + width + suffix_width <= ctx.width)

let width_overflow_reason_from_flat = fun ctx flat_width ->
  match flat_width with
  | Some flat_width -> Width_overflow { flat_width; remaining = remaining_width ctx }
  | None -> Width_overflow { flat_width = 0; remaining = remaining_width ctx }

let inline = { mode = Inline; reasons = [] }

let trace_enabled =
  match Env.get Env.Bool ~var:"KRASNY_LAYOUT_TRACE" with
  | Some true -> true
  | Some false
  | None -> false

let mode_to_string = function
  | Inline -> "Inline"
  | Hang indent -> "Hang(" ^ Int.to_string indent ^ ")"
  | Vertical -> "Vertical"
  | Block -> "Block"
  | Isolate_child_blocks -> "Isolate_child_blocks"
  | Break_after_separator -> "Break_after_separator"

let family_to_string = function
  | Delimited Parens -> "Delimited(Parens)"
  | Delimited Brackets -> "Delimited(Brackets)"
  | Delimited Braces -> "Delimited(Braces)"
  | Delimited Begin_end -> "Delimited(Begin_end)"
  | Delimited Sig_end -> "Delimited(Sig_end)"
  | Delimited Struct_end -> "Delimited(Struct_end)"
  | Separated Tuple -> "Separated(Tuple)"
  | Separated List -> "Separated(List)"
  | Separated Array -> "Separated(Array)"
  | Separated Record_fields -> "Separated(Record_fields)"
  | Separated Variant_rows -> "Separated(Variant_rows)"
  | Application -> "Application"
  | Infix_chain operator -> "Infix_chain(" ^ operator.text ^ ")"
  | Binding_rhs Let_binding -> "Binding_rhs(Let_binding)"
  | Binding_rhs Record_field -> "Binding_rhs(Record_field)"
  | Binding_rhs Method_definition -> "Binding_rhs(Method_definition)"
  | Keyword_clause If_condition -> "Keyword_clause(If_condition)"
  | Keyword_clause If_then -> "Keyword_clause(If_then)"
  | Keyword_clause If_else -> "Keyword_clause(If_else)"
  | Keyword_clause Match_case -> "Keyword_clause(Match_case)"
  | Keyword_clause Try_case -> "Keyword_clause(Try_case)"
  | Keyword_clause Fun_body -> "Keyword_clause(Fun_body)"
  | After_separator Colon -> "After_separator(Colon)"
  | After_separator Coerce -> "After_separator(Coerce)"
  | After_separator Equals -> "After_separator(Equals)"
  | After_separator Arrow -> "After_separator(Arrow)"
  | After_separator With -> "After_separator(With)"
  | Top_level_join -> "Top_level_join"

let reason_to_string = function
  | Has_leading_comment -> "Has_leading_comment"
  | Has_trailing_comment -> "Has_trailing_comment"
  | Contains_hardline -> "Contains_hardline"
  | Contains_section_doc -> "Contains_section_doc"
  | Child_is_block -> "Child_is_block"
  | Width_overflow { flat_width; remaining } ->
      "Width_overflow(flat="
      ^ Int.to_string flat_width
      ^ ", remaining="
      ^ Int.to_string remaining
      ^ ")"
  | Long_infix_chain { operator; terms } ->
      "Long_infix_chain(operator=" ^ operator ^ ", terms=" ^ Int.to_string terms ^ ")"
  | Heavy_nested_apply -> "Heavy_nested_apply"
  | Pipeline_body -> "Pipeline_body"
  | Assignment_body -> "Assignment_body"
  | Inline_rhs_body -> "Inline_rhs_body"
  | Known_width_overflow -> "Known_width_overflow"
  | Single_constructor_payload -> "Single_constructor_payload"
  | Parent_requires_block -> "Parent_requires_block"
  | Opaque_payload -> "Opaque_payload"

let reasons_to_string = fun reasons ->
  match reasons with
  | [] -> "[]"
  | _ -> "[" ^ String.concat ", " (List.map reasons ~fn:reason_to_string) ^ "]"

let trace_line_from_width = fun family ctx ~flat_width decision ->
  "krasny layout: "
  ^ family_to_string family
  ^ " column="
  ^ Int.to_string ctx.column
  ^ " width="
  ^ Int.to_string ctx.width
  ^ " flat=" ^ (
    match flat_width with
    | Some width -> Int.to_string width
    | None -> "unknown"
  ) ^ " -> " ^ mode_to_string decision.mode ^ " " ^ reasons_to_string decision.reasons

let trace_decision_from_width = fun family ctx ~flat_width decision ->
  if trace_enabled then
    eprintln (trace_line_from_width family ctx ~flat_width decision)

let decide_application = fun
  ctx
  ~flat_width
  ~suffix_width
  ~arg_count
  ~callee_class
  ~force_parent_break
  ~has_multiline_args
  ~has_heavy_nested_apply ->
  let single_constructor_payload = Int.equal arg_count 1 && callee_class = Constructor_like in
  let decision =
    if single_constructor_payload then
      { mode = Inline; reasons = [ Single_constructor_payload ] }
    else if force_parent_break then
      { mode = Vertical; reasons = [ Parent_requires_block ] }
    else if has_heavy_nested_apply then
      { mode = Vertical; reasons = [ Heavy_nested_apply ] }
    else if has_multiline_args && Int.(arg_count > 1) then
      { mode = Vertical; reasons = [ Child_is_block ] }
    else if has_multiline_args then
      { mode = Isolate_child_blocks; reasons = [ Child_is_block ] }
    else if fits_flat ctx ~suffix_width flat_width then
      inline
    else
      {
        mode = Hang ctx.style.continuation_indent;
        reasons = [ width_overflow_reason_from_flat ctx flat_width ];
      }
  in
  trace_decision_from_width Application ctx ~flat_width decision;
  decision

let decide_type_after_separator = fun separator ctx ~flat_width ~suffix_width ->
  let family = After_separator separator in
  let decision =
    match flat_width with
    | None -> inline
    | Some _ ->
        if fits_flat ctx ~prefix:1 ~suffix_width flat_width then
          inline
        else
          {
            mode = Break_after_separator;
            reasons = [ width_overflow_reason_from_flat ctx flat_width ];
          }
  in
  trace_decision_from_width family ctx ~flat_width decision;
  decision

let decide_separated = fun kind ctx ~flat_width ~allow_inline ->
  let family = Separated kind in
  let decision =
    if allow_inline then
      if fits_flat ctx flat_width then
        inline
      else
        { mode = Block; reasons = [ width_overflow_reason_from_flat ctx flat_width ] }
    else
      { mode = Block; reasons = [ Child_is_block ] }
  in
  trace_decision_from_width family ctx ~flat_width decision;
  decision

let decide_record_expr = fun ctx ~flat_width ~allow_inline ~item_count:_ ->
  decide_separated
    Record_fields
    ctx
    ~flat_width
    ~allow_inline

let decide_record_type = fun
  ctx
  ~flat_width
  ~allow_inline
  ~has_leading_comment
  ~has_trailing_comment
  ~item_count:_ ->
  let family = Separated Record_fields in
  let decision =
    if has_leading_comment then
      { mode = Block; reasons = [ Has_leading_comment ] }
    else if has_trailing_comment then
      { mode = Block; reasons = [ Has_trailing_comment ] }
    else if not allow_inline then
      { mode = Block; reasons = [ Child_is_block ] }
    else if fits_flat ctx flat_width then
      inline
    else
      { mode = Block; reasons = [ width_overflow_reason_from_flat ctx flat_width ] }
  in
  trace_decision_from_width family ctx ~flat_width decision;
  decision

let decide_tuple = fun ctx ~flat_width ~has_nonfinal_fun_item ->
  let family = Separated Tuple in
  let decision =
    if has_nonfinal_fun_item then
      { mode = Block; reasons = [ Child_is_block ] }
    else
      match flat_width with
      | None -> inline
      | Some _ ->
          if fits_flat ctx flat_width then
            inline
          else
            { mode = Block; reasons = [ width_overflow_reason_from_flat ctx flat_width ] }
  in
  trace_decision_from_width family ctx ~flat_width decision;
  decision

let decide_parenthesized_expr = fun ctx ~has_leading_comment ~is_multiline ~break_after_separator ->
  let flat_width = Some 0 in
  let decision =
    if has_leading_comment then
      { mode = Block; reasons = [ Has_leading_comment ] }
    else if is_multiline then
      { mode = Block; reasons = [ Child_is_block ] }
    else if break_after_separator then
      { mode = Block; reasons = [ Parent_requires_block ] }
    else
      inline
  in
  trace_decision_from_width (Delimited Parens) ctx ~flat_width decision;
  decision

let decide_infix_chain = fun ctx operator ~flat_width ~item_count ->
  let family = Infix_chain operator in
  let decision =
    if operator.always_breaks_pipeline then
      { mode = Vertical; reasons = [] }
    else if operator.breaks_when_long && Int.(item_count >= ctx.style.long_infix_chain_terms) then
      {
        mode = Vertical;
        reasons = [ Long_infix_chain { operator = operator.text; terms = item_count } ];
      }
    else
      match flat_width with
      | None -> inline
      | Some _ ->
          if fits_flat ctx flat_width then
            inline
          else
            { mode = Vertical; reasons = [ width_overflow_reason_from_flat ctx flat_width ] }
  in
  trace_decision_from_width family ctx ~flat_width decision;
  decision

let decide_let_binding_rhs = fun
  ctx
  ~flat_width
  ~suffix_width
  ~force_body_break
  ~has_leading_comment
  ~is_pipeline
  ~is_assignment
  ~inline_body
  ~single_constructor_payload
  ~known_width_overflow
  ~is_multiline ->
  let decision =
    if has_leading_comment then
      { mode = Block; reasons = [ Has_leading_comment ] }
    else if force_body_break then
      { mode = Block; reasons = [ Parent_requires_block ] }
    else if is_pipeline then
      { mode = Block; reasons = [ Pipeline_body ] }
    else if is_assignment then
      { mode = Block; reasons = [ Assignment_body ] }
    else if inline_body then
      { mode = Inline; reasons = [ Inline_rhs_body ] }
    else if single_constructor_payload then
      { mode = Inline; reasons = [ Single_constructor_payload ] }
    else if known_width_overflow then
      { mode = Block; reasons = [ Known_width_overflow ] }
    else if is_multiline then
      { mode = Block; reasons = [ Child_is_block ] }
    else
      match flat_width with
      | None -> inline
      | Some _ ->
          if fits_flat ctx ~prefix:1 ~suffix_width flat_width then
            inline
          else
            { mode = Block; reasons = [ width_overflow_reason_from_flat ctx flat_width ] }
  in
  trace_decision_from_width (Binding_rhs Let_binding) ctx ~flat_width decision;
  decision

let decide_if_condition = fun ctx ~flat_width ~suffix_width ->
  let family = Keyword_clause If_condition in
  let decision =
    match flat_width with
    | None -> inline
    | Some _ ->
        if fits_flat ctx ~prefix:1 ~suffix_width flat_width then
          inline
        else
          { mode = Block; reasons = [ width_overflow_reason_from_flat ctx flat_width ] }
  in
  trace_decision_from_width family ctx ~flat_width decision;
  decision
