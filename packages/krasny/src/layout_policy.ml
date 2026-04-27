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

type pressure =
  | Flat
  | Soft of reason list
  | Strong of reason list
  | Hard of reason list

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
  | Flow
  | Hang of int
  | Vertical
  | Block
  | Isolate_child_blocks
  | Break_after_separator
  | Tight
  | Blank_line

type callee_class =
  | Constructor_like
  | Ordinary
  | Operator_like
  | Unknown_callee

type syntax_family =
  | Expr
  | Type_expr
  | Pattern
  | Declaration
  | Trivia
  | Unknown_syntax

type facts = {
  flat_width: int option;
  pressure: pressure;
  has_leading_comment: bool;
  has_trailing_comment: bool;
  contains_hardline: bool;
  item_count: int;
  suffix_width: int;
  syntax_family: syntax_family;
  callee_class: callee_class option;
}

type style = {
  continuation_indent: int;
  block_indent: int;
  long_infix_chain_terms: int;
  heavy_apply_arg_threshold: int;
}

type context = { width: int; column: int; indent: int; role: role; style: style }

type decision = {
  mode: mode;
  reasons: reason list;
}

let default_style = {
  continuation_indent = 2;
  block_indent = 2;
  long_infix_chain_terms = 8;
  heavy_apply_arg_threshold = 2;
}

let make_facts = fun
  ?flat_width
  ?(pressure = Flat)
  ?(has_leading_comment = false)
  ?(has_trailing_comment = false)
  ?(contains_hardline = false)
  ?(item_count = 0)
  ?(suffix_width = 0)
  ?(syntax_family = Unknown_syntax)
  ?callee_class
  () ->
  {
    flat_width;
    pressure;
    has_leading_comment;
    has_trailing_comment;
    contains_hardline;
    item_count;
    suffix_width;
    syntax_family;
    callee_class;
  }

let make_context = fun ?(role = Top_expr) ?(style = default_style) ~width ~column ~indent () ->
  {
    width;
    column;
    indent;
    role;
    style;
  }

let pressure_reasons = function
  | Flat -> []
  | Soft reasons
  | Strong reasons
  | Hard reasons -> reasons

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

let fits = fun ctx ?(prefix = 0) ?(suffix = 0) facts ->
  match facts.flat_width with
  | None -> false
  | Some width -> Int.(ctx.column + prefix + width + facts.suffix_width + suffix <= ctx.width)

let width_overflow_reason = fun ctx facts ->
  match facts.flat_width with
  | Some flat_width -> Width_overflow { flat_width; remaining = remaining_width ctx }
  | None -> Width_overflow { flat_width = 0; remaining = remaining_width ctx }

let inline = { mode = Inline; reasons = [] }

let is_single_constructor_payload = fun facts ->
  match (facts.callee_class, facts.item_count) with
  | (Some Constructor_like, 1) -> true
  | _ -> false

let decide_application = fun ctx facts ->
  if facts.contains_hardline then
    { mode = Vertical; reasons = [ Contains_hardline ] }
  else if is_single_constructor_payload facts then
    { mode = Inline; reasons = [ Single_constructor_payload ] }
  else
    match facts.pressure with
    | Hard reasons -> { mode = Vertical; reasons }
    | Strong reasons ->
        if has_reason Heavy_nested_apply reasons then
          { mode = Vertical; reasons }
        else if has_reason Parent_requires_block reasons then
          { mode = Vertical; reasons }
        else if has_reason Child_is_block reasons then
          { mode = Vertical; reasons }
        else
          { mode = Vertical; reasons }
    | Soft reasons ->
        if has_reason Child_is_block reasons then
          { mode = Isolate_child_blocks; reasons }
        else if fits ctx facts then
          inline
        else
          {
            mode = Hang ctx.style.continuation_indent;
            reasons = [ width_overflow_reason ctx facts ];
          }
    | Flat ->
        if fits ctx facts then
          inline
        else
          {
            mode = Hang ctx.style.continuation_indent;
            reasons = [ width_overflow_reason ctx facts ];
          }

let decide_binding_rhs = fun ctx facts ->
  let reasons = pressure_reasons facts.pressure in
  if has_reason Inline_rhs_body reasons then
    { mode = Inline; reasons = [ Inline_rhs_body ] }
  else if has_reason Single_constructor_payload reasons then
    { mode = Inline; reasons = [ Single_constructor_payload ] }
  else
    match facts.pressure with
    | Hard reasons
    | Strong reasons -> { mode = Block; reasons }
    | Soft _
    | Flat -> (
        match facts.flat_width with
        | None -> inline
        | Some _ ->
            if fits ctx ~prefix:1 facts then
              inline
            else
              { mode = Block; reasons = [ width_overflow_reason ctx facts ] }
      )

let decide_raw = fun family ctx facts ->
  if facts.contains_hardline then
    { mode = Block; reasons = [ Contains_hardline ] }
  else if facts.has_leading_comment then
    { mode = Block; reasons = [ Has_leading_comment ] }
  else if facts.has_trailing_comment then
    { mode = Block; reasons = [ Has_trailing_comment ] }
  else
    match family with
    | Application -> decide_application ctx facts
    | After_separator _ -> (
        match facts.flat_width with
        | None -> inline
        | Some _ ->
            if fits ctx ~prefix:1 facts then
              inline
            else
              { mode = Break_after_separator; reasons = [ width_overflow_reason ctx facts ] }
      )
    | Binding_rhs _ -> decide_binding_rhs ctx facts
    | Keyword_clause If_condition -> (
        match facts.flat_width with
        | None -> inline
        | Some _ ->
            if fits ctx ~prefix:1 facts then
              inline
            else
              { mode = Block; reasons = [ width_overflow_reason ctx facts ] }
      )
    | Infix_chain operator ->
        if operator.always_breaks_pipeline then
          { mode = Vertical; reasons = [] }
        else if
          operator.breaks_when_long && Int.(facts.item_count >= ctx.style.long_infix_chain_terms)
        then
          {
            mode = Vertical;
            reasons = [ Long_infix_chain { operator = operator.text; terms = facts.item_count } ];
          }
        else
          (
            match facts.pressure with
            | Hard reasons
            | Strong reasons -> { mode = Vertical; reasons }
            | Soft reasons when has_reason
              (Long_infix_chain { operator = operator.text; terms = 0 })
              reasons -> { mode = Vertical; reasons }
            | Soft _
            | Flat -> (
                match facts.flat_width with
                | None -> inline
                | Some _ ->
                    if fits ctx facts then
                      inline
                    else
                      { mode = Vertical; reasons = [ width_overflow_reason ctx facts ] }
              )
          )
    | Delimited _
    | Separated _
    | Keyword_clause _
    | Top_level_join -> (
        match facts.pressure with
        | Hard reasons
        | Strong reasons -> { mode = Block; reasons }
        | Soft _
        | Flat ->
            if fits ctx facts then
              inline
            else
              { mode = Block; reasons = [ width_overflow_reason ctx facts ] }
      )

let trace_enabled =
  match Env.get Env.Bool ~var:"KRASNY_LAYOUT_TRACE" with
  | Some true -> true
  | Some false
  | None -> false

let mode_to_string = function
  | Inline -> "Inline"
  | Flow -> "Flow"
  | Hang indent -> "Hang(" ^ Int.to_string indent ^ ")"
  | Vertical -> "Vertical"
  | Block -> "Block"
  | Isolate_child_blocks -> "Isolate_child_blocks"
  | Break_after_separator -> "Break_after_separator"
  | Tight -> "Tight"
  | Blank_line -> "Blank_line"

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

let trace_line = fun family ctx facts decision ->
  "krasny layout: "
  ^ family_to_string family
  ^ " column="
  ^ Int.to_string ctx.column
  ^ " width="
  ^ Int.to_string ctx.width
  ^ " flat=" ^ (
    match facts.flat_width with
    | Some width -> Int.to_string width
    | None -> "unknown"
  ) ^ " -> " ^ mode_to_string decision.mode ^ " " ^ reasons_to_string decision.reasons

let trace_decision = fun family ctx facts decision ->
  if trace_enabled then
    eprintln (trace_line family ctx facts decision)

let decide = fun family ctx facts ->
  let decision = decide_raw family ctx facts in
  trace_decision family ctx facts decision;
  decision
