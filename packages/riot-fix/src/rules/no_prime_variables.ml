open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-prime-variables"

let rule_description = "Variable names should not contain apostrophes"

let rule_explain =
  {|
Apostrophes are compact, but they do not explain how the new value relates to
the old one. Names such as `next_state`, `updated_state`, or `state2` are a
little longer, but they make data flow explicit and keep later edits easier to
review.
|}

let contains_prime = fun text -> String.exists ~fn:(fun ch -> Char.equal ch '\'') text

let trailing_prime_count = fun text ->
  let rec loop index count =
    if index < 0 then
      count
    else if Char.equal (String.get_unchecked text ~at:index) '\'' then
      loop (index - 1) (count + 1)
    else
      count
  in
  loop (String.length text - 1) 0

let replacement_for = fun text ->
  let trailing_primes = trailing_prime_count text in
  if trailing_primes > 0 then
    let base = String.sub text ~offset:0 ~len:(String.length text - trailing_primes) in
    base ^ Int.to_string (trailing_primes + 1)
  else
    String.map
      ~fn:(fun ch ->
        if Char.equal ch '\'' then
          '2'
        else
          ch)
      text

let make_fix = fun token replacement ->
  H.replace_token_fix
    ~title:("Rename " ^ Ast.Token.text token ^ " to " ^ replacement)
    ~token
    ~text:replacement

let make_diagnostic = fun token ->
  let original = Ast.Token.text token in
  let replacement = replacement_for original in
  H.diagnostic_for_token
    ~rule_id
    ~message:rule_description
    ~token
    ~suggestion:("Rename " ^ original ^ " to " ^ replacement)
    ~fix:(make_fix token replacement)
    ()

let check_binding = fun binding diagnostics ->
  if H.binding_is_function binding then
    ()
  else
    match H.binding_name_token binding with
    | Some token when contains_prime (Ast.Token.text token) ->
        H.push_diagnostic diagnostics (make_diagnostic token)
    | _ -> ()

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  H.for_each_let_binding root ~fn:(fun binding -> check_binding binding diagnostics);
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
