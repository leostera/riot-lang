open Std
open Std.Collections

type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type red_node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type red_token = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_token
type red_element = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_element

let is_trivia kind =
  let open Syn.SyntaxKind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

(* Core traversal that collects elements *)
let traverse ~visit_node ~visit_token tree =
  let open Syn.Ceibo.Red in
  let rec go elem acc =
    match elem with
    | Node n ->
        let acc = visit_node n acc in
        let children = SyntaxNode.children n in
        let result = ref acc in
        for i = 0 to Array.length children - 1 do
          result := go children.(i) !result
        done;
        !result
    | Token t -> visit_token t acc
  in
  go (Node tree) []

(* Find nodes matching predicate *)
let find_nodes predicate tree =
  traverse
    ~visit_node:(fun node acc -> if predicate node then node :: acc else acc)
    ~visit_token:(fun _token acc -> acc)
    tree
  |> List.rev

(* Find nodes by kind *)
let find_by_kind kind tree =
  find_nodes
    (fun node ->
      let open Syn.Ceibo.Red in
      SyntaxNode.kind node = kind)
    tree

(* Find nodes by multiple kinds *)
let find_by_kinds kinds tree =
  find_nodes
    (fun node ->
      let open Syn.Ceibo.Red in
      List.mem (SyntaxNode.kind node) kinds)
    tree

(* Find tokens matching predicate *)
let find_tokens predicate tree =
  traverse
    ~visit_node:(fun _node acc -> acc)
    ~visit_token:(fun token acc -> if predicate token then token :: acc else acc)
    tree
  |> List.rev

(* First non-trivia child *)
let first_non_trivia_child node =
  let open Syn.Ceibo.Red in
  let children = SyntaxNode.children node in
  let rec find i =
    if i >= Array.length children then None
    else
      match children.(i) with
      | Token t when is_trivia (SyntaxToken.kind t) -> find (i + 1)
      | elem -> Some elem
  in
  find 0

(* First non-trivia token *)
let first_non_trivia_token node =
  match first_non_trivia_child node with
  | Some (Syn.Ceibo.Red.Token t) -> Some t
  | _ -> None

(* Visitor pattern *)
type 'acc visitor = {
  visit_node : red_node -> 'acc -> 'acc;
  visit_token : red_token -> 'acc -> 'acc;
}

let fold visitor init tree =
  let open Syn.Ceibo.Red in
  let rec go elem acc =
    match elem with
    | Node n ->
        let acc = visitor.visit_node n acc in
        let children = SyntaxNode.children n in
        let result = ref acc in
        for i = 0 to Array.length children - 1 do
          result := go children.(i) !result
        done;
        !result
    | Token t -> visitor.visit_token t acc
  in
  go (Node tree) init
