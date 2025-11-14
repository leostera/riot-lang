open Std
open Collections

type t = (string, Value.t) HashMap.t

let empty () = HashMap.create ()

let singleton ~var ~value =
  let sub = empty () in
  HashMap.insert sub var value |> ignore;
  sub

let of_list pairs =
  let sub = empty () in
  List.iter (fun (var, value) ->
    HashMap.insert sub var value |> ignore
  ) pairs;
  sub

let bind sub ~var ~value =
  (* Create a new substitution with all existing bindings *)
  let new_sub = empty () in
  HashMap.iter (fun k v ->
    HashMap.insert new_sub k v |> ignore
  ) sub;
  (* Add the new binding *)
  HashMap.insert new_sub var value |> ignore;
  new_sub

let lookup sub ~var =
  HashMap.get sub var

let mem sub ~var =
  Option.is_some (HashMap.get sub var)

let unbind sub ~var =
  HashMap.remove sub var |> ignore;
  sub

let merge sub1 sub2 =
  let result = empty () in
  let conflict = ref false in
  
  (* Add all bindings from sub1 *)
  HashMap.iter (fun var value ->
    HashMap.insert result var value |> ignore
  ) sub1;
  
  (* Try to add bindings from sub2 *)
  HashMap.iter (fun var value ->
    match HashMap.get result var with
    | None -> HashMap.insert result var value |> ignore
    | Some existing_value ->
        if not (Value.equal existing_value value) then
          conflict := true
  ) sub2;
  
  if !conflict then None else Some result

let extend sub pairs =
  let sub2 = of_list pairs in
  merge sub sub2

let apply_to_term sub term =
  match term with
  | Term.Var x ->
      (match HashMap.get sub x with
      | Some value -> Term.Const value
      | None -> Term.Var x)
  | Term.Const _ -> term
  | Term.Wildcard -> term

let apply_to_atom sub atom =
  let new_args = List.map (apply_to_term sub) atom.Ast.args in
  { atom with Ast.args = new_args }

let apply_to_tuple sub terms =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | term :: rest ->
        match apply_to_term sub term with
        | Term.Const value -> go (value :: acc) rest
        | Term.Var _ -> None  (* Still has variables *)
        | Term.Wildcard -> None  (* Can't convert wildcard to value *)
  in
  go [] terms

let bindings sub =
  let pairs = ref [] in
  HashMap.iter (fun var value ->
    pairs := (var, value) :: !pairs
  ) sub;
  !pairs

let vars sub =
  let vs = ref [] in
  HashMap.iter (fun var _value ->
    vs := var :: !vs
  ) sub;
  !vs

let is_empty sub =
  HashMap.len sub = 0

let size sub =
  HashMap.len sub

let to_string sub =
  if is_empty sub then "{}"
  else
    let pairs = bindings sub in
    let pair_strs = List.map (fun (var, value) ->
      var ^ "→" ^ Value.to_string value
    ) pairs in
    "{" ^ String.concat ", " pair_strs ^ "}"

let equal sub1 sub2 =
  let size1 = size sub1 in
  let size2 = size sub2 in
  if size1 = size2 then begin
    let all_match = ref true in
    HashMap.iter (fun var value1 ->
      match HashMap.get sub2 var with
      | None -> all_match := false
      | Some value2 -> 
          if not (Value.equal value1 value2) then
            all_match := false
    ) sub1;
    !all_match
  end else false
