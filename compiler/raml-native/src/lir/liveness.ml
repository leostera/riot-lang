open Std
module Array = Collections.Array
module HashMap = Collections.HashMap
module HashSet = Collections.HashSet
module Lir = Types

type live_set = string HashSet.t

type interval = {
  name: string;
  start: int;
  finish: int;
  live_across_call: bool;
}

type bounds = {
  start: int;
  finish: int;
}

let empty = HashSet.create

let copy = fun live -> HashSet.of_list (HashSet.to_list live)

let add = fun live name ->
  let next = copy live in
  let _ = HashSet.insert next name in
  next

let remove = fun live name ->
  let next = copy live in
  let _ = HashSet.remove next name in
  next

let union = HashSet.union

let difference = HashSet.difference

let equal = fun left right ->
  Int.equal (HashSet.len left) (HashSet.len right)
  && List.for_all (HashSet.contains right) (HashSet.to_list left)

let add_operand_uses = fun live operand ->
  match operand with
  | Lir.Operand.Register name -> add live name
  | Lir.Operand.Home _
  | Lir.Operand.Global _
  | Lir.Operand.Symbol_address _
  | Lir.Operand.Literal _ -> live

let add_destination_defs = fun live destination ->
  match destination with
  | Lir.Destination.Register name -> add live name
  | Lir.Destination.Home _ -> live

let uses_of_instruction = fun instruction ->
  match instruction with
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Jump _ ->
      empty ()
  | Lir.Instruction.Move { src; _ } ->
      add_operand_uses (empty ()) src
  | Lir.Instruction.Store_global { src; _ } ->
      add_operand_uses (empty ()) src
  | Lir.Instruction.Call { callee; arguments; _ } ->
      let live =
        match callee with
        | Lir.Callee.Direct _ -> empty ()
        | Lir.Callee.Indirect operand -> add_operand_uses (empty ()) operand
      in
      List.fold_left add_operand_uses live arguments
  | Lir.Instruction.Branch_if_zero { operand; _ } ->
      add_operand_uses (empty ()) operand
  | Lir.Instruction.Return operand -> (
      match operand with
      | Some operand -> add_operand_uses (empty ()) operand
      | None -> empty ()
    )

let defs_of_instruction = fun instruction ->
  match instruction with
  | Lir.Instruction.Move { dst; _ } ->
      add_destination_defs (empty ()) dst
  | Lir.Instruction.Call { dst; _ } -> (
      match dst with
      | Some dst -> add_destination_defs (empty ()) dst
      | None -> empty ()
    )
  | Lir.Instruction.Label _
  | Lir.Instruction.Comment _
  | Lir.Instruction.Store_global _
  | Lir.Instruction.Branch_if_zero _
  | Lir.Instruction.Jump _
  | Lir.Instruction.Return _ ->
      empty ()

let label_index_map = fun instructions ->
  let labels = HashMap.create () in
  Array.iteri
    (fun index instruction ->
      match instruction with
      | Lir.Instruction.Label name ->
          let _ = HashMap.insert labels name index in
          ()
      | _ -> ())
    instructions;
  labels

let successors_of_instruction = fun ~label_indices instructions index instruction ->
  let next =
    if index + 1 < Array.length instructions then
      Some (index + 1)
    else
      None
  in
  match instruction with
  | Lir.Instruction.Jump target -> (
      match HashMap.get label_indices target with
      | Some target_index -> [ target_index ]
      | None -> []
    )
  | Lir.Instruction.Branch_if_zero { target; _ } ->
      let fallthrough =
        match next with
        | Some next_index -> [ next_index ]
        | None -> []
      in
      (
        match HashMap.get label_indices target with
        | Some target_index -> target_index :: fallthrough
        | None -> fallthrough
      )
  | Lir.Instruction.Return _ ->
      []
  | _ -> (
      match next with
      | Some next_index -> [ next_index ]
      | None -> []
    )

let live_across_call_names = fun instruction live_after ->
  match instruction with
  | Lir.Instruction.Call { dst; _ } -> (
      match dst with
      | Some (Lir.Destination.Register name) -> remove live_after name
      | Some (Lir.Destination.Home _) -> live_after
      | None -> live_after
    )
  | _ -> empty ()

let update_bounds = fun bounds_map name position ->
  match HashMap.get bounds_map name with
  | Some bounds ->
      let start =
        if position < bounds.start then
          position
        else
          bounds.start
      in
      let finish =
        if position > bounds.finish then
          position
        else
          bounds.finish
      in
      let _ = HashMap.insert bounds_map name { start; finish } in
      ()
  | None ->
      let _ = HashMap.insert bounds_map name { start = position; finish = position } in
      ()

let intervals_of_procedure = fun (procedure: Lir.Procedure.t) ->
  let instructions = Array.of_list procedure.body in
  let instruction_count = Array.length instructions in
  let label_indices = label_index_map instructions in
  let live_before =
    Array.init instruction_count (fun _ -> empty ())
  in
  let live_after =
    Array.init instruction_count (fun _ -> empty ())
  in
  let changed = ref true in
  while !changed do
    changed := false;
    for index = instruction_count - 1 downto 0 do
      let instruction = instructions.(index) in
      let next_live_after = successors_of_instruction ~label_indices instructions index instruction
      |> List.fold_left (fun live successor -> union live live_before.(successor)) (empty ()) in
      let next_live_before = union
        (uses_of_instruction instruction)
        (difference next_live_after (defs_of_instruction instruction)) in
      if not (equal live_after.(index) next_live_after) then
        (
          live_after.(index) <- next_live_after;
          changed := true
        );
      if not (equal live_before.(index) next_live_before) then
        (
          live_before.(index) <- next_live_before;
          changed := true
        )
    done
  done;
  let bounds = HashMap.create () in
  let live_across_calls = HashSet.create () in
  Array.iteri
    (fun index instruction ->
      let mentioned = union
        (union live_before.(index) live_after.(index))
        (union (uses_of_instruction instruction) (defs_of_instruction instruction)) in
      HashSet.iter mentioned ~fn:(fun name -> update_bounds bounds name index);
      HashSet.iter (live_across_call_names instruction live_after.(index))
        ~fn:(fun name ->
          let _ = HashSet.insert live_across_calls name in
          ()))
    instructions;
  HashMap.keys bounds
  |> List.sort String.compare
  |> List.filter_map
    (fun name ->
      HashMap.get bounds name
      |> Option.map
        (fun bounds ->
          {
            name;
            start = bounds.start;
            finish = bounds.finish;
            live_across_call = HashSet.contains live_across_calls name
          }))
