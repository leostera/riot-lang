open Std
module Array = Collections.Array
module HashMap = Collections.HashMap
module HashSet = Collections.HashSet
module Lir = Types

type live_set = string HashSet.t

type point = {
  instruction: Lir.Instruction.t;
  live_before: live_set;
  live_after: live_set;
}

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

let copy = fun live -> HashSet.from_list (HashSet.to_list live)

let add = fun live name ->
  let next = copy live in
  let _ = HashSet.insert next ~value:name in
  next

let remove = fun live name ->
  let next = copy live in
  let _ = HashSet.remove next ~value:name in
  next

let union = HashSet.union

let difference = HashSet.difference

let equal = fun left right ->
  Int.equal (HashSet.length left) (HashSet.length right)
  && List.for_all (fun value -> HashSet.contains right ~value) (HashSet.to_list left)

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
      List.fold_left arguments ~init:live ~fn:add_operand_uses
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
  for index = 0 to Array.length instructions - 1 do
    match Array.get_unchecked instructions ~at:index with
    | Lir.Instruction.Label name ->
        let _ = HashMap.insert labels ~key:name ~value:index in
        ()
    | _ -> ()
  done;
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
      match HashMap.get label_indices ~key:target with
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
        match HashMap.get label_indices ~key:target with
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
  match HashMap.get bounds_map ~key:name with
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
      let _ = HashMap.insert bounds_map ~key:name ~value:{ start; finish } in
      ()
  | None ->
      let _ = HashMap.insert bounds_map ~key:name ~value:{ start = position; finish = position } in
      ()

type analysis = {
  instructions: Lir.Instruction.t array;
  live_before: live_set array;
  live_after: live_set array;
}

let analyze_procedure = fun (procedure: Lir.Procedure.t) ->
  let instructions = Array.from_list procedure.body in
  let instruction_count = Array.length instructions in
  let label_indices = label_index_map instructions in
  let live_before =
    Array.init ~count:instruction_count ~fn:(fun _ -> empty ())
  in
  let live_after =
    Array.init ~count:instruction_count ~fn:(fun _ -> empty ())
  in
  let changed = ref true in
  while !changed do
    changed := false;
    for index = instruction_count - 1 downto 0 do
      let instruction = Array.get_unchecked instructions ~at:index in
      let next_live_after = successors_of_instruction ~label_indices instructions index instruction
      |> List.fold_left
        ~init:(empty ())
        ~fn:(fun live successor -> union live (Array.get_unchecked live_before ~at:successor)) in
      let next_live_before = union
        (uses_of_instruction instruction)
        (difference next_live_after (defs_of_instruction instruction)) in
      if not (equal (Array.get_unchecked live_after ~at:index) next_live_after) then
        (
          Array.set_unchecked live_after ~at:index ~value:next_live_after;
          changed := true
        );
      if not (equal (Array.get_unchecked live_before ~at:index) next_live_before) then
        (
          Array.set_unchecked live_before ~at:index ~value:next_live_before;
          changed := true
        )
    done
  done;
  { instructions; live_before; live_after }

let points_of_procedure = fun procedure ->
  let analysis = analyze_procedure procedure in
  let points = ref [] in
  for index = Array.length analysis.instructions - 1 downto 0 do
    let instruction = Array.get_unchecked analysis.instructions ~at:index in
    let live_before = Array.get_unchecked analysis.live_before ~at:index in
    let live_after = Array.get_unchecked analysis.live_after ~at:index in
    points := { instruction; live_before; live_after } :: !points
  done;
  !points

let intervals_of_procedure = fun procedure ->
  let analysis = analyze_procedure procedure in
  let bounds = HashMap.create () in
  let live_across_calls = HashSet.create () in
  for index = 0 to Array.length analysis.instructions - 1 do
    let instruction = Array.get_unchecked analysis.instructions ~at:index in
    let live_before = Array.get_unchecked analysis.live_before ~at:index in
    let live_after = Array.get_unchecked analysis.live_after ~at:index in
    let mentioned = union
      (union live_before live_after)
      (union (uses_of_instruction instruction) (defs_of_instruction instruction)) in
    HashSet.for_each mentioned ~fn:(fun name -> update_bounds bounds name index);
    HashSet.for_each (live_across_call_names instruction live_after)
      ~fn:(fun name ->
        let _ = HashSet.insert live_across_calls ~value:name in
        ())
  done;
  HashMap.keys bounds
  |> List.sort ~compare:String.compare
  |> List.filter_map
    ~fn:(fun name ->
      HashMap.get bounds ~key:name
      |> Option.map
        ~fn:(fun bounds ->
          {
            name;
            start = bounds.start;
            finish = bounds.finish;
            live_across_call = HashSet.contains live_across_calls ~value:name
          }))
