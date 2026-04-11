open Std
module HashSet = Collections.HashSet
module Program = Types.Program
module Procedure = Types.Procedure
module Instruction = Types.Instruction

let resolve_label = fun aliases label ->
  let rec loop label =
    match List.assoc_opt label aliases with
    | Some next when not (String.equal next label) -> loop next
    | Some next -> next
    | None -> label
  in
  loop label

let collapse_adjacent_labels = fun instructions ->
  let rec loop current_label aliases acc instructions =
    match instructions with
    | [] ->
        (List.rev acc, aliases)
    | Instruction.Label name :: rest -> (
        match current_label with
        | Some canonical -> loop (Some canonical) ((name, canonical) :: aliases) acc rest
        | None -> loop (Some name) aliases (Instruction.Label name :: acc) rest
      )
    | instruction :: rest ->
        loop None aliases (instruction :: acc) rest
  in
  loop None [] [] instructions

let rewrite_targets = fun aliases instructions ->
  let rewrite instruction =
    match instruction with
    | Instruction.Branch_if_zero branch -> Instruction.Branch_if_zero {
      branch
      with target = resolve_label aliases branch.target
    }
    | Instruction.Jump target -> Instruction.Jump (resolve_label aliases target)
    | instruction -> instruction
  in
  List.map rewrite instructions

let remove_unreachable = fun instructions ->
  let rec loop reachable acc instructions =
    match instructions with
    | [] -> List.rev acc
    | Instruction.Label _ as instruction :: rest -> loop true (instruction :: acc) rest
    | instruction :: rest ->
        if not reachable then
          loop false acc rest
        else
          match instruction with
          | Instruction.Jump _
          | Instruction.Return _ -> loop false (instruction :: acc) rest
          | _ -> loop true (instruction :: acc) rest
  in
  loop true [] instructions

let remove_fallthrough_jumps = fun instructions ->
  let rec loop acc instructions =
    match instructions with
    | Instruction.Jump target :: Instruction.Label next :: rest when String.equal target next -> loop
      (Instruction.Label next :: acc)
      rest
    | instruction :: rest -> loop (instruction :: acc) rest
    | [] -> List.rev acc
  in
  loop [] instructions

let used_labels = fun instructions ->
  List.fold_left
    (fun used instruction ->
      match instruction with
      | Instruction.Branch_if_zero branch ->
          let _ = HashSet.insert used branch.target in
          used
      | Instruction.Jump target ->
          let _ = HashSet.insert used target in
          used
      | _ ->
          used)
    (HashSet.create ())
    instructions

let remove_unused_labels = fun instructions ->
  let used = used_labels instructions in
  let rec loop is_first_label acc instructions =
    match instructions with
    | [] -> List.rev acc
    | Instruction.Label name as instruction :: rest ->
        if is_first_label || HashSet.contains used name then
          loop false (instruction :: acc) rest
        else
          loop false acc rest
    | instruction :: rest -> loop false (instruction :: acc) rest
  in
  loop true [] instructions

let schedule_body = fun instructions ->
  let instructions, aliases = collapse_adjacent_labels instructions in
  let instructions = rewrite_targets aliases instructions in
  let instructions = remove_unreachable instructions in
  let instructions, aliases = collapse_adjacent_labels instructions in
  let instructions = rewrite_targets aliases instructions in
  let instructions = remove_fallthrough_jumps instructions in
  remove_unused_labels instructions

let schedule_procedure = fun (procedure: Procedure.t) ->
  { procedure with body = schedule_body procedure.body }

let program = fun (program: Program.t) ->
  { program with procedures = List.map schedule_procedure program.procedures }
