(* Library interface module utilities *)

let template ~parent ~modules ~stdlib_modules =
  let parent_str = Module_name.to_string parent in
  let seen = Hashtbl.create 16 in
  modules
  |> List.filter (fun child ->
      let child_str = Module_name.to_string child in
      if Hashtbl.mem seen child_str || List.mem child stdlib_modules then false
      else (
        Hashtbl.add seen child_str true;
        true))
  |> List.map (fun child ->
      let child_str = Module_name.to_string child in
      Printf.sprintf "module %s = %s__%s" child_str parent_str child_str)
