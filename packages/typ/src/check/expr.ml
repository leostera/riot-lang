open Std

let check = Core.check_expression

let check_parameter = fun parameter ->
  match parameter with
  | Syn.Cst.Parameter.Positional positional ->
      Core.check_pattern positional.pattern
  | Syn.Cst.Parameter.Labeled labeled ->
      (match labeled.binding_pattern with
       | Some pattern -> Core.check_pattern pattern
       | None -> [])
  | Syn.Cst.Parameter.Optional optional ->
      List.append
        (match optional.binding_pattern with
         | Some pattern -> Core.check_pattern pattern
         | None -> [])
        (match optional.default_value with
         | Some expression -> Core.check_expression expression
         | None -> [])
  | Syn.Cst.Parameter.LocallyAbstract _ -> []

let check_let_binding = Core.check_let_binding
