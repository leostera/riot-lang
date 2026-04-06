let solve total feedback_ref =
  let () = Period_cell.set_ref feedback_ref total in
  let solved = Eval.one (Eval.PeriodCell total) in
  println "Solving x = 100 + 0.1x with the fixed-point evaluator";
  println (String.concat "" [ "  x = "; Float.to_string ~precision:6 solved ])
