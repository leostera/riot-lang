let rec process = function
  | [] -> []
  | "" :: "" :: rest -> process rest
  | x :: y :: rest -> (x ^ y) :: process rest
