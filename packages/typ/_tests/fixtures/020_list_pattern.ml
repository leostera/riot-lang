let expand_hex = function
  | ["#"; r1; r2; g1; g2; b1; b2] ->
      r1 ^ r2 ^ g1 ^ g2 ^ b1 ^ b2
  | ["#"; r1; g1; b1] ->
      r1 ^ g1 ^ b1
  | _ ->
      ""
