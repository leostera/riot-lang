module BitMask = struct
  let lowest_set_bit_index mask =
    if mask = 0 then
      None
    else
      Some mask

  (* Remove the lowest set bit from mask *)
  [@inline always]
  let remove_lowest_bit mask =
    mask land (mask - 1)
end
