type result = {
  contains_calls: bool;
  frame_required: bool;
  slot_names: string list;
}
val analyze_procedure: Types.Procedure.t -> result
