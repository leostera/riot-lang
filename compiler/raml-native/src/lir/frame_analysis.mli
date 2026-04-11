type result = {
  contains_calls: bool;
  virtual_names: string list;
}
val analyze_procedure: Types.Procedure.t -> result
