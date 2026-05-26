let pp_err value =
  match value with
  | #Rio.io_error as err -> err
  | other -> other
