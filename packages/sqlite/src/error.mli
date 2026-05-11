type t =
  | ConnectionClosed
  | RandomFailure of string
  | OpenFailed of Sqlite__Native.error
  | ConfigurationFailed of {
      statement: string;
      cause: Sqlite__Native.error;
    }
  | PrepareFailed of {
      sql: string;
      cause: Sqlite__Native.error;
    }
  | BindFailed of {
      index: int;
      cause: Sqlite__Native.error;
    }
  | ParameterCountMismatch of { expected: int; actual: int }
  | ExecutionFailed of {
      sql: string;
      cause: Sqlite__Native.error;
    }
  | ResetFailed of Sqlite__Native.error
  | FinalizeFailed of Sqlite__Native.error
  | TransactionAlreadyInProgress
  | NoTransactionInProgress
  | UnsupportedOperation of string

val to_string: t -> string

val serializer: t Serde.Ser.t
