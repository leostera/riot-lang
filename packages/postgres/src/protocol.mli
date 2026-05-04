open Std

type message_type =
  | Startup
  | Query
  | Terminate
  | PasswordMessage
  | Parse
  | Bind
  | Execute
  | Describe
  | Close
  | Sync

module Sqlstate: sig
  type t =
    (* Class 00 — Successful Completion *)
    | SuccessfulCompletion
    (* Class 01 — Warning *)
    | Warning
    | DynamicResultSetsReturned
    | ImplicitZeroBitPadding
    | NullValueEliminatedInSetFunction
    | PrivilegeNotGranted
    | PrivilegeNotRevoked
    | StringDataRightTruncationWarning
    | DeprecatedFeature
    (* Class 02 — No Data *)
    | NoData
    | NoAdditionalDynamicResultSetsReturned
    (* Class 08 — Connection Exception *)
    | ConnectionException
    | ConnectionDoesNotExist
    | ConnectionFailure
    | SqlclientUnableToEstablishSqlconnection
    | SqlserverRejectedEstablishmentOfSqlconnection
    | TransactionResolutionUnknown
    | ProtocolViolation
    (* Class 23 — Integrity Constraint Violation *)
    | IntegrityConstraintViolation
    | RestrictViolation
    | NotNullViolation
    | ForeignKeyViolation
    | UniqueViolation
    | CheckViolation
    | ExclusionViolation
    (* Class 42 — Syntax Error or Access Rule Violation *)
    | SyntaxErrorOrAccessRuleViolation
    | SyntaxError
    | InsufficientPrivilege
    | CannotCoerce
    | GroupingError
    | WindowingError
    | InvalidRecursion
    | InvalidForeignKey
    | InvalidName
    | NameTooLong
    | ReservedName
    | DatatypeMismatch
    | IndeterminateDatatype
    | CollationMismatch
    | IndeterminateCollation
    | WrongObjectType
    | UndefinedColumn
    | UndefinedFunction
    | UndefinedTable
    | UndefinedParameter
    | UndefinedObject
    | DuplicateColumn
    | DuplicateCursor
    | DuplicateDatabase
    | DuplicateFunction
    | DuplicatePreparedStatement
    | DuplicateSchema
    | DuplicateTable
    | DuplicateAlias
    | DuplicateObject
    | AmbiguousColumn
    | AmbiguousFunction
    | AmbiguousParameter
    | AmbiguousAlias
    | InvalidColumnReference
    | InvalidColumnDefinition
    | InvalidCursorDefinition
    | InvalidDatabaseDefinition
    | InvalidFunctionDefinition
    | InvalidPreparedStatementDefinition
    | InvalidSchemaDefinition
    | InvalidTableDefinition
    | InvalidObjectDefinition
    (* Class 53 — Insufficient Resources *)
    | InsufficientResources
    | DiskFull
    | OutOfMemory
    | TooManyConnections
    | ConfigurationLimitExceeded
    (* Class 54 — Program Limit Exceeded *)
    | ProgramLimitExceeded
    | StatementTooComplex
    | TooManyColumns
    | TooManyArguments
    (* Class 55 — Object Not In Prerequisite State *)
    | ObjectNotInPrerequisiteState
    | ObjectInUse
    | CantChangeRuntimeParam
    | LockNotAvailable
    (* Class 57 — Operator Intervention *)
    | OperatorIntervention
    | QueryCanceled
    | AdminShutdown
    | CrashShutdown
    | CannotConnectNow
    | DatabaseDropped
    (* Class 58 — System Error *)
    | SystemError
    | IoError
    | UndefinedFile
    | DuplicateFile
    (* Class P0 — PL/pgSQL Error *)
    | PlpgsqlError
    | RaiseException
    | NoDataFound
    | TooManyRows
    (* Class XX — Internal Error *)
    | InternalError
    | DataCorrupted
    | IndexCorrupted
    (* Other/Unknown *)
    | UnknownSqlstate of string

  val from_string: string -> t

  val to_string: t -> string
end

module Error: sig
  type t = {
    severity: string option;
    sqlstate: Sqlstate.t option;
    message: string;
    detail: string option;
    hint: string option;
    position: int option;
    internal_position: int option;
    internal_query: string option;
    where_context: string option;
    schema_name: string option;
    table_name: string option;
    column_name: string option;
    datatype_name: string option;
    constraint_name: string option;
    source_file: string option;
    source_line: int option;
    source_routine: string option;
  }

  val severity: t -> string option

  val sqlstate: t -> Sqlstate.t option

  val message: t -> string

  val detail: t -> string option

  val hint: t -> string option

  val position: t -> int option

  val internal_position: t -> int option

  val internal_query: t -> string option

  val where_context: t -> string option

  val schema_name: t -> string option

  val table_name: t -> string option

  val column_name: t -> string option

  val datatype_name: t -> string option

  val constraint_name: t -> string option

  val source_file: t -> string option

  val source_line: t -> int option

  val source_routine: t -> string option

  val to_string: t -> string

  val to_json: t -> Data.Json.t

  val from_json: Data.Json.t -> t
end

module TypeOid: sig
  type t =
    | Bool
    | Bytea
    | Char
    | Int8
    | Int2
    | Int4
    | Text
    | Oid
    | Json
    | Float4
    | Float8
    | Varchar
    | Date
    | Time
    | Timestamp
    | Timestamptz
    | Interval
    | Numeric
    | Uuid
    | Jsonb
    | Unknown of int

  val from_int: int -> t

  val to_int: t -> int

  val to_string: t -> string
end

module Oid: sig
  type t = int

  val from_int: int -> t

  val to_int: t -> int

  val zero: t

  val to_string: t -> string
end

module ColumnAttr: sig
  type t =
    | NotFromTable
    | Position of int

  val from_int: int -> t

  val to_int: t -> int

  val to_string: t -> string
end

module TypeSize: sig
  type t =
    | VariableLength
    | NullTerminated
    | Fixed of int

  val from_int: int -> t

  val to_int: t -> int

  val to_string: t -> string
end

module TypeModifier: sig
  type t =
    | NoModifier
    | Modifier of int

  val from_int: int -> t

  val to_int: t -> int

  val to_string: t -> string
end

module Format: sig
  type t =
    | Text
    | Binary

  val from_int: int -> t

  val to_int: t -> int

  val to_string: t -> string
end

module Row: sig
  type field = {
    name: string;
    table_oid: Oid.t;
    column_attr: ColumnAttr.t;
    type_oid: TypeOid.t;
    type_size: TypeSize.t;
    type_modifier: TypeModifier.t;
    format: Format.t;
  }
  type description = field list
  type value =
    | Null
    | Value of string
  type data = value list
end

type backend_message =
  | AuthenticationOk
  | AuthenticationCleartextPassword
  | AuthenticationMD5Password of bytes
  | AuthenticationSASL of string list
  | AuthenticationSASLContinue of string
  | AuthenticationSASLFinal of string
  | BackendKeyData of { process_id: int; secret_key: int }
  | ParameterStatus of { name: string; value: string }
  | ReadyForQuery of char
  | RowDescription of Row.description
  | DataRow of Row.data
  | CommandComplete of string
  | ErrorResponse of Error.t
  | NoticeResponse of Error.t
  | ParseComplete
  | BindComplete
  | CloseComplete
  | NoData
  | ParameterDescription of TypeOid.t list
  | EmptyQueryResponse

module Writer: sig
  val startup_message: user:string -> database:string -> application_name:string option -> string

  val password_message: string -> string

  val sasl_initial_response: mechanism:string -> response:string -> string

  val sasl_response: string -> string

  val query_message: string -> string

  val parse_message: statement_name:string -> query:string -> param_types:int list -> string

  (**
     Build a PostgreSQL extended-query Bind message.

     [None] encodes a SQL NULL parameter with a [-1] value length. [Some ""]
     encodes an empty but present value with a [0] value length.
  *)
  val bind_message:
    portal_name:string ->
    statement_name:string ->
    params:string option list ->
    string

  val execute_message: portal_name:string -> max_rows:int -> string

  val describe_message: what:char -> name:string -> string

  val sync_message: unit -> string

  val close_message: what:char -> name:string -> string

  val terminate_message: unit -> string
end

module Reader: sig
  (** Structured backend frame parsing failure. *)
  type parse_error = {
    (** Backend message tag byte. *)
    message_type: int;
    (** Declared PostgreSQL message length, including the 4-byte length field. *)
    length: int;
    (** Byte offset within the backend message body where parsing failed. *)
    offset: int;
    (** Human-readable parse failure. *)
    message: string;
  }

  (** Render a backend parse error for logs and driver diagnostics. *)
  val parse_error_to_string: parse_error -> string

  (**
     Parse a backend message without raising.

     Use this in networked code so malformed or truncated server frames become
     driver errors instead of process crashes.
  *)
  val parse_backend_message_result: int -> int -> bytes -> (backend_message, parse_error) result

  (**
     Compatibility wrapper around [parse_backend_message_result].

     This raises on malformed input and should only be used in tests or code
     that intentionally wants fail-fast behavior.
  *)
  val parse_backend_message: int -> int -> bytes -> backend_message
end
