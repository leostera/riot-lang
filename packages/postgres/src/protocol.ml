open Std
open Std.IO

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

module Sqlstate = struct
  (* PostgreSQL SQLSTATE codes - https://www.postgresql.org/docs/current/errcodes-appendix.html *)

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

  (* Other/Unknown *)

  (* Other/Unknown *)

  (* Other/Unknown *)

  (* Other/Unknown *)

  (* Other/Unknown *)

  (* Parse SQLSTATE string into typed variant *)

  let from_string = fun code ->
    match code with
    | "00000" -> SuccessfulCompletion
    | "01000" -> Warning
    | "0100C" -> DynamicResultSetsReturned
    | "01008" -> ImplicitZeroBitPadding
    | "01003" -> NullValueEliminatedInSetFunction
    | "01007" -> PrivilegeNotGranted
    | "01006" -> PrivilegeNotRevoked
    | "01004" -> StringDataRightTruncationWarning
    | "01P01" -> DeprecatedFeature
    | "02000" -> NoData
    | "02001" -> NoAdditionalDynamicResultSetsReturned
    | "08000" -> ConnectionException
    | "08003" -> ConnectionDoesNotExist
    | "08006" -> ConnectionFailure
    | "08001" -> SqlclientUnableToEstablishSqlconnection
    | "08004" -> SqlserverRejectedEstablishmentOfSqlconnection
    | "08007" -> TransactionResolutionUnknown
    | "08P01" -> ProtocolViolation
    | "23000" -> IntegrityConstraintViolation
    | "23001" -> RestrictViolation
    | "23502" -> NotNullViolation
    | "23503" -> ForeignKeyViolation
    | "23505" -> UniqueViolation
    | "23514" -> CheckViolation
    | "23P01" -> ExclusionViolation
    | "42000" -> SyntaxErrorOrAccessRuleViolation
    | "42601" -> SyntaxError
    | "42501" -> InsufficientPrivilege
    | "42846" -> CannotCoerce
    | "42803" -> GroupingError
    | "42P20" -> WindowingError
    | "42P19" -> InvalidRecursion
    | "42830" -> InvalidForeignKey
    | "42602" -> InvalidName
    | "42622" -> NameTooLong
    | "42939" -> ReservedName
    | "42804" -> DatatypeMismatch
    | "42P18" -> IndeterminateDatatype
    | "42P21" -> CollationMismatch
    | "42P22" -> IndeterminateCollation
    | "42809" -> WrongObjectType
    | "42703" -> UndefinedColumn
    | "42883" -> UndefinedFunction
    | "42P01" -> UndefinedTable
    | "42P02" -> UndefinedParameter
    | "42704" -> UndefinedObject
    | "42701" -> DuplicateColumn
    | "42P03" -> DuplicateCursor
    | "42P04" -> DuplicateDatabase
    | "42723" -> DuplicateFunction
    | "42P05" -> DuplicatePreparedStatement
    | "42P06" -> DuplicateSchema
    | "42P07" -> DuplicateTable
    | "42712" -> DuplicateAlias
    | "42710" -> DuplicateObject
    | "42702" -> AmbiguousColumn
    | "42725" -> AmbiguousFunction
    | "42P08" -> AmbiguousParameter
    | "42P09" -> AmbiguousAlias
    | "42P10" -> InvalidColumnReference
    | "42611" -> InvalidColumnDefinition
    | "42P11" -> InvalidCursorDefinition
    | "42P12" -> InvalidDatabaseDefinition
    | "42P13" -> InvalidFunctionDefinition
    | "42P14" -> InvalidPreparedStatementDefinition
    | "42P15" -> InvalidSchemaDefinition
    | "42P16" -> InvalidTableDefinition
    | "42P17" -> InvalidObjectDefinition
    | "53000" -> InsufficientResources
    | "53100" -> DiskFull
    | "53200" -> OutOfMemory
    | "53300" -> TooManyConnections
    | "53400" -> ConfigurationLimitExceeded
    | "54000" -> ProgramLimitExceeded
    | "54001" -> StatementTooComplex
    | "54011" -> TooManyColumns
    | "54023" -> TooManyArguments
    | "55000" -> ObjectNotInPrerequisiteState
    | "55006" -> ObjectInUse
    | "55P02" -> CantChangeRuntimeParam
    | "55P03" -> LockNotAvailable
    | "57000" -> OperatorIntervention
    | "57014" -> QueryCanceled
    | "57P01" -> AdminShutdown
    | "57P02" -> CrashShutdown
    | "57P03" -> CannotConnectNow
    | "57P04" -> DatabaseDropped
    | "58000" -> SystemError
    | "58030" -> IoError
    | "58P01" -> UndefinedFile
    | "58P02" -> DuplicateFile
    | "P0000" -> PlpgsqlError
    | "P0001" -> RaiseException
    | "P0002" -> NoDataFound
    | "P0003" -> TooManyRows
    | "XX000" -> InternalError
    | "XX001" -> DataCorrupted
    | "XX002" -> IndexCorrupted
    | code -> UnknownSqlstate code

  (* Convert sqlstate back to string for display *)

  let to_string = fun sqlstate ->
    match sqlstate with
    | SuccessfulCompletion -> "successful_completion"
    | Warning -> "warning"
    | DynamicResultSetsReturned -> "dynamic_result_sets_returned"
    | ImplicitZeroBitPadding -> "implicit_zero_bit_padding"
    | NullValueEliminatedInSetFunction -> "null_value_eliminated_in_set_function"
    | PrivilegeNotGranted -> "privilege_not_granted"
    | PrivilegeNotRevoked -> "privilege_not_revoked"
    | StringDataRightTruncationWarning -> "string_data_right_truncation"
    | DeprecatedFeature -> "deprecated_feature"
    | NoData -> "no_data"
    | NoAdditionalDynamicResultSetsReturned -> "no_additional_dynamic_result_sets_returned"
    | ConnectionException -> "connection_exception"
    | ConnectionDoesNotExist -> "connection_does_not_exist"
    | ConnectionFailure -> "connection_failure"
    | SqlclientUnableToEstablishSqlconnection -> "sqlclient_unable_to_establish_sqlconnection"
    | SqlserverRejectedEstablishmentOfSqlconnection -> "sqlserver_rejected_establishment_of_sqlconnection"
    | TransactionResolutionUnknown -> "transaction_resolution_unknown"
    | ProtocolViolation -> "protocol_violation"
    | IntegrityConstraintViolation -> "integrity_constraint_violation"
    | RestrictViolation -> "restrict_violation"
    | NotNullViolation -> "not_null_violation"
    | ForeignKeyViolation -> "foreign_key_violation"
    | UniqueViolation -> "unique_violation"
    | CheckViolation -> "check_violation"
    | ExclusionViolation -> "exclusion_violation"
    | SyntaxErrorOrAccessRuleViolation -> "syntax_error_or_access_rule_violation"
    | SyntaxError -> "syntax_error"
    | InsufficientPrivilege -> "insufficient_privilege"
    | CannotCoerce -> "cannot_coerce"
    | GroupingError -> "grouping_error"
    | WindowingError -> "windowing_error"
    | InvalidRecursion -> "invalid_recursion"
    | InvalidForeignKey -> "invalid_foreign_key"
    | InvalidName -> "invalid_name"
    | NameTooLong -> "name_too_long"
    | ReservedName -> "reserved_name"
    | DatatypeMismatch -> "datatype_mismatch"
    | IndeterminateDatatype -> "indeterminate_datatype"
    | CollationMismatch -> "collation_mismatch"
    | IndeterminateCollation -> "indeterminate_collation"
    | WrongObjectType -> "wrong_object_type"
    | UndefinedColumn -> "undefined_column"
    | UndefinedFunction -> "undefined_function"
    | UndefinedTable -> "undefined_table"
    | UndefinedParameter -> "undefined_parameter"
    | UndefinedObject -> "undefined_object"
    | DuplicateColumn -> "duplicate_column"
    | DuplicateCursor -> "duplicate_cursor"
    | DuplicateDatabase -> "duplicate_database"
    | DuplicateFunction -> "duplicate_function"
    | DuplicatePreparedStatement -> "duplicate_prepared_statement"
    | DuplicateSchema -> "duplicate_schema"
    | DuplicateTable -> "duplicate_table"
    | DuplicateAlias -> "duplicate_alias"
    | DuplicateObject -> "duplicate_object"
    | AmbiguousColumn -> "ambiguous_column"
    | AmbiguousFunction -> "ambiguous_function"
    | AmbiguousParameter -> "ambiguous_parameter"
    | AmbiguousAlias -> "ambiguous_alias"
    | InvalidColumnReference -> "invalid_column_reference"
    | InvalidColumnDefinition -> "invalid_column_definition"
    | InvalidCursorDefinition -> "invalid_cursor_definition"
    | InvalidDatabaseDefinition -> "invalid_database_definition"
    | InvalidFunctionDefinition -> "invalid_function_definition"
    | InvalidPreparedStatementDefinition -> "invalid_prepared_statement_definition"
    | InvalidSchemaDefinition -> "invalid_schema_definition"
    | InvalidTableDefinition -> "invalid_table_definition"
    | InvalidObjectDefinition -> "invalid_object_definition"
    | InsufficientResources -> "insufficient_resources"
    | DiskFull -> "disk_full"
    | OutOfMemory -> "out_of_memory"
    | TooManyConnections -> "too_many_connections"
    | ConfigurationLimitExceeded -> "configuration_limit_exceeded"
    | ProgramLimitExceeded -> "program_limit_exceeded"
    | StatementTooComplex -> "statement_too_complex"
    | TooManyColumns -> "too_many_columns"
    | TooManyArguments -> "too_many_arguments"
    | ObjectNotInPrerequisiteState -> "object_not_in_prerequisite_state"
    | ObjectInUse -> "object_in_use"
    | CantChangeRuntimeParam -> "cant_change_runtime_param"
    | LockNotAvailable -> "lock_not_available"
    | OperatorIntervention -> "operator_intervention"
    | QueryCanceled -> "query_canceled"
    | AdminShutdown -> "admin_shutdown"
    | CrashShutdown -> "crash_shutdown"
    | CannotConnectNow -> "cannot_connect_now"
    | DatabaseDropped -> "database_dropped"
    | SystemError -> "system_error"
    | IoError -> "io_error"
    | UndefinedFile -> "undefined_file"
    | DuplicateFile -> "duplicate_file"
    | PlpgsqlError -> "plpgsql_error"
    | RaiseException -> "raise_exception"
    | NoDataFound -> "no_data_found"
    | TooManyRows -> "too_many_rows"
    | InternalError -> "internal_error"
    | DataCorrupted -> "data_corrupted"
    | IndexCorrupted -> "index_corrupted"
    | UnknownSqlstate code -> "unknown_" ^ code
end

module Error = struct
  (* PostgreSQL error/notice structured type *)

  type t = {
    severity: string option;  (* 'S' - ERROR, FATAL, PANIC, WARNING, NOTICE, etc. *)
    sqlstate: Sqlstate.t option;  (* 'C' - 5-character SQLSTATE code *)
    message: string;  (* 'M' - Primary human-readable error message *)
    detail: string option;  (* 'D' - Optional detail message *)
    hint: string option;  (* 'H' - Optional hint for fixing the error *)
    position: int option;  (* 'P' - Character position in query string *)
    internal_position: int option;  (* 'p' - Internal query position *)
    internal_query: string option;  (* 'q' - Internal query text *)
    where_context: string option;  (* 'W' - Context (stack trace) *)
    schema_name: string option;  (* 's' - Schema name *)
    table_name: string option;  (* 't' - Table name *)
    column_name: string option;  (* 'c' - Column name *)
    datatype_name: string option;  (* 'd' - Data type name *)
    constraint_name: string option;  (* 'n' - Constraint name *)
    source_file: string option;  (* 'F' - Source file name *)
    source_line: int option;  (* 'L' - Source line number *)
    source_routine: string option;  (* 'R' - Source routine name *)
  }

  (* Direct field accessors *)

  let severity = fun err -> err.severity

  let sqlstate = fun err -> err.sqlstate

  let message = fun err -> err.message

  let detail = fun err -> err.detail

  let hint = fun err -> err.hint

  let position = fun err -> err.position

  let internal_position = fun err -> err.internal_position

  let internal_query = fun err -> err.internal_query

  let where_context = fun err -> err.where_context

  let schema_name = fun err -> err.schema_name

  let table_name = fun err -> err.table_name

  let column_name = fun err -> err.column_name

  let datatype_name = fun err -> err.datatype_name

  let constraint_name = fun err -> err.constraint_name

  let source_file = fun err -> err.source_file

  let source_line = fun err -> err.source_line

  let source_routine = fun err -> err.source_routine

  module De = Serde.De
  module Ser = Serde.Ser

  type field =
    | Field_severity
    | Field_sqlstate
    | Field_message
    | Field_detail
    | Field_hint
    | Field_position
    | Field_internal_position
    | Field_internal_query
    | Field_context
    | Field_schema
    | Field_table
    | Field_column
    | Field_datatype
    | Field_constraint
    | Field_source_file
    | Field_source_line
    | Field_source_routine

  type builder = {
    mutable b_severity: string option;
    mutable b_sqlstate: Sqlstate.t option;
    mutable b_message: string option;
    mutable b_detail: string option;
    mutable b_hint: string option;
    mutable b_position: int option;
    mutable b_internal_position: int option;
    mutable b_internal_query: string option;
    mutable b_where_context: string option;
    mutable b_schema_name: string option;
    mutable b_table_name: string option;
    mutable b_column_name: string option;
    mutable b_datatype_name: string option;
    mutable b_constraint_name: string option;
    mutable b_source_file: string option;
    mutable b_source_line: int option;
    mutable b_source_routine: string option;
  }

  let sqlstate_serializer = Ser.contramap Sqlstate.to_string Ser.string

  let sqlstate_deserializer =
    De.and_then De.string (fun code -> De.const (Sqlstate.from_string code))

  let fields =
    De.fields
      [
        De.field "severity" Field_severity;
        De.field "sqlstate" Field_sqlstate;
        De.field "message" Field_message;
        De.field "detail" Field_detail;
        De.field "hint" Field_hint;
        De.field "position" Field_position;
        De.field "internal_position" Field_internal_position;
        De.field "internal_query" Field_internal_query;
        De.field "context" Field_context;
        De.field "where_context" Field_context;
        De.field "schema" Field_schema;
        De.field "schema_name" Field_schema;
        De.field "table" Field_table;
        De.field "table_name" Field_table;
        De.field "column" Field_column;
        De.field "column_name" Field_column;
        De.field "datatype" Field_datatype;
        De.field "datatype_name" Field_datatype;
        De.field "constraint" Field_constraint;
        De.field "constraint_name" Field_constraint;
        De.field "source_file" Field_source_file;
        De.field "source_line" Field_source_line;
        De.field "source_routine" Field_source_routine;
      ]

  let create_builder = fun () ->
    {
      b_severity = None;
      b_sqlstate = None;
      b_message = None;
      b_detail = None;
      b_hint = None;
      b_position = None;
      b_internal_position = None;
      b_internal_query = None;
      b_where_context = None;
      b_schema_name = None;
      b_table_name = None;
      b_column_name = None;
      b_datatype_name = None;
      b_constraint_name = None;
      b_source_file = None;
      b_source_line = None;
      b_source_routine = None;
    }

  let step = fun reader builder field ->
    match field with
    | Some Field_severity -> builder.b_severity <- De.read reader (De.option De.string)
    | Some Field_sqlstate -> builder.b_sqlstate <- De.read reader (De.option sqlstate_deserializer)
    | Some Field_message -> builder.b_message <- De.read reader (De.option De.string)
    | Some Field_detail -> builder.b_detail <- De.read reader (De.option De.string)
    | Some Field_hint -> builder.b_hint <- De.read reader (De.option De.string)
    | Some Field_position -> builder.b_position <- De.read reader (De.option De.int)
    | Some Field_internal_position ->
        builder.b_internal_position <- De.read reader (De.option De.int)
    | Some Field_internal_query -> builder.b_internal_query <- De.read reader (De.option De.string)
    | Some Field_context -> builder.b_where_context <- De.read reader (De.option De.string)
    | Some Field_schema -> builder.b_schema_name <- De.read reader (De.option De.string)
    | Some Field_table -> builder.b_table_name <- De.read reader (De.option De.string)
    | Some Field_column -> builder.b_column_name <- De.read reader (De.option De.string)
    | Some Field_datatype -> builder.b_datatype_name <- De.read reader (De.option De.string)
    | Some Field_constraint -> builder.b_constraint_name <- De.read reader (De.option De.string)
    | Some Field_source_file -> builder.b_source_file <- De.read reader (De.option De.string)
    | Some Field_source_line -> builder.b_source_line <- De.read reader (De.option De.int)
    | Some Field_source_routine -> builder.b_source_routine <- De.read reader (De.option De.string)
    | None -> De.read reader De.skip_any

  let finish = fun builder ->
    {
      severity = builder.b_severity;
      sqlstate = builder.b_sqlstate;
      message =
        (
          match builder.b_message with
          | Some message -> message
          | None -> "Unknown error"
        );
      detail = builder.b_detail;
      hint = builder.b_hint;
      position = builder.b_position;
      internal_position = builder.b_internal_position;
      internal_query = builder.b_internal_query;
      where_context = builder.b_where_context;
      schema_name = builder.b_schema_name;
      table_name = builder.b_table_name;
      column_name = builder.b_column_name;
      datatype_name = builder.b_datatype_name;
      constraint_name = builder.b_constraint_name;
      source_file = builder.b_source_file;
      source_line = builder.b_source_line;
      source_routine = builder.b_source_routine;
    }

  let deserializer = De.record_mut ~fields ~create:create_builder ~step ~finish

  let serializer =
    Ser.record
      (
        Ser.fields
          [
            Ser.field "severity" (Ser.option Ser.string) (fun err -> err.severity);
            Ser.field "sqlstate" (Ser.option sqlstate_serializer) (fun err -> err.sqlstate);
            Ser.field "message" Ser.string (fun err -> err.message);
            Ser.field "detail" (Ser.option Ser.string) (fun err -> err.detail);
            Ser.field "hint" (Ser.option Ser.string) (fun err -> err.hint);
            Ser.field "position" (Ser.option Ser.int) (fun err -> err.position);
            Ser.field "internal_position" (Ser.option Ser.int) (fun err -> err.internal_position);
            Ser.field "internal_query" (Ser.option Ser.string) (fun err -> err.internal_query);
            Ser.field "context" (Ser.option Ser.string) (fun err -> err.where_context);
            Ser.field "schema" (Ser.option Ser.string) (fun err -> err.schema_name);
            Ser.field "table" (Ser.option Ser.string) (fun err -> err.table_name);
            Ser.field "column" (Ser.option Ser.string) (fun err -> err.column_name);
            Ser.field "datatype" (Ser.option Ser.string) (fun err -> err.datatype_name);
            Ser.field "constraint" (Ser.option Ser.string) (fun err -> err.constraint_name);
            Ser.field "source_file" (Ser.option Ser.string) (fun err -> err.source_file);
            Ser.field "source_line" (Ser.option Ser.int) (fun err -> err.source_line);
            Ser.field "source_routine" (Ser.option Ser.string) (fun err -> err.source_routine);
          ]
      )

  (* Format error for display *)

  let to_string = fun err ->
    let parts = [ "Postgres error: " ^ err.message ] in
    (* Add SQLSTATE with name *)
    let parts =
      match err.sqlstate with
      | Some code -> parts @ [ "SQLSTATE: " ^ Sqlstate.to_string code ]
      | None -> parts
    in
    (* Add severity *)
    let parts =
      match err.severity with
      | Some sev -> parts @ [ "Severity: " ^ sev ]
      | None -> parts
    in
    (* Add detail *)
    let parts =
      match err.detail with
      | Some d -> parts @ [ "Detail: " ^ d ]
      | None -> parts
    in
    (* Add hint *)
    let parts =
      match err.hint with
      | Some h -> parts @ [ "Hint: " ^ h ]
      | None -> parts
    in
    (* Add position *)
    let parts =
      match err.position with
      | Some p -> parts @ [ "Position: " ^ string_of_int p ]
      | None -> parts
    in
    (* Add constraint name if present *)
    let parts =
      match err.constraint_name with
      | Some n -> parts @ [ "Constraint: " ^ n ]
      | None -> parts
    in
    (* Add schema/table/column if present *)
    let parts =
      match (err.schema_name, err.table_name, err.column_name) with
      | (Some s, Some t, Some c) ->
          parts @ [ "Location: schema \"" ^ s ^ "\", table \"" ^ t ^ "\", column \"" ^ c ^ "\"" ]
      | (Some s, Some t, None) -> parts @ [ "Location: schema \"" ^ s ^ "\", table \"" ^ t ^ "\"" ]
      | (None, Some t, Some c) -> parts @ [ "Location: table \"" ^ t ^ "\", column \"" ^ c ^ "\"" ]
      | (Some s, None, Some c) -> parts @ [ "Location: schema \"" ^ s ^ "\", column \"" ^ c ^ "\"" ]
      | (Some s, None, None) -> parts @ [ "Schema: \"" ^ s ^ "\"" ]
      | (None, Some t, None) -> parts @ [ "Table: \"" ^ t ^ "\"" ]
      | (None, None, Some c) -> parts @ [ "Column: \"" ^ c ^ "\"" ]
      | (None, None, None) -> parts
    in
    (* Add where context if present *)
    let parts =
      match err.where_context with
      | Some w -> parts @ [ "Context: " ^ w ]
      | None -> parts
    in
    String.concat "\n" parts
end

module TypeOid = struct
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

  let from_int = fun oid ->
    match oid with
    | 16 -> Bool
    | 17 -> Bytea
    | 18 -> Char
    | 20 -> Int8
    | 21 -> Int2
    | 23 -> Int4
    | 25 -> Text
    | 26 -> Oid
    | 114 -> Json
    | 700 -> Float4
    | 701 -> Float8
    | 1_043 -> Varchar
    | 1_082 -> Date
    | 1_083 -> Time
    | 1_114 -> Timestamp
    | 1_184 -> Timestamptz
    | 1_186 -> Interval
    | 1_700 -> Numeric
    | 2_950 -> Uuid
    | 3_802 -> Jsonb
    | n -> Unknown n

  let to_int = fun oid ->
    match oid with
    | Bool -> 16
    | Bytea -> 17
    | Char -> 18
    | Int8 -> 20
    | Int2 -> 21
    | Int4 -> 23
    | Text -> 25
    | Oid -> 26
    | Json -> 114
    | Float4 -> 700
    | Float8 -> 701
    | Varchar -> 1_043
    | Date -> 1_082
    | Time -> 1_083
    | Timestamp -> 1_114
    | Timestamptz -> 1_184
    | Interval -> 1_186
    | Numeric -> 1_700
    | Uuid -> 2_950
    | Jsonb -> 3_802
    | Unknown n -> n

  let to_string = fun oid ->
    match oid with
    | Bool -> "bool"
    | Bytea -> "bytea"
    | Char -> "char"
    | Int8 -> "int8"
    | Int2 -> "int2"
    | Int4 -> "int4"
    | Text -> "text"
    | Oid -> "oid"
    | Json -> "json"
    | Float4 -> "float4"
    | Float8 -> "float8"
    | Varchar -> "varchar"
    | Date -> "date"
    | Time -> "time"
    | Timestamp -> "timestamp"
    | Timestamptz -> "timestamptz"
    | Interval -> "interval"
    | Numeric -> "numeric"
    | Uuid -> "uuid"
    | Jsonb -> "jsonb"
    | Unknown n -> "unknown_" ^ string_of_int n
end

module Oid = struct
  (* PostgreSQL Object ID - used for tables, types, and other database objects *)

  type t = int

  let from_int = fun n -> n

  let to_int = fun t -> t

  let zero = 0

  (* Special value: not from a table *)

  let to_string = fun t -> string_of_int t
end

module ColumnAttr = struct
  (* Column attribute number within table *)

  type t =
    | NotFromTable
    (* 0 - computed column or not from a specific table *)
    | Position of int

  (* 0 - computed column or not from a specific table *)

  (* 0 - computed column or not from a specific table *)

  (* 0 - computed column or not from a specific table *)

  (* 0 - computed column or not from a specific table *)

  (* 0 - computed column or not from a specific table *)

  (* 1..n - column position in table *)

  let from_int = fun attr ->
    match attr with
    | 0 -> NotFromTable
    | n when n > 0 -> Position n
    | n -> Position n

  (* Defensive: treat negative as position *)

  let to_int = fun attr ->
    match attr with
    | NotFromTable -> 0
    | Position n -> n

  let to_string = fun attr ->
    match attr with
    | NotFromTable -> "not_from_table"
    | Position n -> "col_" ^ string_of_int n
end

module TypeSize = struct
  (* Type size in bytes *)

  type t =
    | VariableLength
    (* -1: varchar, text, bytea, etc. *)
    | NullTerminated
    (* -2: cstring *)
    | Fixed of int

  (* -2: cstring *)

  (* -2: cstring *)

  (* -2: cstring *)

  (* -2: cstring *)

  (* -2: cstring *)

  (* >0: fixed number of bytes *)

  let from_int = fun size ->
    match size with
    | -1 -> VariableLength
    | -2 -> NullTerminated
    | n when n > 0 -> Fixed n
    | n -> Fixed n

  (* Defensive: treat other negatives as fixed *)

  let to_int = fun size ->
    match size with
    | VariableLength -> (-1)
    | NullTerminated -> (-2)
    | Fixed n -> n

  let to_string = fun size ->
    match size with
    | VariableLength -> "variable"
    | NullTerminated -> "null_terminated"
    | Fixed n -> string_of_int n ^ "_bytes"
end

module TypeModifier = struct
  (* Type-specific modifier (e.g., varchar length, numeric precision) *)

  type t =
    | NoModifier
    (* -1: no type modifier *)
    | Modifier of int

  (* -1: no type modifier *)

  (* -1: no type modifier *)

  (* -1: no type modifier *)

  (* -1: no type modifier *)

  (* -1: no type modifier *)

  (* Type-specific encoded value *)

  let from_int = fun modifier ->
    match modifier with
    | -1 -> NoModifier
    | n -> Modifier n

  let to_int = fun modifier ->
    match modifier with
    | NoModifier -> (-1)
    | Modifier n -> n

  let to_string = fun modifier ->
    match modifier with
    | NoModifier -> "no_modifier"
    | Modifier n -> "mod_" ^ string_of_int n
end

module Format = struct
  (* Data format code *)

  type t =
    | Text
    | Binary

  let from_int = fun format_code ->
    match format_code with
    | 0 -> Text
    | 1 -> Binary
    | _ -> Text

  (* Default to text for unknown values *)

  let to_int = fun format ->
    match format with
    | Text -> 0
    | Binary -> 1

  let to_string = fun format ->
    match format with
    | Text -> "text"
    | Binary -> "binary"
end

module Row = struct
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

module Writer = struct
  let write_int32 = fun buf n ->
    Buffer.add_char buf (Char.from_int_unchecked ((n lsr 24) land 0xff));
    Buffer.add_char buf (Char.from_int_unchecked ((n lsr 16) land 0xff));
    Buffer.add_char buf (Char.from_int_unchecked ((n lsr 8) land 0xff));
    Buffer.add_char buf (Char.from_int_unchecked (n land 0xff))

  let write_int16 = fun buf n ->
    Buffer.add_char buf (Char.from_int_unchecked ((n lsr 8) land 0xff));
    Buffer.add_char buf (Char.from_int_unchecked (n land 0xff))

  let write_string = fun buf s ->
    Buffer.add_string buf s;
    Buffer.add_char buf '\x00'

  let startup_message = fun ~user ~database ~application_name ->
    let buf = Buffer.create ~size:256 in
    write_int32 buf 0;
    write_int32 buf 196_608;
    write_string buf "user";
    write_string buf user;
    write_string buf "database";
    write_string buf database;
    (
      match application_name with
      | Some name ->
          write_string buf "application_name";
          write_string buf name
      | None -> ()
    );
    Buffer.add_char buf '\x00';
    let content = Buffer.contents buf in
    let len = String.length content in
    let result = Buffer.create ~size:(len + 4) in
    write_int32 result len;
    Buffer.add_string result (String.sub content ~offset:4 ~len:(len - 4));
    Buffer.contents result

  let password_message = fun password ->
    let buf = Buffer.create ~size:(String.length password + 6) in
    Buffer.add_char buf 'p';
    write_int32 buf (String.length password + 5);
    write_string buf password;
    Buffer.contents buf

  let sasl_initial_response = fun ~mechanism ~response ->
    let buf = Buffer.create ~size:(String.length mechanism + String.length response + 16) in
    Buffer.add_char buf 'p';
    let content = Buffer.create ~size:(String.length mechanism + String.length response + 8) in
    write_string content mechanism;
    write_int32 content (String.length response);
    Buffer.add_string content response;
    let content = Buffer.contents content in
    write_int32 buf (String.length content + 4);
    Buffer.add_string buf content;
    Buffer.contents buf

  let sasl_response = fun response ->
    let buf = Buffer.create ~size:(String.length response + 5) in
    Buffer.add_char buf 'p';
    write_int32 buf (String.length response + 4);
    Buffer.add_string buf response;
    Buffer.contents buf

  let query_message = fun sql ->
    let buf = Buffer.create ~size:(String.length sql + 8) in
    Buffer.add_char buf 'Q';
    write_int32 buf (String.length sql + 5);
    write_string buf sql;
    Buffer.contents buf

  let parse_message = fun ~statement_name ~query ~param_types ->
    let buf = Buffer.create ~size:256 in
    write_string buf statement_name;
    write_string buf query;
    write_int16 buf (List.length param_types);
    List.for_each param_types ~fn:(fun oid -> write_int32 buf oid);
    let content = Buffer.contents buf in
    let length = String.length content + 4 in
    let result = Buffer.create ~size:(length + 1) in
    Buffer.add_char result 'P';
    write_int32 result length;
    Buffer.add_string result content;
    Buffer.contents result

  let bind_message = fun ~portal_name ~statement_name ~params ->
    let buf = Buffer.create ~size:256 in
    write_string buf portal_name;
    write_string buf statement_name;
    write_int16 buf 0;
    write_int16 buf (List.length params);
    List.for_each
      params
      ~fn:(fun param ->
        match param with
        | None -> write_int32 buf (-1)
        | Some param ->
            write_int32 buf (String.length param);
            Buffer.add_string buf param);
    write_int16 buf 0;
    let content = Buffer.contents buf in
    let length = String.length content + 4 in
    let result = Buffer.create ~size:(length + 1) in
    Buffer.add_char result 'B';
    write_int32 result length;
    Buffer.add_string result content;
    Buffer.contents result

  let execute_message = fun ~portal_name ~max_rows ->
    let buf = Buffer.create ~size:64 in
    Buffer.add_char buf 'E';
    write_int32 buf (String.length portal_name + 1 + 4 + 4);
    write_string buf portal_name;
    write_int32 buf max_rows;
    Buffer.contents buf

  let describe_message = fun ~what ~name ->
    let buf = Buffer.create ~size:64 in
    Buffer.add_char buf 'D';
    write_int32 buf (1 + String.length name + 1 + 4);
    Buffer.add_char buf what;
    write_string buf name;
    Buffer.contents buf

  let sync_message = fun () ->
    let buf = Buffer.create ~size:5 in
    Buffer.add_char buf 'S';
    write_int32 buf 4;
    Buffer.contents buf

  let close_message = fun ~what ~name ->
    let buf = Buffer.create ~size:64 in
    Buffer.add_char buf 'C';
    write_int32 buf (1 + String.length name + 1 + 4);
    Buffer.add_char buf what;
    write_string buf name;
    Buffer.contents buf

  let terminate_message = fun () ->
    let buf = Buffer.create ~size:5 in
    Buffer.add_char buf 'X';
    write_int32 buf 4;
    Buffer.contents buf
end

module Reader = struct
  type parse_error = { message_type: int; length: int; offset: int; message: string }

  let parse_error_to_string = fun error ->
    "backend message "
    ^ String.make ~len:1 ~char:(Char.from_int_unchecked error.message_type)
    ^ " length "
    ^ Int.to_string error.length
    ^ " at offset "
    ^ Int.to_string error.offset
    ^ ": "
    ^ error.message

  let fail = fun msg_type length reader message ->
    Error {
      message_type = msg_type;
      length;
      offset = Binary_reader.position reader;
      message;
    }

  let expect = fun msg_type length reader message value ->
    match value with
    | Some value -> Ok value
    | None -> fail msg_type length reader message

  let ( let* ) = fun value fn ->
    match value with
    | Ok value -> fn value
    | Error _ as error -> error

  let read_byte = fun msg_type length reader field ->
    expect
      msg_type
      length
      reader
      ("expected " ^ field)
      (Binary_reader.read_byte reader)

  let read_int16 = fun msg_type length reader field ->
    expect
      msg_type
      length
      reader
      ("expected " ^ field)
      (Binary_reader.read_int16 reader)

  let read_int32 = fun msg_type length reader field ->
    expect
      msg_type
      length
      reader
      ("expected " ^ field)
      (Binary_reader.read_int32 reader)

  let read_bytes = fun msg_type length reader field count ->
    expect
      msg_type
      length
      reader
      ("expected " ^ field)
      (Binary_reader.read_bytes reader count)

  let read_string = fun msg_type length reader field ->
    expect
      msg_type
      length
      reader
      ("expected " ^ field)
      (Binary_reader.read_string reader)

  let finish = fun msg_type length reader message ->
    if Binary_reader.is_eof reader then
      Ok message
    else
      fail msg_type length reader "unexpected trailing bytes"

  let read_cstrings = fun msg_type length reader ->
    let rec loop acc =
      match Binary_reader.read_string reader with
      | None -> fail msg_type length reader "expected null-terminated string list"
      | Some "" -> Ok (List.rev acc)
      | Some value -> loop (value :: acc)
    in
    loop []

  let read_remaining_string = fun reader ->
    let remaining = Binary_reader.remaining reader in
    match Binary_reader.read_bytes reader remaining with
    | Some bytes -> Bytes.to_string bytes
    | None -> ""

  let parse_backend_message_result = fun msg_type length bytes ->
    let reader = Binary_reader.create bytes in
    let expected_body_length = length - 4 in
    if length < 4 then
      fail msg_type length reader "message length must include the 4-byte length field"
    else if expected_body_length != Bytes.length bytes then
      fail
        msg_type
        length
        reader
        ("message length/body mismatch: expected "
        ^ Int.to_string expected_body_length
        ^ " body bytes, got "
        ^ Int.to_string (Bytes.length bytes))
    else
      let msg_char = Char.from_int_unchecked msg_type in
      match msg_char with
      | 'R' -> (
          let* auth_type = read_int32 msg_type length reader "auth_type in Authentication message" in
          match auth_type with
          | 0 -> finish msg_type length reader AuthenticationOk
          | 3 -> finish msg_type length reader AuthenticationCleartextPassword
          | 5 ->
              let* salt = read_bytes msg_type length reader "salt in AuthenticationMD5Password" 4 in
              finish msg_type length reader (AuthenticationMD5Password salt)
          | 10 ->
              let* mechanisms = read_cstrings msg_type length reader in
              finish msg_type length reader (AuthenticationSASL mechanisms)
          | 11 ->
              finish
                msg_type
                length
                reader
                (AuthenticationSASLContinue (read_remaining_string reader))
          | 12 ->
              finish msg_type length reader (AuthenticationSASLFinal (read_remaining_string reader))
          | n -> fail msg_type length reader ("unknown authentication type: " ^ string_of_int n)
        )
      | 'K' ->
          let* process_id = read_int32 msg_type length reader "process_id in BackendKeyData" in
          let* secret_key = read_int32 msg_type length reader "secret_key in BackendKeyData" in
          finish msg_type length reader (BackendKeyData { process_id; secret_key })
      | 'S' ->
          let* name = read_string msg_type length reader "name in ParameterStatus" in
          let* value = read_string msg_type length reader "value in ParameterStatus" in
          finish msg_type length reader (ParameterStatus { name; value })
      | 'Z' ->
          let* status = read_byte msg_type length reader "status in ReadyForQuery" in
          finish msg_type length reader (ReadyForQuery (Char.from_int_unchecked status))
      | 'T' ->
          let* field_count = read_int16 msg_type length reader "field_count in RowDescription" in
          let rec read_fields n acc =
            if n = 0 then
              Ok (List.rev acc)
            else
              let index = field_count - n + 1 in
              let* name =
                read_string
                  msg_type
                  length
                  reader
                  ("field name in RowDescription field " ^ string_of_int index)
              in
              let* table_oid = read_int32 msg_type length reader ("table_oid for field " ^ name) in
              let* column_attr = read_int16 msg_type length reader ("column_attr for field " ^ name) in
              let* type_oid = read_int32 msg_type length reader ("type_oid for field " ^ name) in
              let* type_size = read_int16 msg_type length reader ("type_size for field " ^ name) in
              let* type_modifier =
                read_int32 msg_type length reader ("type_modifier for field " ^ name)
              in
              let* format = read_int16 msg_type length reader ("format for field " ^ name) in
              let field: Row.field = {
                Row.name;
                table_oid = Oid.from_int table_oid;
                column_attr = ColumnAttr.from_int column_attr;
                type_size = TypeSize.from_int type_size;
                type_oid = TypeOid.from_int type_oid;
                type_modifier = TypeModifier.from_int type_modifier;
                format = Format.from_int format;
              }
              in
              read_fields (n - 1) (field :: acc)
          in
          let* fields = read_fields field_count [] in
          finish msg_type length reader (RowDescription fields)
      | 'D' ->
          let* col_count = read_int16 msg_type length reader "column_count in DataRow" in
          let rec read_columns n acc =
            if n = 0 then
              Ok (List.rev acc)
            else
              let index = col_count - n + 1 in
              let* col_len =
                read_int32
                  msg_type
                  length
                  reader
                  ("column_length in DataRow column " ^ string_of_int index)
              in
              if col_len = (-1) then
                read_columns (n - 1) (Row.Null :: acc)
              else if col_len < (-1) then
                fail
                  msg_type
                  length
                  reader
                  ("invalid negative column length in DataRow column " ^ string_of_int index)
              else
                let* value =
                  expect
                    msg_type
                    length
                    reader
                    ("column_value in DataRow column "
                    ^ string_of_int index
                    ^ " with length "
                    ^ string_of_int col_len)
                    (Binary_reader.read_cstring reader col_len)
                in
                read_columns (n - 1) (Row.Value value :: acc)
          in
          let* columns = read_columns col_count [] in
          finish msg_type length reader (DataRow columns)
      | 'C' ->
          let* tag = read_string msg_type length reader "tag in CommandComplete" in
          finish msg_type length reader (CommandComplete tag)
      | 'E'
      | 'N' ->
          (* Build error record by reading all fields *)
          let rec read_fields err =
            if Binary_reader.is_eof reader then
              fail msg_type length reader "expected error response terminator"
            else
              match Binary_reader.read_byte reader with
              | None -> fail msg_type length reader "expected error field code"
              | Some 0 -> Ok err
              | Some field_code -> (
                  let field_char = Char.from_int_unchecked field_code in
                  match Binary_reader.read_string reader with
                  | None -> fail msg_type length reader "expected error field value"
                  | Some value ->
                      let err =
                        match field_char with
                        | 'S' -> { err with Error.severity = Some value }
                        | 'C' -> { err with Error.sqlstate = Some (Sqlstate.from_string value) }
                        | 'M' -> { err with Error.message = value }
                        | 'D' -> { err with Error.detail = Some value }
                        | 'H' -> { err with Error.hint = Some value }
                        | 'P' -> { err with Error.position = Int.parse value }
                        | 'p' -> { err with Error.internal_position = Int.parse value }
                        | 'q' -> { err with Error.internal_query = Some value }
                        | 'W' -> { err with Error.where_context = Some value }
                        | 's' -> { err with Error.schema_name = Some value }
                        | 't' -> { err with Error.table_name = Some value }
                        | 'c' -> { err with Error.column_name = Some value }
                        | 'd' -> { err with Error.datatype_name = Some value }
                        | 'n' -> { err with Error.constraint_name = Some value }
                        | 'F' -> { err with Error.source_file = Some value }
                        | 'L' -> { err with Error.source_line = Int.parse value }
                        | 'R' -> { err with Error.source_routine = Some value }
                        | _ -> err
                      in
                      read_fields err
                )
          in
          let empty_error: Error.t = {
            Error.severity = None;
            sqlstate = None;
            message = "Unknown error";
            detail = None;
            hint = None;
            position = None;
            internal_position = None;
            internal_query = None;
            where_context = None;
            schema_name = None;
            table_name = None;
            column_name = None;
            datatype_name = None;
            constraint_name = None;
            source_file = None;
            source_line = None;
            source_routine = None;
          }
          in
          let* error = read_fields empty_error in
          finish
            msg_type
            length
            reader
            (
              if msg_char = 'E' then
                ErrorResponse error
              else
                NoticeResponse error
            )
      | '1' -> finish msg_type length reader ParseComplete
      | '2' -> finish msg_type length reader BindComplete
      | '3' -> finish msg_type length reader CloseComplete
      | 'n' -> finish msg_type length reader NoData
      | 'I' -> finish msg_type length reader EmptyQueryResponse
      | 't' ->
          let* param_count = read_int16 msg_type length reader "param_count in ParameterDescription" in
          let rec read_oids n acc =
            if n = 0 then
              Ok (List.rev acc)
            else
              let* oid = read_int32 msg_type length reader "OID in ParameterDescription" in
              read_oids (n - 1) (TypeOid.from_int oid :: acc)
          in
          let* oids = read_oids param_count [] in
          finish msg_type length reader (ParameterDescription oids)
      | _ ->
          let hex =
            let h = msg_type lsr 4 in
            let l = msg_type land 0xf in
            let to_hex_char n =
              if n < 10 then
                Char.from_int_unchecked (48 + n)
              else
                Char.from_int_unchecked (87 + n)
            in
            String.make ~len:1 ~char:(to_hex_char h) ^ String.make ~len:1 ~char:(to_hex_char l)
          in
          fail msg_type length reader ("unknown message type 0x" ^ hex)

  let parse_backend_message = fun msg_type length bytes ->
    match parse_backend_message_result msg_type length bytes with
    | Ok message -> message
    | Error error -> panic (parse_error_to_string error)
end
