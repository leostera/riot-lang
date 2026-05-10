open Std
open Result.Syntax

type t = Sqlx.Pool.t

let connect ?pool_size ?(pool_config = Sqlx.Config.default) ~driver config =
  let sqlx_config =
    match pool_size with
    | None -> pool_config
    | Some pool_size -> { pool_config with Sqlx.Config.pool_size }
  in
  match Sqlx.connect ~config:sqlx_config ~driver config with
  | Ok pool -> Ok pool
  | Error error -> Error (Error.Sqlx error)

let shutdown db = Sqlx.shutdown db

let migrate_with ?(config = Schema.postgres_migration_config ()) ?(source = Schema.source ()) db =
  match Sqlx.migrate ~config ~source db () with
  | Ok () -> Ok ()
  | Error error -> Error (Error.Migration error)

let migrate db = migrate_with db

let sql_text value = Sqlx.Value.string value

let sql_json value = Sqlx.Value.json value

let sql_option = fun value ->
  match value with
  | Some value -> sql_text value
  | None -> Sqlx.Value.null

let sql_option_map value ~fn =
  match value with
  | Some value -> sql_text (fn value)
  | None -> Sqlx.Value.null

let sql_timestamp value = Sqlx.Value.string value

let driver_error_type = fun error ->
  match error with
  | Sqlx.Connection.DriverError { error; to_json; _ } ->
      (
        match Data.Json.get_field "type" (to_json error) with
      | Some value -> Data.Json.get_string value
      | None -> None
      )
  | Sqlx.Connection.RuntimeError _ -> Some "runtime_error"

let driver_sqlstate = fun error ->
  match error with
  | Sqlx.Connection.DriverError { error; to_json; _ } ->
      (
        match Data.Json.get_field "sqlstate" (to_json error) with
      | Some value -> Data.Json.get_string value
      | None -> None
      )
  | Sqlx.Connection.RuntimeError _ -> None

let connection_sqlstate_matches values error =
  match driver_sqlstate error with
  | Some sqlstate -> List.exists (String.equal sqlstate) values
  | None -> false

let connection_error_type_matches values error =
  match driver_error_type error with
  | Some error_type -> List.exists (String.equal error_type) values
  | None -> false

let duplicate_prepared_statement = fun error ->
  match error with
  | Sqlx.PoolError (Sqlx.Pool.ConnectionError error) ->
      connection_sqlstate_matches [ "duplicate_prepared_statement"; "42P05"; ] error
  | _ -> false

let retryable_sqlstate = fun error ->
  match error with
  | Sqlx.PoolError (Sqlx.Pool.ConnectionError error) ->
      connection_sqlstate_matches [ "protocol_violation"; "08P01"; ] error
  | _ -> false

let sqlx_connection_error = fun error ->
  match error with
  | Sqlx.PoolError (Sqlx.Pool.ConnectionError error) ->
      connection_error_type_matches
        [ "transport_error"; "connection_closed"; "unexpected_message"; "runtime_error"; ]
        error
  | _ -> false

let retryable_sqlx_error error =
  duplicate_prepared_statement error || retryable_sqlstate error || sqlx_connection_error error

let unique_violation = fun error ->
  match error with
  | Sqlx.PoolError (Sqlx.Pool.ConnectionError error) ->
      connection_sqlstate_matches [ "unique_violation"; "23505"; ] error
  | _ -> false

let with_sqlx_retry operation =
  let rec attempt remaining =
    match operation () with
    | Ok _ as ok -> ok
    | Error error when remaining > 0 && retryable_sqlx_error error -> attempt (remaining - 1)
    | Error _ as error -> error
  in
  attempt 5

let exec db sql values =
  match with_sqlx_retry (fun () -> Sqlx.exec db sql values) with
  | Ok _ -> Ok ()
  | Error error -> Error (Error.Sqlx error)

let query_cursor db sql values consume =
  let run_once () =
    match Sqlx.Pool.acquire db with
    | Error error -> Error (Error.Sqlx (Sqlx.PoolError error))
    | Ok conn ->
        let release () = Sqlx.Pool.release db conn in
        let result =
          try
            match Sqlx.Connection.query conn sql values with
            | Error error ->
                Sqlx.Connection.close conn;
                Error (Error.Sqlx (Sqlx.PoolError (Sqlx.Pool.ConnectionError error)))
            | Ok cursor -> consume cursor
          with
          | exn ->
              release ();
              raise exn
        in
        release ();
        result
  in
  let rec attempt remaining =
    match run_once () with
    | Ok _ as ok -> ok
    | Error (Error.Sqlx error) when remaining > 0 && retryable_sqlx_error error ->
        attempt (remaining - 1)
    | Error _ as error -> error
  in
  attempt 5

let query_many db sql values decode =
  match query_cursor
    db
    sql
    values
    (fun cursor ->
      let rows = Sqlx.Cursor.to_mut_iter cursor in
      let rec loop acc =
        match Iter.MutIterator.next rows with
        | None -> Ok (List.reverse acc)
        | Some row ->
            let* value = decode row in
            loop (value :: acc)
      in
      loop []) with
  | Ok rows -> Ok rows
  | Error _ as error -> error

let missing field = Error (Error.Missing_field (Error.FieldMissing field))

let value_string field value =
  match value with
  | Sqlx.Value.String value
  | Sqlx.Value.Uuid value
  | Sqlx.Value.Json value
  | Sqlx.Value.Numeric value -> Ok value
  | Sqlx.Value.Timestamp value
  | Sqlx.Value.TimestampWithTimezone value -> Ok (DateTime.to_iso8601 value)
  | Sqlx.Value.Int value -> Ok (Int.to_string value)
  | Sqlx.Value.Int64 value -> Ok (Int64.to_string value)
  | Sqlx.Value.Null -> missing field
  | value ->
      Error (Error.Missing_field (Error.FieldTypeMismatch {
        field;
        expected = Error.ExpectedText;
        actual = Sqlx.Value.to_string value;
      }))

let value_string_option field value =
  match value with
  | Sqlx.Value.Null -> Ok None
  | value ->
      let* value = value_string field value in
      Ok (Some value)

let row_value field row =
  match Sqlx.Row.get field row with
  | Some value -> Ok value
  | None -> missing field

let row_string field row =
  let* value = row_value field row in
  value_string field value

let row_string_option field row =
  match Sqlx.Row.get field row with
  | Some value -> value_string_option field value
  | None -> Ok None

let parse_row_int field value =
  match Int.from_string_opt value with
  | Some value -> Ok value
  | None -> missing field

let row_int field row =
  match Sqlx.Row.get field row with
  | Some (Sqlx.Value.Int value) -> Ok value
  | Some (Sqlx.Value.Int64 value) -> Ok (Int64.to_int value)
  | Some (Sqlx.Value.String value) -> parse_row_int field value
  | Some value ->
      Error (Error.Missing_field (Error.FieldTypeMismatch {
        field;
        expected = Error.ExpectedInt;
        actual = Sqlx.Value.to_string value;
      }))
  | None -> missing field

let decode_job row =
  let* job_id = row_string "job_id" row in
  let* queue_id = row_string "queue_id" row in
  let* worker_id = row_string "worker_id" row in
  let* state_text = row_string "state" row in
  let* state = State.from_string state_text in
  let* args = row_string "args" row in
  let* meta = row_string "meta" row in
  let* tags = row_string "tags" row in
  let* attempt = row_int "attempt" row in
  let* max_attempts = row_int "max_attempts" row in
  let* priority = row_int "priority" row in
  let* unique_key = row_string_option "unique_key" row in
  let* fanout_id = row_string_option "fanout_id" row in
  let* parent_job_id = row_string_option "parent_job_id" row in
  let* locked_by = row_string_option "locked_by" row in
  let* locked_at = row_string_option "locked_at" row in
  let* inserted_at = row_string "inserted_at" row in
  let* scheduled_at = row_string "scheduled_at" row in
  let* attempted_at = row_string_option "attempted_at" row in
  let* completed_at = row_string_option "completed_at" row in
  let* discarded_at = row_string_option "discarded_at" row in
  let* cancelled_at = row_string_option "cancelled_at" row in
  let* last_error = row_string_option "last_error" row in
  let parse_id field parse value =
    match parse value with
    | Ok value -> Ok value
    | Error _ -> missing field
  in
  let* job_id = parse_id "job_id" Job_id.from_string job_id in
  let* queue_id = parse_id "queue_id" Queue_id.from_string queue_id in
  let* worker_id = parse_id "worker_id" Worker_id.from_string worker_id in
  let* unique_key =
    match unique_key with
    | None -> Ok None
    | Some value ->
        let* value = parse_id "unique_key" Unique_key.from_string value in
        Ok (Some value)
  in
  let* fanout_id =
    match fanout_id with
    | None -> Ok None
    | Some value ->
        let* value = parse_id "fanout_id" Fanout_id.from_string value in
        Ok (Some value)
  in
  let* parent_job_id =
    match parent_job_id with
    | None -> Ok None
    | Some value ->
        let* value = parse_id "parent_job_id" Job_id.from_string value in
        Ok (Some value)
  in
  let* locked_by =
    match locked_by with
    | None -> Ok None
    | Some value ->
        let* value = parse_id "locked_by" Worker_id.from_string value in
        Ok (Some value)
  in
  Ok Job.{
    id = job_id;
    queue = queue_id;
    worker = worker_id;
    state;
    args;
    meta;
    tags;
    attempt;
    max_attempts;
    priority;
    unique_key;
    fanout_id;
    parent_job_id;
    locked_by;
    locked_at;
    inserted_at;
    scheduled_at;
    attempted_at;
    completed_at;
    discarded_at;
    cancelled_at;
    last_error;
  }

let select_columns =
  "job_id, queue_id, worker_id, state, args, meta, tags, attempt, max_attempts, priority, unique_key, fanout_id, parent_job_id, locked_by, locked_at, inserted_at, scheduled_at, attempted_at, completed_at, discarded_at, cancelled_at, last_error"

let select_by_job_id db job_id =
  match query_many
    db
    ("select " ^ select_columns ^ " from suri_jobs where job_id = $1 limit 1")
    [ sql_text (Job_id.to_string job_id) ]
    decode_job with
  | Error _ as error -> error
  | Ok (job :: _) -> Ok job
  | Ok [] -> Error (Error.Missing_field (Error.JobRowMissing job_id))

let select_by_active_unique_key db unique_key =
  match query_many
    db
    ("select "
    ^ select_columns
    ^ " from suri_jobs where unique_key = $1 and state in ('available', 'scheduled', 'executing', 'retryable') order by inserted_at limit 1")
    [ sql_text (Unique_key.to_string unique_key) ]
    decode_job with
  | Error _ as error -> error
  | Ok (job :: _) -> Ok job
  | Ok [] -> Error (Error.Missing_field (Error.ActiveUniqueKeyRowMissing unique_key))

let recover_unique_violation db request error =
  match request.Job.unique_key with
  | Some unique_key -> select_by_active_unique_key db unique_key
  | None -> Error (Error.Sqlx error)

let recover_empty_insert db request =
  match select_by_job_id db request.Job.id with
  | Ok job -> Ok job
  | Error id_error ->
      match request.Job.unique_key with
      | Some unique_key -> select_by_active_unique_key db unique_key
      | None -> Error id_error

let insert_job db (request: 'payload Job.enqueue) =
  let state = Job.state_for_enqueue request in
  match query_many
    db
    ("insert into suri_jobs (job_id, queue_id, worker_id, state, args, meta, tags, max_attempts, priority, scheduled_at, unique_key, fanout_id, parent_job_id) values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10::timestamptz, nullif($11, ''), nullif($12, ''), nullif($13, '')) on conflict (job_id) do update set job_id = excluded.job_id returning "
    ^ select_columns)
    [
      sql_text (Job_id.to_string request.Job.id);
      sql_text (Queue_id.to_string request.queue);
      sql_text (Worker_id.to_string request.worker);
      sql_text (State.to_string state);
      sql_json request.args;
      sql_json request.meta;
      sql_json request.tags;
      Sqlx.Value.int request.max_attempts;
      Sqlx.Value.int request.priority;
      sql_timestamp request.scheduled_at;
      sql_option_map request.unique_key ~fn:Unique_key.to_string;
      sql_option_map request.fanout_id ~fn:Fanout_id.to_string;
      sql_option_map request.parent_job_id ~fn:Job_id.to_string;
    ]
    decode_job with
  | Error (Error.Sqlx error) when unique_violation error ->
      recover_unique_violation db request error
  | Error _ as error -> error
  | Ok (job :: _) -> Ok job
  | Ok [] -> recover_empty_insert db request

let enqueue_with_unique db request unique_key =
  match select_by_active_unique_key db unique_key with
  | Ok job -> Ok job
  | Error (Error.Missing_field _) -> insert_job db request
  | Error _ as error -> error

let enqueue db (request: 'payload Job.enqueue) =
  match request.Job.unique_key with
  | Some unique_key -> enqueue_with_unique db request unique_key
  | None -> insert_job db request

let enqueue_many db (requests: 'payload Job.enqueue list) =
  let rec loop acc = fun requests ->
    match requests with
    | [] -> Ok (List.reverse acc)
    | request :: rest ->
        let* stored = enqueue db request in
        loop (stored :: acc) rest
  in
  loop [] requests

let fetch db ?(stale_after_seconds = 900) queue ~limit ~locked_by =
  let* stored =
    query_many
      db
      ("with picked as (select job_id as picked_job_id from suri_jobs where queue_id = $1 and worker_id = $5 and (((attempt < max_attempts and state in ('available', 'scheduled', 'retryable') and scheduled_at <= now()) or (state = 'executing' and locked_at is not null and locked_at <= now() - ($3::integer * interval '1 second')))) order by priority asc, scheduled_at asc, inserted_at asc limit $2 for update skip locked) update suri_jobs j set state = 'executing', attempt = case when j.state = 'executing' then j.attempt else j.attempt + 1 end, attempted_at = now(), locked_by = $4, locked_at = now(), last_error = null from picked where j.job_id = picked.picked_job_id and j.queue_id = $1 and j.worker_id = $5 returning "
      ^ select_columns)
      [
        sql_text (Queue_id.to_string (Queue.id queue));
        Sqlx.Value.int limit;
        Sqlx.Value.int stale_after_seconds;
        sql_text (Worker_id.to_string locked_by);
        sql_text (Worker_id.to_string (Queue.worker queue));
      ]
      decode_job
  in
  let rec decode acc = fun jobs ->
    match jobs with
    | [] -> Ok (List.reverse acc)
    | job :: rest ->
        let* typed = Job.decode queue job in
        decode (typed :: acc) rest
  in
  decode [] stored

let recover_executing db queue =
  exec
    db
    "update suri_jobs set state = case when attempt >= max_attempts then 'discarded' else 'retryable' end, scheduled_at = case when attempt >= max_attempts then scheduled_at else now() end, discarded_at = case when attempt >= max_attempts then now() else discarded_at end, locked_by = null, locked_at = null, last_error = '{\"kind\":\"worker_shutdown\"}' where queue_id = $1 and worker_id = $2 and state = 'executing'"
    [
      sql_text (Queue_id.to_string (Queue.id queue));
      sql_text (Worker_id.to_string (Queue.worker queue));
    ]

let complete db (stored: Job.stored) =
  exec
    db
    "update suri_jobs set state = 'completed', completed_at = now(), locked_by = null, locked_at = null, last_error = null where job_id = $1"
    [ sql_text (Job_id.to_string stored.Job.id) ]

let fail db (stored: Job.stored) ~error ~backoff_seconds =
  exec
    db
    "update suri_jobs set state = case when attempt >= max_attempts then 'discarded' else 'retryable' end, scheduled_at = case when attempt >= max_attempts then scheduled_at else now() + ($2::integer * interval '1 second') end, discarded_at = case when attempt >= max_attempts then now() else discarded_at end, last_error = $3, locked_by = null, locked_at = null where job_id = $1"
    [ sql_text (Job_id.to_string stored.Job.id); Sqlx.Value.int backoff_seconds; sql_text error ]

let cancel db (stored: Job.stored) =
  exec
    db
    "update suri_jobs set state = 'cancelled', cancelled_at = now(), locked_by = null, locked_at = null where job_id = $1"
    [ sql_text (Job_id.to_string stored.Job.id) ]

let list db ~limit =
  query_many
    db
    ("select " ^ select_columns ^ " from suri_jobs order by inserted_at desc limit $1")
    [ Sqlx.Value.int limit ]
    decode_job

let get db ~job_id =
  match query_many
    db
    ("select " ^ select_columns ^ " from suri_jobs where job_id = $1 limit 1")
    [ sql_text (Job_id.to_string job_id) ]
    decode_job with
  | Error _ as error -> error
  | Ok (job :: _) -> Ok (Some job)
  | Ok [] -> Ok None

let decode_fanout_count row =
  let* state_text = row_string "state" row in
  let* state = State.from_string state_text in
  let* count = row_int "count" row in
  Ok (state, count)

let state_counts db =
  let* counts =
    query_many
      db
      "select state, count(*) as count from suri_jobs group by state"
      []
      decode_fanout_count
  in
  Ok (List.fold_left
    counts
    ~init:Fanout.empty
    ~fn:(fun status (state, count) ->
      Fanout.add_count state count status))

let fanout_status db ~fanout_id =
  let* counts =
    query_many
      db
      "select state, count(*) as count from suri_jobs where fanout_id = $1 group by state"
      [ sql_text (Fanout_id.to_string fanout_id) ]
      decode_fanout_count
  in
  Ok (List.fold_left
    counts
    ~init:Fanout.empty
    ~fn:(fun status (state, count) ->
      Fanout.add_count state count status))
