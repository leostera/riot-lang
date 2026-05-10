open Std

module Vector = Collections.Vector
module M = Sqlx.Migrate

let create_jobs_sql =
  String.concat
    "\n"
    [
      "create extension if not exists pgcrypto;";
      "create or replace function suri_jobs_uuid_v7() returns uuid as $$ declare unix_ts_ms bytea; rand_bytes bytea; begin unix_ts_ms = substring(int8send(floor(extract(epoch from clock_timestamp()) * 1000)::bigint) from 3); rand_bytes = gen_random_bytes(10); rand_bytes = set_byte(rand_bytes, 0, (get_byte(rand_bytes, 0) & 15) | 112); rand_bytes = set_byte(rand_bytes, 2, (get_byte(rand_bytes, 2) & 63) | 128); return encode(unix_ts_ms || rand_bytes, 'hex')::uuid; end $$ language plpgsql volatile;";
      "create table if not exists suri_jobs (id uuid primary key default suri_jobs_uuid_v7(), job_id text not null unique, queue_id text not null, worker_id text not null, state text not null, args jsonb not null, meta jsonb not null default '{}'::jsonb, tags jsonb not null default '[]'::jsonb, attempt integer not null default 0, max_attempts integer not null, priority integer not null default 0, unique_key text, fanout_id text, parent_job_id text, locked_by text, locked_at timestamptz, inserted_at timestamptz not null default now(), scheduled_at timestamptz not null default now(), attempted_at timestamptz, completed_at timestamptz, discarded_at timestamptz, cancelled_at timestamptz, last_error text);";
      "create unique index if not exists suri_jobs_unique_key_active_idx on suri_jobs(unique_key) where unique_key is not null and state in ('available', 'scheduled', 'executing', 'retryable');";
      "create index if not exists suri_jobs_available_idx on suri_jobs(queue_id, state, priority, scheduled_at, inserted_at);";
      "create index if not exists suri_jobs_fanout_id_idx on suri_jobs(fanout_id);";
      "create index if not exists suri_jobs_parent_job_id_idx on suri_jobs(parent_job_id);";
    ]

let create_fetch_index_sql =
  "create index if not exists suri_jobs_fetch_idx on suri_jobs(queue_id, worker_id, state, priority, scheduled_at, inserted_at);"

let migrations =
  Vector.from_list
    [
      M.Migration.make
        ~version:(M.Version.from_int64_unchecked 1L)
        ~description:"create suri jobs"
        ~migration_type:M.Simple
        ~sql:create_jobs_sql
        ();
      M.Migration.make
        ~version:(M.Version.from_int64_unchecked 2L)
        ~description:"index suri jobs fetches by queue worker"
        ~migration_type:M.Simple
        ~sql:create_fetch_index_sql
        ();
    ]

let source () = M.Source.from_migrations migrations

let migration_config () =
  let table_name = M.TableName.from_string_unchecked "suri_jobs_schema_migrations" in
  { M.Config.default with table_name }

let postgres_migration_config () =
  let table_name = M.TableName.from_string_unchecked "suri_jobs_schema_migrations" in
  { (M.Config.for_postgres ()) with table_name }
