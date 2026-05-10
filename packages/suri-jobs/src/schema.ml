open Std

module Vector = Collections.Vector
module M = Sqlx.Migrate

type dialect =
  | Postgres
  | Mysql

let create_postgres_jobs_sql =
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

let create_postgres_fetch_index_sql =
  "create index if not exists suri_jobs_fetch_idx on suri_jobs(queue_id, worker_id, state, priority, scheduled_at, inserted_at);"

let create_mysql_jobs_sql =
  String.concat
    "\n"
    [
      "create table if not exists suri_jobs (";
      "id bigint unsigned not null auto_increment primary key,";
      "job_id varchar(200) not null unique,";
      "queue_id varchar(200) not null,";
      "worker_id varchar(200) not null,";
      "state varchar(32) not null,";
      "args json not null,";
      "meta json not null,";
      "tags json not null,";
      "attempt integer not null default 0,";
      "max_attempts integer not null,";
      "priority integer not null default 0,";
      "unique_key varchar(500),";
      "active_unique_key varchar(500) generated always as (case when unique_key is not null and state in ('available', 'scheduled', 'executing', 'retryable') then unique_key else null end) stored,";
      "fanout_id varchar(200),";
      "parent_job_id varchar(200),";
      "locked_by varchar(200),";
      "locked_at varchar(64),";
      "inserted_at varchar(64) not null,";
      "scheduled_at varchar(64) not null,";
      "attempted_at varchar(64),";
      "completed_at varchar(64),";
      "discarded_at varchar(64),";
      "cancelled_at varchar(64),";
      "last_error text,";
      "unique key suri_jobs_unique_key_active_idx (active_unique_key),";
      "index suri_jobs_available_idx (queue_id, state, priority, scheduled_at, inserted_at),";
      "index suri_jobs_fanout_id_idx (fanout_id),";
      "index suri_jobs_parent_job_id_idx (parent_job_id),";
      "index suri_jobs_fetch_idx (queue_id, worker_id, state, priority, scheduled_at, inserted_at)";
      ") engine=InnoDB";
    ]

let create_mysql_fetch_index_sql = "select 1"

let postgres_migrations =
  Vector.from_list
    [
      M.Migration.make
        ~version:(M.Version.from_int64_unchecked 1L)
        ~description:"create suri jobs"
        ~migration_type:M.Simple
        ~sql:create_postgres_jobs_sql
        ();
      M.Migration.make
        ~version:(M.Version.from_int64_unchecked 2L)
        ~description:"index suri jobs fetches by queue worker"
        ~migration_type:M.Simple
        ~sql:create_postgres_fetch_index_sql
        ();
    ]

let mysql_migrations =
  Vector.from_list
    [
      M.Migration.make
        ~version:(M.Version.from_int64_unchecked 1L)
        ~description:"create suri jobs"
        ~migration_type:M.Simple
        ~sql:create_mysql_jobs_sql
        ();
      M.Migration.make
        ~version:(M.Version.from_int64_unchecked 2L)
        ~description:"index suri jobs fetches by queue worker"
        ~migration_type:M.Simple
        ~sql:create_mysql_fetch_index_sql
        ();
    ]

let create_jobs_sql = create_postgres_jobs_sql

let create_fetch_index_sql = create_postgres_fetch_index_sql

let migrations = postgres_migrations

let source () = M.Source.from_migrations migrations

let postgres_source () = M.Source.from_migrations postgres_migrations

let mysql_source () = M.Source.from_migrations mysql_migrations

let source_for = fun dialect ->
  match dialect with
  | Postgres -> postgres_source ()
  | Mysql -> mysql_source ()

let migration_config () =
  let table_name = M.TableName.from_string_unchecked "suri_jobs_schema_migrations" in
  { M.Config.default with table_name }

let postgres_migration_config () =
  let table_name = M.TableName.from_string_unchecked "suri_jobs_schema_migrations" in
  { (M.Config.for_postgres ()) with table_name }

let mysql_migration_config () =
  let table_name = M.TableName.from_string_unchecked "suri_jobs_schema_migrations" in
  { (M.Config.for_mysql ()) with table_name }

let migration_config_for = fun dialect ->
  match dialect with
  | Postgres -> postgres_migration_config ()
  | Mysql -> mysql_migration_config ()
