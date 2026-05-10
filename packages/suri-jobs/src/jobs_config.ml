open Std
open Result.Syntax

module StdConfig = Std.Config

type backend =
  | Backend: {
      driver: (module Sqlx.Driver.Intf with type config = 'config);
      config: 'config;
      dialect: Schema.dialect;
    } -> backend
  | Postgres_env of { postgres_url_env: string }
  | Mysql_env of { mysql_url_env: string }

type t = {
  pool_config: Sqlx.Config.t;
  migration_config: Sqlx.Migrate.Config.t;
  migration_source: Sqlx.Migrate.Source.t;
  backend: backend;
}

let default_pool_size = Int.max 4 (Thread.available_parallelism * 2)

let default = {
  pool_config = { Sqlx.Config.default with pool_size = default_pool_size };
  migration_config = Schema.postgres_migration_config ();
  migration_source = Schema.source ();
  backend = Postgres_env { postgres_url_env = "SURI_JOBS_POSTGRES_URL" };
}

let driver_name = fun (type config) (driver: (module Sqlx.Driver.Intf with type config = config)) ->
  let module D = (val driver) in
  D.name

let make
  ?pool_size
  ?(pool_config = Sqlx.Config.default)
  ?dialect
  ?migration_config
  ?migration_source
  ~driver
  config =
  let dialect =
    match dialect with
    | Some dialect -> dialect
    | None ->
        if String.equal (driver_name driver) "MySQL" then
          Schema.Mysql
        else
          Schema.Postgres
  in
  let migration_config =
    Option.unwrap_or ~default:(Schema.migration_config_for dialect) migration_config
  in
  let migration_source = Option.unwrap_or ~default:(Schema.source_for dialect) migration_source in
  let pool_config =
    match pool_size with
    | None -> pool_config
    | Some pool_size -> { pool_config with Sqlx.Config.pool_size }
  in
  {
    pool_config;
    migration_config;
    migration_source;
    backend = Backend { driver; config; dialect };
  }

let spec =
  StdConfig.Spec.for_app
    ~app:"suri-jobs"
    [
      StdConfig.Spec.string
        "backend"
        ~default:"postgres"
        ~help:"Suri jobs storage backend. Supports postgres and mysql.";
      StdConfig.Spec.string
        "postgres_url_env"
        ~default:"SURI_JOBS_POSTGRES_URL"
        ~help:"Environment variable containing the Suri jobs PostgreSQL URL";
      StdConfig.Spec.string
        "mysql_url_env"
        ~default:"SURI_JOBS_MYSQL_URL"
        ~help:"Environment variable containing the Suri jobs MySQL URL";
      StdConfig.Spec.int "pool_size" ~default:default_pool_size ~help:"Suri jobs database pool size";
      StdConfig.Spec.string
        "migration_table"
        ~default:"suri_jobs_schema_migrations"
        ~help:"SQL migration table name for Suri jobs migrations";
    ]

let normalize_backend value = String.lowercase_ascii (String.trim value)

let migration_config_from_table dialect value =
  match Sqlx.Migrate.TableName.from_string value with
  | Error error ->
      Error (StdConfig.ValidationError {
        app = "suri-jobs";
        errors = [ Sqlx.Migrate.TableName.error_to_string error ];
      })
  | Ok table_name -> Ok { (Schema.migration_config_for dialect) with table_name }

let get conf =
  let backend = normalize_backend (StdConfig.get_string conf "backend") in
  let postgres_url_env = StdConfig.get_string conf "postgres_url_env" in
  let mysql_url_env = StdConfig.get_string conf "mysql_url_env" in
  let pool_size = StdConfig.get_int conf "pool_size" in
  let migration_table = StdConfig.get_string conf "migration_table" in
  if pool_size <= 0 then
    Error (StdConfig.ValidationError {
      app = "suri-jobs";
      errors = [ "pool_size must be greater than 0" ];
    })
  else
    match backend with
    | "postgres" ->
        let* migration_config = migration_config_from_table Schema.Postgres migration_table in
        Ok {
          pool_config = { Sqlx.Config.default with pool_size };
          migration_config;
          migration_source = Schema.postgres_source ();
          backend = Postgres_env { postgres_url_env };
        }
    | "mysql" ->
        let* migration_config = migration_config_from_table Schema.Mysql migration_table in
        Ok {
          pool_config = { Sqlx.Config.default with pool_size };
          migration_config;
          migration_source = Schema.mysql_source ();
          backend = Mysql_env { mysql_url_env };
        }
    | other ->
        Error (StdConfig.ValidationError {
          app = "suri-jobs";
          errors = [ "backend must be postgres or mysql, got " ^ other ];
        })

module Std_config = struct
  let spec = spec

  type nonrec t = t

  let get = get
end

let load () = StdConfig.get (module Std_config)

let require_env env =
  match Env.get Env.String ~var:env with
  | Some value -> Ok value
  | None -> Error (Error.Config (Error.Missing_env env))

let postgres_config_from_env env =
  let* url = require_env env in
  match Postgres.Config.from_string url with
  | Ok config -> Ok config
  | Error error ->
      Error (Error.Config (Error.Invalid_postgres_url {
        env;
        message = Postgres.Config.parse_error_to_string error;
      }))

let mysql_config_from_env env =
  let* url = require_env env in
  match Mysql.Config.from_string url with
  | Ok config -> Ok config
  | Error error ->
      Error (Error.Config (Error.Invalid_mysql_url {
        env;
        message = Mysql.Config.parse_error_to_string error;
      }))

let connect config =
  match config.backend with
  | Backend backend ->
      Sqlx_backend.connect
        ~pool_config:config.pool_config
        ~dialect:backend.dialect
        ~driver:backend.driver
        backend.config
  | Postgres_env { postgres_url_env } ->
      let* postgres_config = postgres_config_from_env postgres_url_env in
      Sqlx_backend.connect
        ~pool_config:config.pool_config
        ~dialect:Schema.Postgres
        ~driver:(module Postgres.Driver)
        postgres_config
  | Mysql_env { mysql_url_env } ->
      let* mysql_config = mysql_config_from_env mysql_url_env in
      Sqlx_backend.connect
        ~pool_config:config.pool_config
        ~dialect:Schema.Mysql
        ~driver:(module Mysql.Driver)
        mysql_config

let migrate config db =
  Sqlx_backend.migrate_with ~config:config.migration_config ~source:config.migration_source db
