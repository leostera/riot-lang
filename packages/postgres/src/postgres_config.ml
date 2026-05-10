open Std

type ssl_mode =
  | Disable
  | Require
  | Prefer

type t = {
  host: string;
  port: int;
  database: string;
  user: string;
  password: string;
  ssl_mode: ssl_mode;
  application_name: string option;
  connect_timeout: Time.Duration.t;
  keepalives_idle: Time.Duration.t option;
}

type parse_error =
  | InvalidUserinfoFormat
  | InvalidAuthorityFormat
  | MissingUserCredentials
  | InvalidPortNumber of string
  | InvalidConnectionStringFormat
  | InvalidUri

let parse_error_to_string = fun error ->
  match error with
  | InvalidUserinfoFormat -> "invalid userinfo format in URI"
  | InvalidAuthorityFormat -> "invalid authority format in URI"
  | MissingUserCredentials -> "missing user credentials in URI"
  | InvalidPortNumber value -> "invalid port number: " ^ value
  | InvalidConnectionStringFormat -> "invalid connection string format (use 'postgresql://user:pass@host:port/db' or 'host:port:database:user:password')"
  | InvalidUri -> "failed to parse connection string"

let default = fun () ->
  {
    host = "localhost";
    port = 5_432;
    database = "postgres";
    user = "postgres";
    password = "";
    ssl_mode = Prefer;
    application_name = None;
    connect_timeout = Time.Duration.from_secs 10;
    keepalives_idle = None;
  }

let from_string = fun str ->
  match Net.Uri.from_string str with
  | Ok uri when Net.Uri.scheme uri = Some "postgresql" || Net.Uri.scheme uri = Some "postgres" -> (
      let hostname =
        Net.Uri.host uri
        |> Option.unwrap_or ~default:"localhost"
      in
      let port =
        Net.Uri.port uri
        |> Option.unwrap_or ~default:5_432
      in
      let host =
        match Net.Addr.from_host_and_port ~host:hostname ~port with
        | Ok addr ->
            let resolved_ip = Net.Addr.ip addr in
            if resolved_ip = "::1" then
              "127.0.0.1"
            else
              resolved_ip
        | Error _ -> hostname
      in
      let path = Net.Uri.path uri in
      let database =
        if String.length path > 1 && String.get_unchecked path ~at:0 = '/' then
          String.sub path ~offset:1 ~len:(String.length path - 1)
        else
          "postgres"
      in
      match Net.Uri.authority uri with
      | Some auth_str -> (
          match String.split_on_char '@' auth_str with
          | [ userinfo; _ ] -> (
              match String.split_on_char ':' userinfo with
              | [ user; password ] ->
                  Ok {
                    host;
                    port;
                    database;
                    user;
                    password;
                    ssl_mode = Prefer;
                    application_name = None;
                    connect_timeout = Time.Duration.from_secs 10;
                    keepalives_idle = None;
                  }
              | [ user ] ->
                  Ok {
                    host;
                    port;
                    database;
                    user;
                    password = "";
                    ssl_mode = Prefer;
                    application_name = None;
                    connect_timeout = Time.Duration.from_secs 10;
                    keepalives_idle = None;
                  }
              | _ -> Error InvalidUserinfoFormat
            )
          | _ -> Error InvalidAuthorityFormat
        )
      | None -> Error MissingUserCredentials
    )
  | Ok _ -> (
      match String.split_on_char ':' str with
      | [ host; port_str; database; user; password ] -> (
          match Int.parse port_str with
          | Some port ->
              Ok {
                host;
                port;
                database;
                user;
                password;
                ssl_mode = Prefer;
                application_name = None;
                connect_timeout = Time.Duration.from_secs 10;
                keepalives_idle = None;
              }
          | None -> Error (InvalidPortNumber port_str)
        )
      | _ -> Error InvalidConnectionStringFormat
    )
  | Error _ -> Error InvalidUri
