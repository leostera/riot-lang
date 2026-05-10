open Std

module Json = Data.Json

type store = {
  list_jobs: limit:int -> (Job.stored list, Error.t) result;
  get_job: job_id:Job_id.t -> (Job.stored option, Error.t) result;
  state_counts: unit -> (Fanout.status, Error.t) result;
}

type route_error =
  | StoreError of Error.t
  | InvalidJobId
  | JobNotFound

let memory_store db = {
  list_jobs = (fun ~limit -> Memory.list db ~limit);
  get_job = (fun ~job_id -> Memory.get db ~job_id);
  state_counts = (fun () -> Memory.state_counts db);
}

let unavailable_store ?(error = Error.Not_started) () = {
  list_jobs = (fun ~limit:_ -> Error error);
  get_job = (fun ~job_id:_ -> Error error);
  state_counts = (fun () -> Error error);
}

let sqlx_store db = {
  list_jobs = (fun ~limit -> Sqlx_backend.list db ~limit);
  get_job = (fun ~job_id -> Sqlx_backend.get db ~job_id);
  state_counts = (fun () -> Sqlx_backend.state_counts db);
}

let html_escape value =
  let buffer = IO.Buffer.create ~size:(String.length value) in
  String.iter
    (fun char ->
      match char with
      | '&' -> IO.Buffer.add_string buffer "&amp;"
      | '<' -> IO.Buffer.add_string buffer "&lt;"
      | '>' -> IO.Buffer.add_string buffer "&gt;"
      | '"' -> IO.Buffer.add_string buffer "&quot;"
      | '\'' -> IO.Buffer.add_string buffer "&#39;"
      | other -> IO.Buffer.add_char buffer other)
    value;
  IO.Buffer.contents buffer

let trim_trailing_slash path =
  if String.length path > 1 && String.ends_with ~suffix:"/" path then
    String.sub path ~offset:0 ~len:(String.length path - 1)
  else
    path

let mounted_path conn ~suffix =
  let path = Suri.Conn.path conn |> trim_trailing_slash in
  if not (String.is_empty suffix) && String.ends_with ~suffix path then
    String.sub path ~offset:0 ~len:(String.length path - String.length suffix)
    |> trim_trailing_slash
  else
    path

let json_option encode = fun value ->
  match value with
  | Some value -> encode value
  | None -> Json.null

let id_json to_string value = Json.string (to_string value)

let job_json (job: Job.stored) =
  Json.obj
    [
      ("id", id_json Job_id.to_string job.id);
      ("queue", id_json Queue_id.to_string job.queue);
      ("worker", id_json Worker_id.to_string job.worker);
      ("state", Json.string (State.to_string job.state));
      ("attempt", Json.int job.attempt);
      ("max_attempts", Json.int job.max_attempts);
      ("priority", Json.int job.priority);
      ("unique_key", json_option (id_json Unique_key.to_string) job.unique_key);
      ("fanout_id", json_option (id_json Fanout_id.to_string) job.fanout_id);
      ("parent_job_id", json_option (id_json Job_id.to_string) job.parent_job_id);
      ("locked_by", json_option (id_json Worker_id.to_string) job.locked_by);
      ("locked_at", json_option Json.string job.locked_at);
      ("inserted_at", Json.string job.inserted_at);
      ("scheduled_at", Json.string job.scheduled_at);
      ("attempted_at", json_option Json.string job.attempted_at);
      ("completed_at", json_option Json.string job.completed_at);
      ("discarded_at", json_option Json.string job.discarded_at);
      ("cancelled_at", json_option Json.string job.cancelled_at);
      ("last_error", json_option Json.string job.last_error);
    ]

let status_json (status: Fanout.status) =
  Json.obj
    [
      ("total", Json.int status.total);
      ("available", Json.int status.available);
      ("scheduled", Json.int status.scheduled);
      ("executing", Json.int status.executing);
      ("retryable", Json.int status.retryable);
      ("completed", Json.int status.completed);
      ("cancelled", Json.int status.cancelled);
      ("discarded", Json.int status.discarded);
      ("suspended", Json.int status.suspended);
    ]

let route_error_to_json = fun error ->
  match error with
  | StoreError error -> Error.to_json error
  | InvalidJobId -> Json.obj [ ("kind", Json.string "invalid_job_id"); ]
  | JobNotFound -> Json.obj [ ("kind", Json.string "job_not_found"); ]

let error_json conn error =
  Suri.Conn.render_json
    Net.Http.Status.InternalServerError
    (Json.obj [ ("error", route_error_to_json (StoreError error)); ])
    conn

let invalid_job_id conn =
  Suri.Conn.render_json
    Net.Http.Status.BadRequest
    (Json.obj [ ("error", route_error_to_json InvalidJobId); ])
    conn

let not_found conn =
  Suri.Conn.render_json
    Net.Http.Status.NotFound
    (Json.obj [ ("error", route_error_to_json JobNotFound); ])
    conn

let param params name =
  match List.find params ~fn:(fun (key, _) -> String.equal key name) with
  | Some (_, value) -> Some value
  | None -> None

let parse_limit raw =
  match Int.from_string_opt raw with
  | Some value when value > 0 && value <= 200 -> value
  | Some value when value > 200 -> 200
  | _ -> 50

let limit conn =
  match param (Suri.Conn.query_params conn) "limit" with
  | Some raw -> parse_limit raw
  | None -> 50

let counts_html (status: Fanout.status) =
  String.concat
    ""
    [
      "<ul class=\"suri-jobs-counts\">";
      "<li><strong>";
      Int.to_string status.total;
      "</strong> total</li><li><strong>";
      Int.to_string status.available;
      "</strong> available</li><li><strong>";
      Int.to_string status.executing;
      "</strong> executing</li><li><strong>";
      Int.to_string status.retryable;
      "</strong> retryable</li><li><strong>";
      Int.to_string status.completed;
      "</strong> completed</li><li><strong>";
      Int.to_string status.discarded;
      "</strong> discarded</li></ul>";
    ]

let job_row mount_path (job: Job.stored) =
  let id = Job_id.to_string job.id in
  String.concat
    ""
    [
      "<tr><td><a href=\"";
      html_escape mount_path;
      "/jobs/";
      html_escape id;
      "\">";
      html_escape id;
      "</a></td><td>";
      html_escape (Queue_id.to_string job.queue);
      "</td><td>";
      html_escape (Worker_id.to_string job.worker);
      "</td><td>";
      html_escape (State.to_string job.state);
      "</td><td>";
      Int.to_string job.attempt;
      "/";
      Int.to_string job.max_attempts;
      "</td><td>";
      html_escape job.scheduled_at;
      "</td></tr>";
    ]

let index_html mount_path status jobs =
  String.concat
    ""
    [
      "<!doctype html><meta charset=\"utf-8\"><title>Suri Jobs</title>";
      "<style>body{font-family:system-ui,sans-serif;margin:2rem;line-height:1.4}table{border-collapse:collapse;width:100%}td,th{border-bottom:1px solid #ddd;padding:.45rem;text-align:left}code,pre{background:#f6f6f6;padding:.15rem .25rem}.suri-jobs-counts{display:flex;gap:1rem;list-style:none;padding:0;flex-wrap:wrap}</style>";
      "<h1>Suri Jobs</h1>";
      counts_html status;
      "<p><a href=\"";
      html_escape mount_path;
      "/jobs\">JSON</a></p>";
      "<table><thead><tr><th>Job</th><th>Queue</th><th>Worker</th><th>State</th><th>Attempt</th><th>Scheduled</th></tr></thead><tbody>";
      String.concat "" (List.map jobs ~fn:(job_row mount_path));
      "</tbody></table>";
    ]

let detail_html mount_path (job: Job.stored) =
  String.concat
    ""
    [
      "<!doctype html><meta charset=\"utf-8\"><title>Suri Job ";
      html_escape (Job_id.to_string job.id);
      "</title><style>body{font-family:system-ui,sans-serif;margin:2rem;line-height:1.4}dt{font-weight:700}dd{margin:0 0 .75rem}pre{background:#f6f6f6;padding:1rem;overflow:auto}</style>";
      "<p><a href=\"";
      html_escape mount_path;
      "\">Jobs</a> <a href=\"";
      html_escape mount_path;
      "/jobs/";
      html_escape (Job_id.to_string job.id);
      "/json\">JSON</a></p><h1>";
      html_escape (Job_id.to_string job.id);
      "</h1><dl><dt>Queue</dt><dd>";
      html_escape (Queue_id.to_string job.queue);
      "</dd><dt>Worker</dt><dd>";
      html_escape (Worker_id.to_string job.worker);
      "</dd><dt>State</dt><dd>";
      html_escape (State.to_string job.state);
      "</dd><dt>Attempts</dt><dd>";
      Int.to_string job.attempt;
      "/";
      Int.to_string job.max_attempts;
      "</dd><dt>Scheduled</dt><dd>";
      html_escape job.scheduled_at;
      "</dd><dt>Last error</dt><dd>";
      html_escape (Option.unwrap_or ~default:"" job.last_error);
      "</dd></dl><h2>Args</h2><pre>";
      html_escape job.args;
      "</pre><h2>Meta</h2><pre>";
      html_escape job.meta;
      "</pre><h2>Tags</h2><pre>";
      html_escape job.tags;
      "</pre>";
    ]

let list_json store conn _req =
  let limit = limit conn in
  match (store.state_counts (), store.list_jobs ~limit) with
  | (Error error, _)
  | (_, Error error) -> error_json conn error
  | (Ok status, Ok jobs) ->
      Suri.Conn.render_json
        Net.Http.Status.Ok
        (Json.obj
          [
            ("count", Json.int (List.length jobs));
            ("limit", Json.int limit);
            ("status", status_json status);
            ("jobs", Json.array (List.map jobs ~fn:job_json));
          ])
        conn

let list_html store conn _req =
  let limit = limit conn in
  match (store.state_counts (), store.list_jobs ~limit) with
  | (Error error, _)
  | (_, Error error) -> error_json conn error
  | (Ok status, Ok jobs) ->
      let mount_path = mounted_path conn ~suffix:"" in
      Suri.Conn.render_text
        ~headers:[ ("Content-Type", "text/html; charset=utf-8"); ]
        Net.Http.Status.Ok
        (index_html mount_path status jobs)
        conn

let job_id_param conn =
  match param (Suri.Conn.params conn) "job_id" with
  | None -> Error InvalidJobId
  | Some raw ->
      match Job_id.from_string raw with
      | Ok job_id -> Ok job_id
      | Error _ -> Error InvalidJobId

let show_json store conn _req =
  match job_id_param conn with
  | Error InvalidJobId -> invalid_job_id conn
  | Error (StoreError error) -> error_json conn error
  | Error JobNotFound -> not_found conn
  | Ok job_id ->
      match store.get_job ~job_id with
      | Error error -> error_json conn error
      | Ok None -> not_found conn
      | Ok (Some job) -> Suri.Conn.render_json Net.Http.Status.Ok (job_json job) conn

let show_html store conn _req =
  match job_id_param conn with
  | Error InvalidJobId -> invalid_job_id conn
  | Error (StoreError error) -> error_json conn error
  | Error JobNotFound -> not_found conn
  | Ok job_id ->
      match store.get_job ~job_id with
      | Error error -> error_json conn error
      | Ok None -> not_found conn
      | Ok (Some job) ->
          let mount_path =
            mounted_path conn ~suffix:("/jobs/" ^ Job_id.to_string job_id)
          in
          Suri.Conn.render_text
            ~headers:[ ("Content-Type", "text/html; charset=utf-8"); ]
            Net.Http.Status.Ok
            (detail_html mount_path job)
            conn

let routes = fun store ->
  Suri.Middleware.Router.[
    get "" (list_html store);
    scope
      "/jobs"
      [
        get "" (list_json store);
        scope "/:job_id" [ get "" (show_html store); get "/json" (show_json store); ];
      ];
  ]
