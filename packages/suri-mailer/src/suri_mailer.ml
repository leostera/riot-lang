open Std
open Result.Syntax

module Message = struct
  type t = {
    from: string option;
    to_: string list;
    cc: string list;
    bcc: string list;
    reply_to: string list;
    subject: string;
    headers: (string * string) list;
    text_body: string option;
    html_body: string option;
  }

  let make = fun
    ?from ?(cc = []) ?(bcc = []) ?(reply_to = []) ?(headers = []) ?text ?html ~to_ ~subject () ->
    {
      from;
      to_;
      cc;
      bcc;
      reply_to;
      subject;
      headers;
      text_body = text;
      html_body = html;
    }

  let recipients = fun message -> (message.to_ @ message.cc) @ message.bcc

  let pad2 = fun value ->
    let raw = Int.to_string value in
    if String.length raw >= 2 then
      raw
    else
      "0" ^ raw

  let pad4 = fun value ->
    let raw = Int.to_string value in
    if String.length raw >= 4 then
      raw
    else
      String.make ~len:(4 - String.length raw) ~char:'0' ^ raw

  let weekday = fun value ->
    match value with
    | 0 -> "Sun"
    | 1 -> "Mon"
    | 2 -> "Tue"
    | 3 -> "Wed"
    | 4 -> "Thu"
    | 5 -> "Fri"
    | _ -> "Sat"

  let month = fun value ->
    match value with
    | 0 -> "Jan"
    | 1 -> "Feb"
    | 2 -> "Mar"
    | 3 -> "Apr"
    | 4 -> "May"
    | 5 -> "Jun"
    | 6 -> "Jul"
    | 7 -> "Aug"
    | 8 -> "Sep"
    | 9 -> "Oct"
    | 10 -> "Nov"
    | _ -> "Dec"

  let date_header = fun () ->
    let tm =
      Time.SystemTime.now ()
      |> Time.SystemTime.secs_float
      |> Time.gmtime
    in
    String.concat
      ""
      [
        weekday tm.tm_wday;
        ", ";
        pad2 tm.tm_mday;
        " ";
        month tm.tm_mon;
        " ";
        pad4 (tm.tm_year + 1_900);
        " ";
        pad2 tm.tm_hour;
        ":";
        pad2 tm.tm_min;
        ":";
        pad2 tm.tm_sec;
        " +0000";
      ]

  let sanitize_header_value = fun value ->
    let buffer = IO.Buffer.create ~size:(String.length value) in
    String.iter
      (fun char ->
        match char with
        | '\r'
        | '\n' -> IO.Buffer.add_char buffer ' '
        | other -> IO.Buffer.add_char buffer other)
      value;
    IO.Buffer.contents buffer

  let is_header_name_char = fun char ->
    (char >= 'A' && char <= 'Z')
    || (char >= 'a' && char <= 'z')
    || (char >= '0' && char <= '9')
    || char = '-'

  let is_valid_header_name = fun name ->
    not (String.is_empty name) && String.for_all name ~fn:is_header_name_char

  let header = fun name value ->
    if is_valid_header_name name then
      Some (name ^ ": " ^ sanitize_header_value value)
    else
      None

  let header_list = fun name values ->
    match values with
    | [] -> None
    | _ -> header name (String.concat ", " values)

  let boundary = fun message ->
    let seed =
      String.length message.subject
      + Option.unwrap_or ~default:0 (Option.map message.text_body ~fn:String.length)
      + Option.unwrap_or ~default:0 (Option.map message.html_body ~fn:String.length)
    in
    "suri-mailer-" ^ Int.to_string seed ^ "-alternative"

  let content_headers = fun content_type -> [
    "MIME-Version: 1.0";
    "Content-Type: " ^ content_type ^ "; charset=utf-8";
    "Content-Transfer-Encoding: 8bit";
  ]

  let render_body = fun message ->
    match (message.text_body, message.html_body) with
    | (Some text, Some html) ->
        let boundary = boundary message in
        String.concat
          "\r\n"
          [
            "MIME-Version: 1.0";
            "Content-Type: multipart/alternative; boundary=\"" ^ boundary ^ "\"";
            "";
            "--" ^ boundary;
            "Content-Type: text/plain; charset=utf-8";
            "Content-Transfer-Encoding: 8bit";
            "";
            text;
            "--" ^ boundary;
            "Content-Type: text/html; charset=utf-8";
            "Content-Transfer-Encoding: 8bit";
            "";
            html;
            "--" ^ boundary ^ "--";
          ]
    | (Some text, None) -> String.concat "\r\n" (content_headers "text/plain" @ [ ""; text; ])
    | (None, Some html) -> String.concat "\r\n" (content_headers "text/html" @ [ ""; html; ])
    | (None, None) -> ""

  let render = fun message ->
    let from_header =
      match message.from with
      | Some from -> header "From" from
      | None -> None
    in
    let base_headers = [
      Some ("Date: " ^ date_header ());
      header "Subject" message.subject;
      from_header;
      header_list "To" message.to_;
      header_list "Cc" message.cc;
      header_list "Reply-To" message.reply_to;
    ]
    in
    let custom_headers =
      List.filter_map message.headers ~fn:(fun (name, value) -> header name value)
    in
    let headers = List.filter_map base_headers ~fn:(fun value -> value) @ custom_headers in
    String.concat "\r\n" (headers @ [ render_body message; ])
end

module Delivery = struct
  type invalid_message =
    | MissingRecipient
    | MissingSubject
    | MissingBody

  type filesystem_error =
    | CreateOutboxDirectoryFailed of {
        path: Path.t;
        reason: string;
      }
    | WriteMessageFailed of {
        path: Path.t;
        reason: string;
      }

  type adapter_error =
    | MailboxNotStarted
    | MailboxTimeout

  type error =
    | InvalidMessage of invalid_message
    | FilesystemError of filesystem_error
    | AdapterError of adapter_error

  type t = {
    name: string;
    send: Message.t -> (unit, error) result;
  }

  let error_to_string = fun error ->
    match error with
    | InvalidMessage MissingRecipient -> "invalid email message: at least one recipient is required"
    | InvalidMessage MissingSubject -> "invalid email message: subject is required"
    | InvalidMessage MissingBody -> "invalid email message: text or html body is required"
    | FilesystemError (CreateOutboxDirectoryFailed { path; reason }) ->
        "email filesystem error: failed to create outbox directory "
        ^ Path.to_string path
        ^ ": "
        ^ reason
    | FilesystemError (WriteMessageFailed { path; reason }) ->
        "email filesystem error: failed to write message " ^ Path.to_string path ^ ": " ^ reason
    | AdapterError MailboxNotStarted -> "email adapter error: mailer mailbox is not started"
    | AdapterError MailboxTimeout -> "email adapter error: mailer mailbox did not respond"

  let adapter = fun ~name send -> { name; send }

  let name = fun delivery -> delivery.name

  let validate = fun message ->
    if List.is_empty (Message.recipients message) then
      Error (InvalidMessage MissingRecipient)
    else if String.is_empty (String.trim message.subject) then
      Error (InvalidMessage MissingSubject)
    else
      match (Message.(message.text_body), Message.(message.html_body)) with
      | (None, None) -> Error (InvalidMessage MissingBody)
      | _ -> Ok ()

  let next_outbox_counter =
    let counter = ref 0 in
    fun () ->
      counter := !counter + 1;
      !counter

  let slug_char = fun char ->
    if (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9') then
      Some (Char.lowercase_ascii char)
    else if char = '-' || char = '_' then
      Some char
    else
      None

  let slugify = fun value ->
    let buffer = IO.Buffer.create ~size:(String.length value) in
    String.iter
      (fun char ->
        match slug_char char with
        | Some safe -> IO.Buffer.add_char buffer safe
        | None ->
            if IO.Buffer.length buffer > 0 then
              IO.Buffer.add_char buffer '-')
      value;
    let raw = IO.Buffer.contents buffer in
    let trimmed = String.trim raw in
    if String.is_empty trimmed then
      "email"
    else
      trimmed

  let outbox_filename = fun message ->
    let timestamp =
      Time.SystemTime.now ()
      |> Time.SystemTime.to_unix_timestamp
      |> Int.to_string
    in
    timestamp
    ^ "-"
    ^ Int.to_string (next_outbox_counter ())
    ^ "-"
    ^ slugify Message.(message.subject)
    ^ ".eml"

  let deliver_to_outbox = fun ~dir message ->
    let* () = validate message in
    let outbox_dir = Path.v dir in
    let path = Path.join outbox_dir (Path.v (outbox_filename message)) in
    let rendered = Message.render message in
    let* () =
      match Fs.create_dir_all outbox_dir with
      | Ok () -> Ok ()
      | Error error ->
          Error (FilesystemError (CreateOutboxDirectoryFailed {
            path = outbox_dir;
            reason = IO.error_message error;
          }))
    in
    match Fs.write rendered path with
    | Ok () -> Ok path
    | Error error ->
        Error (FilesystemError (WriteMessageFailed { path; reason = IO.error_message error }))

  let outbox = fun ~dir () ->
    adapter
      ~name:"outbox"
      (fun message ->
        match deliver_to_outbox ~dir message with
        | Ok _path -> Ok ()
        | Error error -> Error error)

  let deliver_now = fun delivery message ->
    let* () = validate message in
    delivery.send message
end

module Mailbox = struct
  type delivered = {
    id: int;
    delivered_at: string;
    message: Message.t;
    rendered: string;
  }

  type error =
    | NotStarted
    | Timeout
    | StartError of { reason: string }

  type t = {
    pid_ref: Pid.t option Sync.Cell.t;
    request_timeout: Time.Duration.t;
  }

  type state = {
    next_id: int;
    max_messages: int option;
    deliveries: delivered list;
  }

  type command =
    | DeliverCommand of {
        ref_: int;
        reply_to: Pid.t;
        message: Message.t;
      }
    | ListCommand of {
        ref_: int;
        reply_to: Pid.t;
      }
    | GetCommand of {
        ref_: int;
        reply_to: Pid.t;
        id: int;
      }
    | ClearCommand of {
        ref_: int;
        reply_to: Pid.t;
      }

  type Std.Message.t +=
    | MailboxDeliver of {
        ref_: int;
        reply_to: Pid.t;
        message: Message.t;
      }
    | MailboxDeliverResult of {
        ref_: int;
        result: (unit, Delivery.error) result;
      }
    | MailboxList of {
        ref_: int;
        reply_to: Pid.t;
      }
    | MailboxListResult of {
        ref_: int;
        deliveries: delivered list;
      }
    | MailboxGet of {
        ref_: int;
        reply_to: Pid.t;
        id: int;
      }
    | MailboxGetResult of {
        ref_: int;
        delivered: delivered option;
      }
    | MailboxClear of {
        ref_: int;
        reply_to: Pid.t;
      }
    | MailboxClearResult of { ref_: int }

  let error_to_string = fun error ->
    match error with
    | NotStarted -> "mailer mailbox is not started"
    | Timeout -> "mailer mailbox did not respond"
    | StartError { reason } -> "mailer mailbox failed to start: " ^ reason

  let next_request_ref =
    let counter = ref 0 in
    fun () ->
      counter := !counter + 1;
      !counter

  let take = fun limit values ->
    let rec go acc remaining values =
      match (remaining, values) with
      | (0, _) -> List.rev acc
      | (_, []) -> List.rev acc
      | (_, value :: rest) -> go (value :: acc) (remaining - 1) rest
    in
    if limit <= 0 then
      []
    else
      go [] limit values

  let clamp_deliveries = fun max_messages deliveries ->
    match max_messages with
    | None -> deliveries
    | Some limit -> take limit deliveries

  let command_selector = fun message ->
    match message with
    | MailboxDeliver { ref_; reply_to; message } ->
        Select (DeliverCommand { ref_; reply_to; message })
    | MailboxList { ref_; reply_to } -> Select (ListCommand { ref_; reply_to })
    | MailboxGet { ref_; reply_to; id } -> Select (GetCommand { ref_; reply_to; id })
    | MailboxClear { ref_; reply_to } -> Select (ClearCommand { ref_; reply_to })
    | _ -> Skip

  let deliver_to_state = fun state message ->
    let* () = Delivery.validate message in
    let delivered = {
      id = state.next_id;
      delivered_at = Message.date_header ();
      message;
      rendered = Message.render message;
    }
    in
    let deliveries =
      delivered :: state.deliveries
      |> clamp_deliveries state.max_messages
    in
    Ok ({ state with next_id = state.next_id + 1; deliveries }, ())

  let rec loop: state -> (unit, exn) result = fun state ->
    match receive ~selector:command_selector () with
    | DeliverCommand { ref_; reply_to; message } -> (
        match deliver_to_state state message with
        | Ok (state, ()) ->
            send reply_to (MailboxDeliverResult { ref_; result = Ok () });
            loop state
        | Error error ->
            send reply_to (MailboxDeliverResult { ref_; result = Error error });
            loop state
      )
    | ListCommand { ref_; reply_to } ->
        send reply_to (MailboxListResult { ref_; deliveries = state.deliveries });
        loop state
    | GetCommand { ref_; reply_to; id } ->
        let delivered = List.find state.deliveries ~fn:(fun value -> value.id = id) in
        send reply_to (MailboxGetResult { ref_; delivered });
        loop state
    | ClearCommand { ref_; reply_to } ->
        send reply_to (MailboxClearResult { ref_ });
        loop { state with deliveries = [] }

  let init = fun ?max_messages () -> loop { next_id = 1; max_messages; deliveries = [] }

  let start_under = fun
    supervisor ?max_messages ?(request_timeout = Time.Duration.from_secs 2) () ->
    let pid_ref = ref None in
    let start = fun () ->
      let pid = spawn_link (fun () -> init ?max_messages ()) in
      pid_ref := Some pid;
      pid
    in
    match Std.Supervisor.Dynamic.start_child
      supervisor
      ~start
      ~restart:Permanent
      ~shutdown:(Timeout (Time.Duration.from_secs 5))
      () with
    | Ok pid ->
        pid_ref := Some pid;
        Ok { pid_ref; request_timeout }
    | Error reason -> Error (StartError { reason })

  let pid = fun mailbox -> !(mailbox.pid_ref)

  let deliver = fun mailbox message ->
    match pid mailbox with
    | None -> Error (Delivery.AdapterError Delivery.MailboxNotStarted)
    | Some pid ->
        let ref_ = next_request_ref () in
        send pid (MailboxDeliver { ref_; reply_to = self (); message });
        let selector = fun message ->
          match message with
          | MailboxDeliverResult { ref_ = candidate; result } when candidate = ref_ -> Select result
          | _ -> Skip
        in
        try receive ~selector ~timeout:mailbox.request_timeout () with
        | Receive_timeout -> Error (Delivery.AdapterError Delivery.MailboxTimeout)

  let list = fun mailbox ->
    match pid mailbox with
    | None -> Error NotStarted
    | Some pid ->
        let ref_ = next_request_ref () in
        send pid (MailboxList { ref_; reply_to = self () });
        let selector = fun message ->
          match message with
          | MailboxListResult { ref_ = candidate; deliveries } when candidate = ref_ ->
              Select deliveries
          | _ -> Skip
        in
        try Ok (receive ~selector ~timeout:mailbox.request_timeout ()) with
        | Receive_timeout -> Error Timeout

  let get = fun mailbox ~id ->
    match pid mailbox with
    | None -> Error NotStarted
    | Some pid ->
        let ref_ = next_request_ref () in
        send pid (MailboxGet { ref_; reply_to = self (); id });
        let selector = fun message ->
          match message with
          | MailboxGetResult { ref_ = candidate; delivered } when candidate = ref_ ->
              Select delivered
          | _ -> Skip
        in
        try Ok (receive ~selector ~timeout:mailbox.request_timeout ()) with
        | Receive_timeout -> Error Timeout

  let clear = fun mailbox ->
    match pid mailbox with
    | None -> Error NotStarted
    | Some pid ->
        let ref_ = next_request_ref () in
        send pid (MailboxClear { ref_; reply_to = self () });
        let selector = fun message ->
          match message with
          | MailboxClearResult { ref_ = candidate } when candidate = ref_ -> Select ()
          | _ -> Skip
        in
        try Ok (receive ~selector ~timeout:mailbox.request_timeout ()) with
        | Receive_timeout -> Error Timeout

  let delivery = fun mailbox -> Delivery.adapter ~name:"mailbox" (deliver mailbox)
end

module TestDelivery = struct
  type t = {
    mutable messages: Message.t list;
  }

  let create_store = fun () -> { messages = [] }

  let delivery = fun store ->
    Delivery.adapter
      ~name:"test"
      (fun message ->
        store.messages <- message :: store.messages;
        Ok ())

  let deliveries = fun store -> List.rev store.messages

  let clear = fun store -> store.messages <- []
end

module Routes = struct
  module Json = Data.Json

  let html_escape = fun value ->
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

  let html_page = fun ~title body ->
    "<!doctype html><html><head><meta charset=\"utf-8\"><title>"
    ^ html_escape title
    ^ "</title><style>body{font-family:system-ui,sans-serif;margin:2rem;line-height:1.45}"
    ^ "table{border-collapse:collapse;width:100%}td,th{border-bottom:1px solid #ddd;"
    ^ "padding:.5rem;text-align:left;vertical-align:top}pre{white-space:pre-wrap;"
    ^ "border:1px solid #ddd;padding:1rem;overflow:auto}button{padding:.45rem .7rem}"
    ^ "</style></head><body>"
    ^ body
    ^ "</body></html>"

  let trim_trailing_slash = fun path ->
    if String.length path > 1 && String.ends_with ~suffix:"/" path then
      String.sub path ~offset:0 ~len:(String.length path - 1)
    else
      path

  let mounted_path = fun conn ~suffix ->
    let path =
      Suri.Conn.path conn
      |> trim_trailing_slash
    in
    if not (String.is_empty suffix) && String.ends_with ~suffix path then
      String.sub path ~offset:0 ~len:(String.length path - String.length suffix)
      |> trim_trailing_slash
    else
      path

  let json_string_option = fun value ->
    match value with
    | Some value -> Json.string value
    | None -> Json.null

  let json_string_list = fun values -> Json.array (List.map values ~fn:Json.string)

  let header_json = fun (name, value) ->
    Json.obj
      [ ("name", Json.string name); ("value", Json.string value); ]

  let message_json = fun ?(include_bodies = false) ?(include_rendered = false) delivered ->
    let message = Mailbox.(delivered.message) in
    let fields = [
      ("id", Json.int Mailbox.(delivered.id));
      ("delivered_at", Json.string Mailbox.(delivered.delivered_at));
      ("subject", Json.string Message.(message.subject));
      ("from", json_string_option Message.(message.from));
      ("to", json_string_list Message.(message.to_));
      ("cc", json_string_list Message.(message.cc));
      ("bcc", json_string_list Message.(message.bcc));
      ("reply_to", json_string_list Message.(message.reply_to));
      ("headers", Json.array (List.map Message.(message.headers) ~fn:header_json));
    ]
    in
    let fields =
      if include_bodies then
        fields
        @ [
          ("text_body", json_string_option Message.(message.text_body));
          ("html_body", json_string_option Message.(message.html_body));
        ]
      else
        fields
    in
    let fields =
      if include_rendered then
        fields @ [ ("rendered", Json.string Mailbox.(delivered.rendered)); ]
      else
        fields
    in
    Json.obj fields

  let messages_json = fun deliveries ->
    Json.obj
      [
        ("count", Json.int (List.length deliveries));
        ("messages", Json.array (List.map deliveries ~fn:message_json));
      ]

  let mailbox_error = fun conn error ->
    Suri.Conn.render_text
      Net.Http.Status.InternalServerError
      (Mailbox.error_to_string error)
      conn

  let invalid_id = fun conn ->
    Suri.Conn.render_text
      Net.Http.Status.BadRequest
      "invalid message id"
      conn

  let not_found = fun conn ->
    Suri.Conn.render_text
      Net.Http.Status.NotFound
      "email message not found"
      conn

  let int_param = fun conn name ->
    match List.find (Suri.Conn.params conn) ~fn:(fun (key, _value) -> key = name) with
    | None -> None
    | Some (_key, value) -> Int.from_string_opt value

  let index_rows = fun mount_path deliveries ->
    deliveries
    |> List.map
      ~fn:(fun delivered ->
        let message = Mailbox.(delivered.message) in
        "<tr><td><a href=\""
        ^ mount_path
        ^ "/messages/"
        ^ Int.to_string Mailbox.(delivered.id)
        ^ "\">#"
        ^ Int.to_string Mailbox.(delivered.id)
        ^ "</a></td><td>"
        ^ html_escape Mailbox.(delivered.delivered_at)
        ^ "</td><td>"
        ^ html_escape Message.(message.subject)
        ^ "</td><td>"
        ^ html_escape (String.concat ", " Message.(message.to_))
        ^ "</td></tr>")
    |> String.concat ""

  let index_html = fun mount_path deliveries ->
    let rows =
      match deliveries with
      | [] -> "<tr><td colspan=\"4\">No email has been delivered.</td></tr>"
      | _ -> index_rows mount_path deliveries
    in
    html_page
      ~title:"Suri Mailer"
      ("<h1>Suri Mailer</h1><form method=\"post\" action=\""
      ^ mount_path
      ^ "/clear\"><button type=\"submit\">Clear</button></form>"
      ^ "<table><thead><tr><th>ID</th><th>Delivered</th><th>Subject</th><th>To</th>"
      ^ "</tr></thead><tbody>"
      ^ rows
      ^ "</tbody></table>")

  let detail_html = fun mount_path delivered ->
    let message = Mailbox.(delivered.message) in
    html_page
      ~title:("Email #" ^ Int.to_string Mailbox.(delivered.id))
      ("<p><a href=\""
      ^ mount_path
      ^ "\">Inbox</a> | <a href=\""
      ^ mount_path
      ^ "/messages/"
      ^ Int.to_string Mailbox.(delivered.id)
      ^ "/raw\">Raw</a></p><h1>"
      ^ html_escape Message.(message.subject)
      ^ "</h1><dl><dt>Delivered</dt><dd>"
      ^ html_escape Mailbox.(delivered.delivered_at)
      ^ "</dd><dt>To</dt><dd>"
      ^ html_escape (String.concat ", " Message.(message.to_))
      ^ "</dd></dl><pre>"
      ^ html_escape Mailbox.(delivered.rendered)
      ^ "</pre>")

  let list_html = fun mailbox conn _req ->
    match Mailbox.list mailbox with
    | Error error -> mailbox_error conn error
    | Ok deliveries ->
        let mount_path = mounted_path conn ~suffix:"" in
        Suri.Conn.render_text
          ~headers:[ ("Content-Type", "text/html; charset=utf-8"); ]
          Net.Http.Status.Ok
          (index_html mount_path deliveries)
          conn

  let list_json = fun mailbox conn _req ->
    match Mailbox.list mailbox with
    | Error error -> mailbox_error conn error
    | Ok deliveries -> Suri.Conn.render_json Net.Http.Status.Ok (messages_json deliveries) conn

  let show_json = fun mailbox conn _req ->
    match int_param conn "id" with
    | None -> invalid_id conn
    | Some id -> (
        match Mailbox.get mailbox ~id with
        | Error error -> mailbox_error conn error
        | Ok None -> not_found conn
        | Ok (Some delivered) ->
            Suri.Conn.render_json
              Net.Http.Status.Ok
              (message_json ~include_bodies:true ~include_rendered:true delivered)
              conn
      )

  let show_html = fun mailbox conn _req ->
    match int_param conn "id" with
    | None -> invalid_id conn
    | Some id -> (
        match Mailbox.get mailbox ~id with
        | Error error -> mailbox_error conn error
        | Ok None -> not_found conn
        | Ok (Some delivered) ->
            let mount_path = mounted_path conn ~suffix:("/messages/" ^ Int.to_string id) in
            Suri.Conn.render_text
              ~headers:[ ("Content-Type", "text/html; charset=utf-8"); ]
              Net.Http.Status.Ok
              (detail_html mount_path delivered)
              conn
      )

  let show_raw = fun mailbox conn _req ->
    match int_param conn "id" with
    | None -> invalid_id conn
    | Some id -> (
        match Mailbox.get mailbox ~id with
        | Error error -> mailbox_error conn error
        | Ok None -> not_found conn
        | Ok (Some delivered) ->
            conn
            |> Suri.Conn.respond ~status:Net.Http.Status.Ok ~body:Mailbox.(delivered.rendered)
            |> Suri.Conn.set_header "Content-Type" "message/rfc822; charset=utf-8"
            |> Suri.Conn.send
      )

  let clear_json = fun mailbox conn _req ->
    match Mailbox.clear mailbox with
    | Error error -> mailbox_error conn error
    | Ok () ->
        Suri.Conn.render_json Net.Http.Status.Ok (Json.obj [ ("cleared", Json.bool true); ]) conn

  let clear_and_redirect = fun mailbox conn _req ->
    match Mailbox.clear mailbox with
    | Error error -> mailbox_error conn error
    | Ok () -> Suri.Conn.redirect (mounted_path conn ~suffix:"/clear") conn

  let routes = fun mailbox ->
    Suri.Middleware.Router.[
      get "" (list_html mailbox);
      scope
        "/messages"
        [
          get "" (list_json mailbox);
          delete "" (clear_json mailbox);
          scope
            "/:id"
            [
              get "" (show_html mailbox);
              get "/json" (show_json mailbox);
              get "/raw" (show_raw mailbox);
            ];
        ];
      post "/clear" (clear_and_redirect mailbox);
    ]
end

module MessageDelivery = struct
  type t = {
    message: Message.t;
    delivery: Delivery.t;
  }

  let make = fun ~delivery message -> { message; delivery }

  let message = fun delivery -> delivery.message

  let deliver_now = fun delivery -> Delivery.deliver_now delivery.delivery delivery.message
end

module Mailer = struct
  type t = {
    default_from: string option;
    delivery: Delivery.t;
  }

  let make = fun ?default_from ~delivery () -> { default_from; delivery }

  let mail = fun mailer ?from ?cc ?bcc ?reply_to ?headers ?text ?html ~to_ ~subject () ->
    let from =
      match from with
      | Some value -> Some value
      | None -> mailer.default_from
    in
    Message.make ?from ?cc ?bcc ?reply_to ?headers ?text ?html ~to_ ~subject ()
    |> MessageDelivery.make ~delivery:mailer.delivery
end

module Supervisor = struct
  type t = {
    supervisor: Std.Supervisor.Dynamic.t;
    mailbox: Mailbox.t;
  }

  type start_error = Mailbox.error

  let start_error_to_string = Mailbox.error_to_string

  let start_link = fun ?max_messages ?request_timeout () ->
    let supervisor =
      Std.Supervisor.Dynamic.start_link
        ~intensity:{ max_restarts = 3; window = Time.Duration.from_secs 5 }
        ~max_children:1
        ()
    in
    match Mailbox.start_under supervisor ?max_messages ?request_timeout () with
    | Ok mailbox -> Ok { supervisor; mailbox }
    | Error error -> Error error

  let dynamic_supervisor = fun t -> t.supervisor

  let mailbox = fun t -> t.mailbox

  let delivery = fun t -> Mailbox.delivery t.mailbox

  let mailer = fun ?default_from t -> Mailer.make ?default_from ~delivery:(delivery t) ()

  let routes = fun t -> Routes.routes t.mailbox
end

let routes = Supervisor.routes

let deliver_now = Delivery.deliver_now
