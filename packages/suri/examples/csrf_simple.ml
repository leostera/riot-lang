open Std
open Suri

(** Simple CSRF protection demo with form submission *)
let form_page = fun conn ->
  let open Component in
  match (Middleware.Csrf.meta_tag conn, Middleware.Csrf.hidden_field conn) with
  | (Error error, _)
  | (_, Error error) -> Error error
  | (Ok csrf_meta, Ok csrf_field) ->
      let page =
        html
          [
            head [ title [ text "CSRF Protection Demo" ]; csrf_meta ];
            body
              [
                h1 [ text "CSRF Protection Demo" ];
                h2 [ text "HTML Form" ];
                form
                  ~attrs:[ method_ "POST"; action "/submit" ]
                  [
                    csrf_field;
                    p [ label [ text "Name: " ]; input ~attrs:[ type_ "text"; name "name" ] () ];
                    button ~attrs:[ type_ "submit" ] [ text "Submit Form" ];
                  ];
                h2 [ text "AJAX Form" ];
                p
                  [
                    input ~attrs:[ type_ "text"; id "ajax-name"; placeholder "Name" ] ();
                    text " ";
                    button ~attrs:[ id "ajax-submit" ] [ text "Submit via AJAX" ];
                  ];
                div ~attrs:[ id "result" ] [];
                script
                  {|
document.getElementById('ajax-submit').addEventListener('click', function() {
  const name = document.getElementById('ajax-name').value;
  const token = document.querySelector('meta[name="csrf-token"]').content;
  
  fetch('/submit-ajax', {
    method: 'POST',
    headers: {
      'X-CSRF-Token': token,
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({ name: name })
  })
  .then(r => r.text())
  .then(result => {
    document.getElementById('result').innerHTML = '<p>' + result + '</p>';
  })
  .catch(err => {
    document.getElementById('result').innerHTML = '<p>Error: ' + err + '</p>';
  });
});
|};
              ];
          ]
      in
      Ok (Component.to_html page)

let submit_handler = fun conn req ->
  (* Get form data from body_params (parsed by body_parser middleware) *)
  let params = Conn.body_params conn in
  let user_name =
    match Std.Collections.Proplist.get params ~key:"name" with
    | Option.Some n -> n
    | Option.None -> "Anonymous"
  in
  let open Component in
  let page =
    html
      [
        body
          [
            h1 [ text "Form Submitted!" ];
            p [ text "Welcome, "; strong [ text user_name ]; text "!" ];
            p [ a ~attrs:[ href "/" ] [ text "Back to form" ] ];
          ];
      ]
  in
  conn
  |> Conn.respond ~status:Ok ~body:(Component.to_html page)
  |> Conn.with_header "content-type" "text/html"
  |> Conn.send

let submit_ajax_handler = fun conn req ->
  (* Get JSON data from body_params (parsed by body_parser middleware) *)
  let params = Conn.body_params conn in
  let user_name =
    match Std.Collections.Proplist.get params ~key:"name" with
    | Option.Some n -> n
    | Option.None -> "Anonymous"
  in
  let message = "AJAX request received successfully! Welcome, " ^ user_name ^ "!" in
  conn
  |> Conn.respond ~status:Ok ~body:message
  |> Conn.send

let routes =
  Middleware.Router.[
    get
      "/"
      (fun conn req ->
        match form_page conn with
        | Ok html ->
            conn
            |> Conn.respond ~status:Ok ~body:html
            |> Conn.with_header "content-type" "text/html"
            |> Conn.send
        | Error error ->
            conn
            |> Conn.respond
              ~status:Net.Http.Status.InternalServerError
              ~body:(Middleware.Csrf.error_to_string error)
            |> Conn.send);
    post "/submit" submit_handler;
    post "/submit-ajax" submit_ajax_handler;
  ]

let main ~args:_ =
  let secret = "dev-secret-not-for-production-use-32bit" in
  match Middleware.session ~secret () with
  | Error error -> Error (Failure (Middleware.Session.setup_error_to_string error))
  | Ok session_middleware ->
      let app =
        Middleware.[
          request_id;
          logger;
          session_middleware;
          body_parser ();
          csrf ();
          router routes;
        ]
      in
      match Suri.config ~port:4_000 () with
      | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
      | Ok config ->
          match Suri.start_link ~config app with
          | Ok _supervisor ->
              Log.info "===========================================";
              Log.info "CSRF Protection Demo Running";
              Log.info "===========================================";
              Log.info "Server: http://localhost:4000";
              Log.info "";
              Log.info "Try these:";
              Log.info "1. Submit the form normally (works)";
              Log.info "2. Submit via AJAX (works)";
              Log.info "3. Try POST without token (gets 403)";
              Log.info "";
              Log.info "Test without token:";
              Log.info "  curl -X POST http://localhost:4000/submit";
              Log.info "";
              Log.info "===========================================";
              let rec loop () =
                sleep (Time.Duration.from_secs 100);
                loop ()
              in
              loop ()
          | Error error ->
              Log.error "Failed to bind to port 4000";
              Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
