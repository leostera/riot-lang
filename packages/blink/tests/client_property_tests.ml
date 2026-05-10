open Std

module H = Blink.Client

let expected_status_class = fun status ->
  if status = 429 then
    H.Response.RateLimited
  else if status >= 100 && status < 200 then
    H.Response.Informational
  else if status >= 200 && status < 300 then
    H.Response.Success
  else if status >= 300 && status < 400 then
    H.Response.Redirect
  else if status >= 400 && status < 500 then
    H.Response.ClientError
  else if status >= 500 && status < 600 then
    H.Response.ServerError
  else
    H.Response.UnknownStatus

let property_status_classes_are_range_based = fun _ctx ->
  for status = (-20) to 620 do
    Test.assert_equal
      ~expected:(expected_status_class status)
      ~actual:(H.Response.status_class status)
  done;
  Ok ()

let property_budget_allows_capacity_per_window = fun _ctx ->
  let now = Time.Instant.now () in
  let window = Time.Duration.from_secs 10 in
  for capacity = 0 to 12 do
    let budget = H.Budget.create ~capacity ~window now in
    for _request = 1 to capacity do
      Test.assert_true (H.Budget.allow ~now budget)
    done;
    Test.assert_false (H.Budget.allow ~now budget);
    let reset_at = Time.Instant.add now window in
    for _request = 1 to capacity do
      Test.assert_true (H.Budget.allow ~now:reset_at budget)
    done;
    Test.assert_false (H.Budget.allow ~now:reset_at budget)
  done;
  Ok ()

let property_request_descriptions_include_method_and_url = fun _ctx ->
  let url = "https://example.test/resource" in
  let methods = [
    (H.Request.Get, "GET");
    (H.Request.Post, "POST");
    (H.Request.Put, "PUT");
    (H.Request.Patch, "PATCH");
    (H.Request.Delete, "DELETE");
  ]
  in
  List.for_each
    methods
    ~fn:(fun (method_, method_text) ->
      let request = H.Request.make ~method_ ~url () in
      Test.assert_equal ~expected:(method_text ^ " " ^ url) ~actual:(H.Request.describe request));
  Ok ()

let tests =
  Test.[
    case "property: status classes are range based" property_status_classes_are_range_based;
    case "property: budget allows capacity per window" property_budget_allows_capacity_per_window;
    case
      "property: request descriptions include method and url"
      property_request_descriptions_include_method_and_url;
  ]

let main ~args = Test.Cli.main ~name:"blink_client_property_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
