open Std

type node_result = {
  node: Work_node.t;
  status: Work_node.status;
  error: Error.t option;
}

type t = {
  results: node_result list;
  completed_count: int;
  failed_count: int;
}

let has_failures = fun summary -> not (Int.equal summary.failed_count 0)
