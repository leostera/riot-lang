open Std

val run:
  concurrency:int ->
  tasks:'task list ->
  fn:('task -> ('result, 'error) result) ->
  on_result:(task:'task -> outcome:('result, 'error) result -> 'task list) ->
  ('task * ('result, 'error) result) list
