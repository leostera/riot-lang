open Std

val run:
  concurrency:int ->
  tasks:'task list ->
  fn:('task -> (('result * 'task list), 'error) result) ->
  ('task * ('result, 'error) result) list
