val ( let* ):
  ('value, 'error) Result.t ->
  ('value -> ('next, 'error) Result.t) ->
  ('next, 'error) Result.t
