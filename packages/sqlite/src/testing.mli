open Std

val with_db:
  Sqlite__Config.t ->
  (Sqlite__Driver.connection -> ('value, string) result) ->
  ('value, string) result
