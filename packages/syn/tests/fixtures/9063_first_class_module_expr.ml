let driver =
  (module Sqlite.Driver)

let constrained =
  (module M : S)
