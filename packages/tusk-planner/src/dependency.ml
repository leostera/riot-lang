open Std
open Tusk_model

type t = { package : Package.t; library_path : Path.t }

let make ~package ~library_path = { package; library_path }
