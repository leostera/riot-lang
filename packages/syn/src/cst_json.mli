open Std

val of_source_file : Cst.source_file -> Data.Json.t
val of_error : Cst_builder.error -> Data.Json.t
val of_result : (Cst.source_file, Cst_builder.error) result -> Data.Json.t
