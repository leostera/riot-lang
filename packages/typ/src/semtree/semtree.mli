type t

val lower : source:Model.Source.t -> Syn.Cst.source_file -> t

val serializer : t Serde.Ser.t
