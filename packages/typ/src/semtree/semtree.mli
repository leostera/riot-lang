type t
val lower: source:Model.Source.t -> Syn.Parser.parse_result -> t

val serializer: t Serde.Ser.t
