open Std

type t

type entry = {
  envelope_from : string Option.t;
  envelope_date : string Option.t;
  message : Message.t;
}

val of_file : Fs.File.t -> (t, string) Result.t
val into_mut_iter : t -> entry Iter.MutIterator.t
val parse_separator : string -> (string * string, string) Result.t
