module Sstable : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Index : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end
