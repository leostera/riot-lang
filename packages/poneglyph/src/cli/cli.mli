module New : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module State : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Load : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Query : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Get : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Stats : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Compact : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Inspect : sig
  module Sstable : sig
    val command : Std.ArgParser.command
    val run : Std.ArgParser.matches -> (unit, exn) Std.result
  end

  module Index : sig
    val command : Std.ArgParser.command
    val run : Std.ArgParser.matches -> (unit, exn) Std.result
  end
end

module Search : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end

module Dump : sig
  val command : Std.ArgParser.command
  val run : Std.ArgParser.matches -> (unit, exn) Std.result
end
