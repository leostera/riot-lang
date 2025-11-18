open Std

module New : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module State : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module Load : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module Query : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module Get : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module Stats : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module Compact : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module Inspect : sig
  module Sstable : sig
    val command : ArgParser.command
    val run : ArgParser.matches -> (unit, exn) result
  end
  
  module Index : sig
    val command : ArgParser.command
    val run : ArgParser.matches -> (unit, exn) result
  end
end

module Search : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end

module Dump : sig
  val command : ArgParser.command
  val run : ArgParser.matches -> (unit, exn) result
end
