(** Standard library extensions and utilities *)
module Agent = Agent
module Actor = Actor
module Application = Application
module Archive = Archive
module ArgParser = Arg_parser
module Array = Collections.Array
module Bench = Bench
module Bool = Bool
module Calendar = Calendar
module Char = Char
module Collections = Collections
module Command = Command
module Compress = Compress
module Config = Config
module Crypto = Crypto
module Data = Data
module Date = Date
module DateTime = DateTime
module Diff = Diff
module Env = Env
module Exception = Exception
module Encoding = Encoding
module Float = Float
module Fs = Fs
module Glob = Glob
module Graph = Graph
module IO = IO
module Int = Int
module Int32 = Int32
module Int64 = Int64
module Iter = Iter
module List = Collections.List
module Log = Log
module Message = Message
module Net = Net
module Option = Option
module Order = Order
module Path = Path
module Pid = Pid
module Process = Process
module Ptr = Ptr
module Random = Random
module Range = Range
module Regex = Regex
module Ref = Ref
module Result = Result
module Runtime = Runtime
module String = String
module StringBuilder = StringBuilder
module Supervisor = Supervisor
module Sync = Sync
module System = System
module Task = Task
module Telemetry = Telemetry
module Test = Test
module Thread = Thread
module Time = Time
module Timer = Timer
module Type = Type
module UUID = Uuid
module Unicode = Unicode
module Version = Version
module WorkerPool = Worker_pool

type 'a vec = 'a Collections.Vector.t

type 'a queue = 'a Collections.Queue.t

type 'a set = 'a Collections.HashSet.t

type ('k, 'v) map = ('k, 'v) Collections.HashMap.t

let vec = Collections.Vector.from_list

let queue = Collections.Queue.from_list

let set = Collections.HashSet.from_list

let map = Collections.HashMap.from_list

(* Include std's Global module which re-exports from Kernel *)

include Global

(* Application startup *)

let start = fun ~apps ->
  let config = Runtime.Config.default in
  let main ~args:_ =
    match Application.start_applications apps with
    | Ok _app_pids ->
        (* Keep system running indefinitely *)
        let rec keep_alive () =
          sleep (Time.Duration.from_secs 100_000);
          keep_alive ()
        in
        let () = keep_alive () in
        Ok ()
    | Error e -> Error e
  in
  Runtime.run ~config ~main ~args:[] ()
