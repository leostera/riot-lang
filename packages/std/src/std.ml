(** Standard library extensions and utilities *)

module Dynlink = Kernel.Dynlink
module Agent = Agent
module Application = Application
module ArgParser = Arg_parser
module Bench = Bench
module Bool = Bool
module Char = Char
module Collections = Collections
module Command = Command
module Crypto = Crypto
module Data = Data
module Datetime = Datetime
module Diff = Diff
module Env = Env
module Exception = Exception
module Float = Float
module Fs = Fs
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
module Path = Path
module Pid = Pid
module Process = Process
module Ptr = Ptr
module Random = Random
module Ref = Ref
module Result = Result
module String = String
module Supervisor = Supervisor
module Sync = Sync
module System = System
module Task = Task
module Telemetry = Telemetry
module Test = Test
module Time = Time
module Timer = Timer
module Type = Type
module UUID = Uuid
module Unicode = Unicode
module Version = Version
module WorkerPool = Worker_pool

(* Include std's Global module which re-exports from Kernel *)
include Global

(* Application startup *)
let start ~apps =
  let config = Miniriot.Config.default in
  let main ~args:_ =
    match Application.start_applications apps with
    | Ok _app_pids ->
        (* Keep system running indefinitely *)
        let rec keep_alive () =
          Miniriot.receive_any () |> ignore;
          keep_alive ()
        in
        keep_alive ();
        Ok ()
    | Error e -> 
        Error e
  in
  Miniriot.run ~config ~main ~args:[] ()
