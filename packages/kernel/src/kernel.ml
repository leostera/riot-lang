module Prelude = Prelude

include Prelude

external dangerously_cast_value: 'value -> 'cast = "%identity"

module Array = Array
module Atomic = Sync.Atomic
module Async = Async
module Bool = Bool
module Bytes = Bytes
module Char = Char
module Condition = Sync.Condition
module Effect = Effect
module Env = Env
module Error = Error
module Exception = Exception
module Float = Float
module Fs = Fs
module Gc = Gc
module IO = IO
module Int = Int
module Int32 = Int32
module Int64 = Int64
module List = List
module Mutex = Sync.Mutex
module Net = Net
module Option = Option
module Path = Path
module Process = Process
module Ptr = Ptr
module Random = Random
module Regex = Regex
module Result = Result
module Sync = Sync
module String = String
module System = System
module SystemError = System_error
module Thread = Thread
module Time = Time
module Unicode = Unicode

let panic = SystemError.panic
