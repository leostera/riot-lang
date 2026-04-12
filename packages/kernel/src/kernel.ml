include Prelude

module Array = Array
module Atomic = Atomic
module Async = Async
module Bool = Bool
module Bytes = Bytes
module Char = Char
module Condition = Condition
module Domain = Domain
module Effect = Effect
module Env = Env
module Error = Error
module Exception = Exception
module Float = Float
module Fs = Fs
module IO = Io
module Int = Int
module Int32 = Int32
module Int64 = Int64
module List = List
module Mutex = Mutex
module Net = Net
module Option = Option
module Path = Path
module Process = Process
module Ptr = Ptr
module Regex = Regex
module Result = Result
module String = String
module System = System
module SystemError = System_error
module Thread = Thread
module Time = Time
module Unicode = Unicode

let panic = SystemError.panic
