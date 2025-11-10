open Std
open Std.Collections

type Message.t +=
  | FileEvent of Fs.FileWatcher.event
