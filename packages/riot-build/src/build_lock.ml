open Std

type t = Client.BuildLock.t = {
  path: Path.t;
  file: Fs.File.t;
}

let retry_interval = Client.BuildLock.retry_interval

let path = Client.BuildLock.path

let release = Client.BuildLock.release

let wait = Client.BuildLock.wait

let acquire = Client.BuildLock.acquire
