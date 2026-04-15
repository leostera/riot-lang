open Std

type t = Build_session.BuildLock.t = {
  path: Path.t;
  file: Fs.File.t;
}

let retry_interval = Build_session.BuildLock.retry_interval

let path = Build_session.BuildLock.path

let release = Build_session.BuildLock.release

let wait = Build_session.BuildLock.wait

let acquire = Build_session.BuildLock.acquire
