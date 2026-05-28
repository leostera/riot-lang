use App.Core.{make_user, User}
use App.Core.*

type Maybe<'a> =
  | None
  | Some('a)

type User = User { name: string, _id: u32, active: bool }

let _value = 1

let make_user (name: string) = User { name: name, _id: 1, active: true }

let read_id (User { _id }) = _id
