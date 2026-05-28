type Msg<'a> =
  | Ping
  | User { name: string, value: 'a }
