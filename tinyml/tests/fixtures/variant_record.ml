type msg<'a> =
  | Ping
  | User { name: string, value: 'a }
