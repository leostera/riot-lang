type _ Effect.t +=
  | Receive: {
      selector:
        Message.t -> [
          `select of 'msg
          | `skip
        ];
    } -> 'msg Effect.t

type _ Effect.t +=
  Yield: unit Effect.t
