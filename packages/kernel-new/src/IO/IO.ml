module Iovec = Iovec

type error = Error.t

type 'value io_result = ('value, error) Result.t
