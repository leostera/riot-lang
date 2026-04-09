module Iovec = Iovec

type error = System_error.t
type 'value io_result = ('value, error) Result.t
