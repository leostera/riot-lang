include Kernel.IO.IoVec

let error_message = Kernel.IO.Error.message

module IoSlice = struct
  include Kernel.IO.IoVec.IoSlice

  let error_message = Kernel.IO.Error.message
end
