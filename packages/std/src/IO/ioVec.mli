include module type of Kernel.IO.IoVec

val error_message: error -> string

module IoSlice: sig
  include module type of Kernel.IO.IoVec.IoSlice

  val error_message: error -> string
end
