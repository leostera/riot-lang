module type Intf = sig
  val init : int -> unit
  val on_result : int -> Test_result.t -> unit
  val finalize : Test_result.summary -> unit
end
