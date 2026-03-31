module type S (* eq *) = sig
  val x : int
end

val y (* colon *) : int

external z (* colon2 *) : int (* eq2 *) = "z"
