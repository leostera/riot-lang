class c = object
  method (* priv *) private m = 1
  val (* mut *) mutable x = 0
end

class d = object
  method (* bang *) ! (* priv2 *) private m = 2
  val (* bangv *) ! (* mut2 *) mutable x = 1
  method (* virt *) virtual reset : int
  val (* virtv *) virtual state : int
end

class type t = object
  method (* privt *) private m : int
  val (* mutt *) mutable x : int
end
