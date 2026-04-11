open Std

type error =
  | UnsupportedPhysicalRegisterHome of { name: string }
  | PhysicalRegisterExpected of { home: Lir.Home.t }
  | UnassignedVirtualRegister of { name: string }
  | UnassignedVirtualDestination of { name: string }
  | TooManyCallArguments of { provided: int; max_supported: int }
  | ArgumentNotPlaced of { index: int; expected_register: string; actual: Lir.Operand.t }
  | ReturnNotPlaced of { expected_register: string; actual: Lir.Operand.t option }
  | CallResultNotExplicit of { destination: Lir.Destination.t }
  | TooManyParameters of { provided: int; max_supported: int }
val error_to_json: error -> Std.Data.Json.t

val emit_program: Lir.Program.t -> (string, error) result
