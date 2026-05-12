type live_set = string Std.Collections.HashSet.t
val empty: unit -> live_set

val mem: live_set -> string -> bool

val add: live_set -> string -> live_set

val union: live_set -> live_set -> live_set

val remove: live_set -> string -> live_set

val from_operand: Types.Operand.t -> live_set

val from_callee: Types.Callee.t -> live_set

val from_operands: Types.Operand.t list -> live_set

val before_instruction: after:live_set -> Types.Instruction.t -> live_set

val before_instructions: after:live_set -> Types.Instruction.t list -> live_set
