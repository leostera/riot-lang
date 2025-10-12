open Std

(** {1 ARM64 Instructions}

    ARM64 assembly instruction set for code generation. *)

type register =
  | X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | SP
  | LR
  | XZR

val register_to_string : register -> string

type operand = Reg of register | Imm of int | Label of string

val operand_to_string : operand -> string

type address =
  | RegOffset of register * int
  | RegReg of register * register
  | PreIndex of register * int
  | PostIndex of register * int

val address_to_string : address -> string

type instruction =
  (* Data movement *)
  | MOV of register * operand
  | MOVK of register * int * int
  | LDR of register * address
  | STR of register * address
  | STP of register * register * address
  | LDP of register * register * address
  (* Arithmetic *)
  | ADD of register * register * operand
  | SUB of register * register * operand
  | MUL of register * register * register
  | SDIV of register * register * register
  | NEG of register * register
  (* Comparisons *)
  | CMP of register * operand
  (* Control flow *)
  | B of string
  | BL of string
  | BR of register
  | BLR of register
  | RET
  (* Conditional branches *)
  | BEQ of string
  | BNE of string
  | BLT of string
  | BLE of string
  | BGT of string
  | BGE of string
  (* Logical *)
  | AND of register * register * operand
  | ORR of register * register * operand
  | EOR of register * register * operand
  | MVN of register * register
  (* Pseudo *)
  | LABEL of string
  | DIRECTIVE of string
  | COMMENT of string

val instruction_to_string : instruction -> string
(** Convert instruction to ARM64 assembly syntax. *)

val emit_prologue : stack_size:int -> instruction list
(** Generate function prologue (saves FP/LR, allocates stack). *)

val emit_epilogue : stack_size:int -> instruction list
(** Generate function epilogue (restores stack, returns). *)
