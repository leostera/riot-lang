open Std

type register =
  | RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | RBP
  | RSP
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

val register_to_string : register -> string

type operand = Reg of register | Imm of int | Label of string

val operand_to_string : operand -> string

type address = RegOffset of register * int | RegReg of register * register

val address_to_string : address -> string

type instruction =
  | MOV of register * operand
  | LEA of register * address
  | PUSH of operand
  | POP of register
  | ADD of register * operand
  | SUB of register * operand
  | IMUL of register * operand
  | IDIV of register
  | NEG of register
  | CMP of register * operand
  | JMP of string
  | JE of string
  | JNE of string
  | JL of string
  | JLE of string
  | JG of string
  | JGE of string
  | CALL of string
  | RET
  | AND of register * operand
  | OR of register * operand
  | XOR of register * operand
  | NOT of register
  | LABEL of string
  | DIRECTIVE of string
  | COMMENT of string

val instruction_to_string : instruction -> string
val emit_prologue : stack_size:int -> instruction list
val emit_epilogue : stack_size:int -> instruction list
