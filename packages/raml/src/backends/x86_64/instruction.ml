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

let register_to_string = function
  | RAX -> "%rax"
  | RBX -> "%rbx"
  | RCX -> "%rcx"
  | RDX -> "%rdx"
  | RSI -> "%rsi"
  | RDI -> "%rdi"
  | RBP -> "%rbp"
  | RSP -> "%rsp"
  | R8 -> "%r8"
  | R9 -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"
  | R13 -> "%r13"
  | R14 -> "%r14"
  | R15 -> "%r15"

type operand = Reg of register | Imm of int | Label of string

let operand_to_string = function
  | Reg r -> register_to_string r
  | Imm n -> format "$%d" n
  | Label l -> l

type address = RegOffset of register * int | RegReg of register * register

let address_to_string = function
  | RegOffset (r, 0) -> format "(%s)" (register_to_string r)
  | RegOffset (r, off) -> format "%d(%s)" off (register_to_string r)
  | RegReg (r1, r2) ->
      format "(%s,%s)" (register_to_string r1) (register_to_string r2)

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

let instruction_to_string = function
  | MOV (dst, src) ->
      format "    movq %s, %s" (operand_to_string src) (register_to_string dst)
  | LEA (dst, addr) ->
      format "    leaq %s, %s" (address_to_string addr) (register_to_string dst)
  | PUSH op -> format "    pushq %s" (operand_to_string op)
  | POP reg -> format "    popq %s" (register_to_string reg)
  | ADD (dst, src) ->
      format "    addq %s, %s" (operand_to_string src) (register_to_string dst)
  | SUB (dst, src) ->
      format "    subq %s, %s" (operand_to_string src) (register_to_string dst)
  | IMUL (dst, src) ->
      format "    imulq %s, %s" (operand_to_string src) (register_to_string dst)
  | IDIV reg -> format "    idivq %s" (register_to_string reg)
  | NEG reg -> format "    negq %s" (register_to_string reg)
  | CMP (dst, src) ->
      format "    cmpq %s, %s" (operand_to_string src) (register_to_string dst)
  | JMP label -> format "    jmp %s" label
  | JE label -> format "    je %s" label
  | JNE label -> format "    jne %s" label
  | JL label -> format "    jl %s" label
  | JLE label -> format "    jle %s" label
  | JG label -> format "    jg %s" label
  | JGE label -> format "    jge %s" label
  | CALL label -> format "    call %s" label
  | RET -> "    ret"
  | AND (dst, src) ->
      format "    andq %s, %s" (operand_to_string src) (register_to_string dst)
  | OR (dst, src) ->
      format "    orq %s, %s" (operand_to_string src) (register_to_string dst)
  | XOR (dst, src) ->
      format "    xorq %s, %s" (operand_to_string src) (register_to_string dst)
  | NOT reg -> format "    notq %s" (register_to_string reg)
  | LABEL name -> format "%s:" name
  | DIRECTIVE dir -> format "    %s" dir
  | COMMENT msg -> format "    # %s" msg

let emit_prologue ~stack_size =
  let instrs = [ PUSH (Reg RBP); MOV (RBP, Reg RSP) ] in
  if stack_size > 0 then instrs @ [ SUB (RSP, Imm stack_size) ] else instrs

let emit_epilogue ~stack_size =
  let instrs = if stack_size > 0 then [ MOV (RSP, Reg RBP) ] else [] in
  instrs @ [ POP RBP; RET ]
