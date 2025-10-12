open Std

(** {1 ARM64 Instructions}
    
    A subset of ARM64 assembly instructions for code generation.
    
    {b For beginners:} ARM64 is the instruction set for Apple Silicon (M1/M2/M3) Macs
    and modern ARM processors. Each instruction is a single operation the CPU can execute.
    
    {b We're keeping it simple:}
    - Only the most common instructions
    - No fancy SIMD/vector stuff
    - No floating point (for now)
    - Just enough to run simple programs!
*)

(** {2 Registers}
    
    ARM64 has 31 general-purpose registers (X0-X30) plus special registers.
*)

type register =
  (* General purpose registers *)
  | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15
  | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23
  | X24 | X25 | X26 | X27 | X28 | X29 | X30
  
  (* Special registers *)
  | SP  (** Stack pointer *)
  | LR  (** Link register (return address) - alias for X30 *)
  | XZR (** Zero register (always reads as 0) *)

let register_to_string = function
  | X0 -> "x0" | X1 -> "x1" | X2 -> "x2" | X3 -> "x3"
  | X4 -> "x4" | X5 -> "x5" | X6 -> "x6" | X7 -> "x7"
  | X8 -> "x8" | X9 -> "x9" | X10 -> "x10" | X11 -> "x11"
  | X12 -> "x12" | X13 -> "x13" | X14 -> "x14" | X15 -> "x15"
  | X16 -> "x16" | X17 -> "x17" | X18 -> "x18" | X19 -> "x19"
  | X20 -> "x20" | X21 -> "x21" | X22 -> "x22" | X23 -> "x23"
  | X24 -> "x24" | X25 -> "x25" | X26 -> "x26" | X27 -> "x27"
  | X28 -> "x28" | X29 -> "x29" | X30 -> "x30"
  | SP -> "sp"
  | LR -> "x30"  (* LR is actually x30 *)
  | XZR -> "xzr"

(** {2 Operands}
    
    Values that can be used in instructions.
*)

type operand =
  | Reg of register
      (** Register operand: [x0], [sp], etc. *)
  | Imm of int
      (** Immediate value (constant): [#42], [#0], etc.
          ARM64 limits immediate sizes - we'll handle that in emission *)
  | Label of string
      (** Label reference: [_main], [_my_function], etc. *)

let operand_to_string = function
  | Reg r -> register_to_string r
  | Imm n -> format "#%d" n
  | Label l -> l

(** {2 Memory Addressing}
    
    Different ways to address memory on ARM64.
*)

type address =
  | RegOffset of register * int
      (** Register + offset: [[x0, #8]] = *(x0 + 8) *)
  | RegReg of register * register
      (** Register + register: [[x0, x1]] = *(x0 + x1) *)
  | PreIndex of register * int
      (** Pre-increment: [[x0, #8]!] = *(x0 += 8) *)
  | PostIndex of register * int
      (** Post-increment: [[x0], #8] = tmp = *x0; x0 += 8; return tmp *)

let address_to_string = function
  | RegOffset (r, 0) -> format "[%s]" (register_to_string r)
  | RegOffset (r, off) -> format "[%s, #%d]" (register_to_string r) off
  | RegReg (r1, r2) -> format "[%s, %s]" (register_to_string r1) (register_to_string r2)
  | PreIndex (r, off) -> format "[%s, #%d]!" (register_to_string r) off
  | PostIndex (r, off) -> format "[%s], #%d" (register_to_string r) off

(** {2 Instructions}
    
    ARM64 instructions we'll use for code generation.
*)

type instruction =
  (* {3 Data Movement} *)
  | MOV of register * operand
      (** Move: [MOV x0, #42] sets x0 = 42 *)
  
  | MOVK of register * int * int
      (** Move with keep: [MOVK x0, #42, 16] - set bits 16-31 to 42, keep others
          Used for loading large constants in pieces *)
  
  | LDR of register * address
      (** Load register: [LDR x0, [x1, #8]] loads x0 = *(x1 + 8) *)
  
  | STR of register * address
      (** Store register: [STR x0, [x1, #8]] stores *(x1 + 8) = x0 *)
  
  | STP of register * register * address
      (** Store pair: [STP x0, x1, [sp, #-16]!] stores both x0 and x1 *)
  
  | LDP of register * register * address
      (** Load pair: [LDP x0, x1, [sp], #16] loads both x0 and x1 *)
  
  (* {3 Arithmetic} *)
  | ADD of register * register * operand
      (** Add: [ADD x0, x1, #42] sets x0 = x1 + 42 *)
  
  | SUB of register * register * operand
      (** Subtract: [SUB x0, x1, #10] sets x0 = x1 - 10 *)
  
  | MUL of register * register * register
      (** Multiply: [MUL x0, x1, x2] sets x0 = x1 * x2
          Note: No immediate form! *)
  
  | SDIV of register * register * register
      (** Signed divide: [SDIV x0, x1, x2] sets x0 = x1 / x2 *)
  
  | NEG of register * register
      (** Negate: [NEG x0, x1] sets x0 = -x1 *)
  
  (* {3 Comparisons} *)
  | CMP of register * operand
      (** Compare: [CMP x0, #42] sets flags based on x0 - 42
          Doesn't store result, just sets condition flags *)
  
  (* {3 Control Flow} *)
  | B of string
      (** Unconditional branch: [B label] jumps to label *)
  
  | BL of string
      (** Branch with link: [BL function] calls function
          Stores return address in LR (x30) *)
  
  | BR of register
      (** Branch to register: [BR x0] jumps to address in x0 *)
  
  | BLR of register
      (** Branch with link to register: calls function at address in register *)
  
  | RET
      (** Return: jumps to address in LR (x30) *)
  
  (* {3 Conditional Branches} *)
  | BEQ of string
      (** Branch if equal: jump if last CMP was equal *)
  | BNE of string
      (** Branch if not equal *)
  | BLT of string
      (** Branch if less than (signed) *)
  | BLE of string
      (** Branch if less than or equal *)
  | BGT of string
      (** Branch if greater than *)
  | BGE of string
      (** Branch if greater than or equal *)
  
  (* {3 Logical Operations} *)
  | AND of register * register * operand
      (** Bitwise AND *)
  | ORR of register * register * operand
      (** Bitwise OR *)
  | EOR of register * register * operand
      (** Bitwise XOR *)
  | MVN of register * register
      (** Bitwise NOT: [MVN x0, x1] sets x0 = ~x1 *)
  
  (* {3 Pseudo-instructions and Directives} *)
  | LABEL of string
      (** Label definition: [_my_label:] *)
  
  | DIRECTIVE of string
      (** Assembler directive: [.global _main], [.align 2], etc. *)
  
  | COMMENT of string
      (** Comment for debugging: [// This is a comment] *)

(** {2 Instruction Formatting}
    
    Convert instructions to ARM64 assembly syntax.
*)

let instruction_to_string = function
  | MOV (dst, src) ->
      format "    mov %s, %s" (register_to_string dst) (operand_to_string src)
  
  | MOVK (dst, imm, shift) ->
      format "    movk %s, #%d, lsl #%d" (register_to_string dst) imm shift
  
  | LDR (dst, addr) ->
      format "    ldr %s, %s" (register_to_string dst) (address_to_string addr)
  
  | STR (src, addr) ->
      format "    str %s, %s" (register_to_string src) (address_to_string addr)
  
  | STP (r1, r2, addr) ->
      format "    stp %s, %s, %s" 
        (register_to_string r1) (register_to_string r2) (address_to_string addr)
  
  | LDP (r1, r2, addr) ->
      format "    ldp %s, %s, %s"
        (register_to_string r1) (register_to_string r2) (address_to_string addr)
  
  | ADD (dst, src, op) ->
      format "    add %s, %s, %s"
        (register_to_string dst) (register_to_string src) (operand_to_string op)
  
  | SUB (dst, src, op) ->
      format "    sub %s, %s, %s"
        (register_to_string dst) (register_to_string src) (operand_to_string op)
  
  | MUL (dst, src1, src2) ->
      format "    mul %s, %s, %s"
        (register_to_string dst) (register_to_string src1) (register_to_string src2)
  
  | SDIV (dst, src1, src2) ->
      format "    sdiv %s, %s, %s"
        (register_to_string dst) (register_to_string src1) (register_to_string src2)
  
  | NEG (dst, src) ->
      format "    neg %s, %s" (register_to_string dst) (register_to_string src)
  
  | CMP (src, op) ->
      format "    cmp %s, %s" (register_to_string src) (operand_to_string op)
  
  | B label ->
      format "    b %s" label
  
  | BL label ->
      format "    bl %s" label
  
  | BR reg ->
      format "    br %s" (register_to_string reg)
  
  | BLR reg ->
      format "    blr %s" (register_to_string reg)
  
  | RET ->
      "    ret"
  
  | BEQ label -> format "    b.eq %s" label
  | BNE label -> format "    b.ne %s" label
  | BLT label -> format "    b.lt %s" label
  | BLE label -> format "    b.le %s" label
  | BGT label -> format "    b.gt %s" label
  | BGE label -> format "    b.ge %s" label
  
  | AND (dst, src, op) ->
      format "    and %s, %s, %s"
        (register_to_string dst) (register_to_string src) (operand_to_string op)
  
  | ORR (dst, src, op) ->
      format "    orr %s, %s, %s"
        (register_to_string dst) (register_to_string src) (operand_to_string op)
  
  | EOR (dst, src, op) ->
      format "    eor %s, %s, %s"
        (register_to_string dst) (register_to_string src) (operand_to_string op)
  
  | MVN (dst, src) ->
      format "    mvn %s, %s" (register_to_string dst) (register_to_string src)
  
  | LABEL name ->
      format "%s:" name
  
  | DIRECTIVE dir ->
      format "    %s" dir
  
  | COMMENT msg ->
      format "    // %s" msg

(** {2 Helper Functions} *)

let emit_prologue ~stack_size =
  (** Generate function prologue.
      
      Saves frame pointer and link register, sets up stack frame.
      
      Example:
      {[
        stp x29, x30, [sp, #-16]!  // Save FP and LR
        mov x29, sp                // Set up frame pointer
        sub sp, sp, #stack_size    // Allocate stack space
      ]}
  *)
  let instrs = [
    STP (X29, X30, PreIndex (SP, -16));
    MOV (X29, Reg SP);
  ] in
  if stack_size > 0 then
    instrs @ [SUB (SP, SP, Imm stack_size)]
  else
    instrs

let emit_epilogue ~stack_size =
  (** Generate function epilogue.
      
      Restores stack and returns.
      
      Example:
      {[
        mov sp, x29                // Restore stack pointer
        ldp x29, x30, [sp], #16    // Restore FP and LR
        ret                        // Return
      ]}
  *)
  let instrs =
    if stack_size > 0 then
      [MOV (SP, Reg X29)]
    else
      []
  in
  instrs @ [
    LDP (X29, X30, PostIndex (SP, 16));
    RET;
  ]
