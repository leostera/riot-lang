.text
.p2align 2

.globl _caml_start_program
_caml_start_program:
  stp x29, x30, [sp, #-160]!
  mov x29, sp
  stp x19, x20, [sp, #16]
  stp x21, x22, [sp, #32]
  stp x23, x24, [sp, #48]
  stp x25, x26, [sp, #64]
  stp x27, x28, [sp, #80]
  stp d8, d9, [sp, #96]
  stp d10, d11, [sp, #112]
  stp d12, d13, [sp, #128]
  stp d14, d15, [sp, #144]
  mov x28, x0
  bl _caml_program
  ldp x19, x20, [sp, #16]
  ldp x21, x22, [sp, #32]
  ldp x23, x24, [sp, #48]
  ldp x25, x26, [sp, #64]
  ldp x27, x28, [sp, #80]
  ldp d8, d9, [sp, #96]
  ldp d10, d11, [sp, #112]
  ldp d12, d13, [sp, #128]
  ldp d14, d15, [sp, #144]
  ldp x29, x30, [sp], #160
  ret

.globl _caml_c_call
_caml_c_call:
  sub sp, sp, #16
  str x30, [sp]
  blr x8
  ldr x30, [sp]
  add sp, sp, #16
  ret

.globl _caml_call_realloc_stack
_caml_call_realloc_stack:
  ret
