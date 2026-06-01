use tinyml::{backend::native, checker, lambda, parser};

#[test]
fn public_pipeline_reads_ast_tir_lambda_nir_target_mir() {
    let src = r#"
let double = fn x -> x * 2
let main = fn value -> double(value + 1)
"#;

    let ast: parser::Ast = parser::parse(src).expect("parse ok");
    let tir: checker::Tir = checker::check(&ast);
    let lambda: lambda::Lambda = lambda::translate(&tir);
    let nir: native::Nir = native::lower(&lambda);
    let aarch64: native::aarch64::Mir = native::aarch64::select(&nir);

    let mut asm = String::new();
    aarch64.emit(&mut asm).expect("emit ok");
    insta::assert_snapshot!(asm, @r#"
.text
.globl _double
_double:
  stp x29, x30, [sp, #-16]!
  mov x29, sp
  sub sp, sp, #16
  str x0, [sp, #0]
  ldr x9, [sp, #0]
  mov x10, #2
  mul x0, x9, x10
  mov sp, x29
  ldp x29, x30, [sp], #16
  ret
.globl _main
_main:
  stp x29, x30, [sp, #-16]!
  mov x29, sp
  sub sp, sp, #16
  str x0, [sp, #0]
  ldr x9, [sp, #0]
  mov x10, #1
  add x0, x9, x10
  bl _double
  mov sp, x29
  ldp x29, x30, [sp], #16
  ret
"#);
}
