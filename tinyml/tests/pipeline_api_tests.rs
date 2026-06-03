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
    let nir: native::ssa::SsaProgram = native::lower(&lambda).optimize();
    let aarch64: native::aarch64::Mir = native::aarch64::select(&nir);

    let mut asm = String::new();
    aarch64.emit(&mut asm).expect("emit ok");
    insta::assert_snapshot!(asm, @"
    .text
    .globl _double
    _double:
      stp x29, x30, [sp, #-16]!
      mov x29, sp
      sub sp, sp, #16
      str x0, [sp, #0]
      ldr x9, [sp, #0]
      mov x10, #2
      mul x9, x9, x10
      str x9, [sp, #8]
      ldr x0, [sp, #8]
      mov sp, x29
      ldp x29, x30, [sp], #16
      ret
    .globl _main
    _main:
      stp x29, x30, [sp, #-16]!
      mov x29, sp
      sub sp, sp, #32
      str x0, [sp, #0]
      ldr x9, [sp, #0]
      mov x10, #1
      add x9, x9, x10
      str x9, [sp, #8]
      ldr x0, [sp, #8]
      bl _double
      str x0, [sp, #16]
      ldr x0, [sp, #16]
      mov sp, x29
      ldp x29, x30, [sp], #16
      ret
    ");
}
