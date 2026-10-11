use tinyml::{backend::js, checker, lambda, parser};

#[test]
fn public_pipeline_lowers_source_to_javascript() {
    let src = r#"
let double = fn x -> x * 2
let main = fn value -> double(value + 1)
"#;

    let ast: parser::Ast = parser::parse(src).expect("parse ok");
    let checked = checker::check_module(&ast);
    assert!(checked.diagnostics.is_empty(), "{:?}", checked.diagnostics);
    let lambda: lambda::Lambda = lambda::translate(&checked.tir);
    let js = js::lower(&checked.interface, &lambda);
    let emitted = js::emit_js_string(&js);

    assert!(emitted.contains("function double"));
    assert!(emitted.contains("function main"));
    assert!(emitted.contains("export {"));
}
