use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

fn tinyml() -> &'static str {
    env!("CARGO_BIN_EXE_tinyml")
}

fn temp_dir(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "tinyml-cli-{name}-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn write_valid_source(dir: &Path) -> std::path::PathBuf {
    let dst = dir.join("hello.ml");
    fs::write(&dst, "let x = 1\n").unwrap();
    dst
}

#[test]
fn check_prints_ok() {
    let dir = temp_dir("check");
    let file = write_valid_source(&dir);

    let output = Command::new(tinyml())
        .arg("check")
        .arg(&file)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "ok\n");
}

#[test]
fn emit_writes_selected_artifacts() {
    let dir = temp_dir("emit");
    let file = write_valid_source(&dir);

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--stage")
        .arg("ast")
        .arg("--stage")
        .arg("tir")
        .arg("--stage")
        .arg("lambda")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    for extension in ["ast", "tir", "lambda", "mjs"] {
        let artifact = dir.join(format!("hello.{extension}"));
        assert!(artifact.exists(), "missing artifact {}", artifact.display());
    }
}

#[test]
fn emit_javascript_produces_executable_module() {
    let dir = temp_dir("emit-js");
    let file = dir.join("main.ml");
    fs::write(&file, "let add = fn x -> x + 1\n").unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let module = dir.join("main.mjs");
    assert!(module.exists());
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ add }} from {:?}; if (add(41n) !== 42n) process.exit(1)",
            module.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_match_module_executes() {
    let dir = temp_dir("emit-js-match");
    let file = dir.join("match.ml");
    fs::write(
        &file,
        "type Option<'a> = | None | Some('a)\nlet get = fn x -> match x { | None -> 0 | Some(value) -> value }\nlet unwrap = fn (Some(value)) -> value\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let module = dir.join("match.mjs");
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ get, unwrap }} from {:?}; if (get({{type: 'Some', value: 41n}}) !== 41n || get({{type: 'None'}}) !== 0n || unwrap({{type: 'Some', value: 9n}}) !== 9n) process.exit(1)",
            module.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_recursive_functions_execute() {
    let dir = temp_dir("emit-js-recursive");
    let file = dir.join("fact.ml");
    fs::write(
        &file,
        "let fact = fn n -> match n { | 0 -> 1 | n -> n * fact (n - 1) }\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ fact }} from {:?}; if (fact(6n) !== 720n) process.exit(1)",
            dir.join("fact.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_closures_capture_values() {
    let dir = temp_dir("emit-js-closure");
    let file = dir.join("closure.ml");
    fs::write(&file, "let make_adder = fn x -> fn y -> x + y\n").unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ make_adder }} from {:?}; if (make_adder(2n)(40n) !== 42n) process.exit(1)",
            dir.join("closure.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_multi_parameter_functions_use_curried_calls() {
    let dir = temp_dir("emit-js-curried-functions");
    let file = dir.join("curried.ml");
    fs::write(
        &file,
        r#"let add left right = left + right
let result = add 20 22
"#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ add, result }} from {:?}; if (add(20n)(22n) !== 42n || result !== 42n) process.exit(1)",
            dir.join("curried.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_comparisons_execute() {
    let dir = temp_dir("emit-js-comparisons");
    let file = dir.join("comparisons.ml");
    fs::write(
        &file,
        "let less = fn x -> x < 3\nlet same = fn x -> x == 3\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ less, same }} from {:?}; if (!less(2n) || less(3n) || !same(3n) || same(2n)) process.exit(1)",
            dir.join("comparisons.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_tuple_and_record_patterns_execute() {
    let dir = temp_dir("emit-js-patterns");
    let file = dir.join("patterns.ml");
    fs::write(
        &file,
        "type User = User { name: string, age: i32 }\nlet first = fn (a, b) -> a\nlet get_name = fn (User { name, age: _ }) -> name\n",
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ first, get_name }} from {:?}; if (first([7n, 8n]) !== 7n || get_name({{name: 'Ada', age: 42n}}) !== 'Ada') process.exit(1)",
            dir.join("patterns.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_multiple_tuple_lets_use_distinct_bindings() {
    let dir = temp_dir("emit-js-multiple-tuple-lets");
    let file = dir.join("tuple_lets.ml");
    fs::write(
        &file,
        "let result = { let (first, _) = (1, 2); let (second, _) = (3, 4); first + second }",
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result !== 4n) process.exit(1)",
            dir.join("tuple_lets.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_fixed_width_integer_operations_are_exact() {
    let dir = temp_dir("emit-js-integers");
    let file = dir.join("integers.ml");
    fs::write(
        &file,
        r#"
        let add_i8 = fn (value: i8) -> value + 1
        let add_i16 = fn (value: i16) -> value + 1
        let add_i32 = fn (value: i32) -> value + 1
        let add_i64 = fn (value: i64) -> value + 1
        let add_u8 = fn (value: u8) -> value + 1
        let add_u16 = fn (value: u16) -> value + 1
        let add_u32 = fn (value: u32) -> value + 1
        let add_u64 = fn (value: u64) -> value + 1
        let subtract_i8 = fn (value: i8) -> value - 1
        let multiply_u8 = fn (value: u8) -> value * 2
        let unsigned_divide = fn (left: u8) -> fn (right: u8) -> left / right
        let unsigned_remainder = fn (left: u8) -> fn (right: u8) -> left % right
        let signed_divide = fn (left: i32) -> fn (right: i32) -> left / right
        let signed_remainder = fn (left: i32) -> fn (right: i32) -> left % right
        let exact_u64 = (18446744073709551615: u64)
        let radix_i8 = (-0x80: i8)
        "#,
    )
    .unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import * as m from {:?}; let trapped = false; try {{ m.signed_divide(1n)(0n) }} catch (error) {{ trapped = error instanceof RangeError }}; if (m.add_i8(127n) !== -128n || m.add_i16(32767n) !== -32768n || m.add_i32(2147483647n) !== -2147483648n || m.add_i64(9223372036854775807n) !== -9223372036854775808n || m.add_u8(255n) !== 0n || m.add_u16(65535n) !== 0n || m.add_u32(4294967295n) !== 0n || m.add_u64(18446744073709551615n) !== 0n || m.subtract_i8(-128n) !== 127n || m.multiply_u8(200n) !== 144n || m.unsigned_divide(255n)(10n) !== 25n || m.unsigned_remainder(255n)(10n) !== 5n || m.signed_divide(-7n)(2n) !== -3n || m.signed_remainder(-7n)(2n) !== -1n || m.exact_u64 !== 18446744073709551615n || m.radix_i8 !== -128n || !trapped) process.exit(1)",
            dir.join("integers.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn emitted_string_literals_decode_escapes() {
    let dir = temp_dir("emit-js-string-escapes");
    let file = dir.join("strings.ml");
    let escape = char::from(92);
    let quote = char::from(34);
    let source = format!(
        "let escaped = {quote}line{escape}nquote: {escape}{quote}yes{escape}{quote} slash: {escape}{escape}{quote}\nlet unknown = {quote}unknown{escape}q{quote}\n"
    );
    fs::write(&file, source).unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("compile")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let expected = format!(
        "line{}quote: {quote}yes{quote} slash: {escape}",
        char::from(10)
    );
    let codepoints = expected
        .chars()
        .map(|character| (character as u32).to_string())
        .collect::<Vec<_>>()
        .join(",");
    let unknown = format!("unknown{escape}q");
    let unknown_codepoints = unknown
        .chars()
        .map(|character| (character as u32).to_string())
        .collect::<Vec<_>>()
        .join(",");
    let node_source = format!(
        "import {{ escaped, unknown }} from {:?}; const expected = String.fromCharCode({codepoints}); const expectedUnknown = String.fromCharCode({unknown_codepoints}); if (escaped !== expected || unknown !== expectedUnknown) process.exit(1)",
        dir.join("strings.mjs").to_str().unwrap()
    );
    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(node_source)
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn bundled_string_intrinsics_cover_unicode_indexing_and_reject_bad_types() {
    let dir = temp_dir("string-intrinsics");
    let source = dir.join("strings.ml");
    fs::write(
        &source,
        r#"
        use String.{concat, length, get, slice, equal}
        use Prelude.{Option, None, Some}
        let text = "A😀B"
        let result = (
          length text,
          get text 1,
          get text 4,
          slice text 1 2,
          equal text "A😀B",
          equal text "AB",
          concat text "!"
        )
        "#,
    )
    .unwrap();
    let output = dir.join("strings.mjs");
    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&output)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result[0] !== 3n || result[1].type !== 'Some' || result[1].value !== 128512n || result[2].type !== 'None' || result[3] !== '😀' || result[4] !== true || result[5] !== false || result[6] !== 'A😀B!') process.exit(1)",
            output.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
    assert!(dir.join("String.mjs").exists());

    let invalid = dir.join("invalid_string_call.ml");
    fs::write(
        &invalid,
        r#"use String.{get}
let invalid = get "text" true
"#,
    )
    .unwrap();
    let invalid_output = dir.join("invalid.mjs");
    let failed = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&invalid_output)
        .arg(&invalid)
        .output()
        .unwrap();
    assert!(!failed.status.success());
    assert!(!invalid_output.exists());
}

#[test]
fn emitted_string_concatenation_executes() {
    let dir = temp_dir("emit-js-strings");
    let file = dir.join("strings.ml");
    fs::write(&file, "let greeting = \"hello, \" + \"world\"\n").unwrap();

    let output = Command::new(tinyml())
        .current_dir(&dir)
        .arg("emit")
        .arg("--target")
        .arg("js")
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ greeting }} from {:?}; if (greeting !== 'hello, world') process.exit(1)",
            dir.join("strings.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn bundled_riotml_compiler_source_executes_nested_match_lexer() {
    let repository_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let compiler_source = repository_root.join("tinyml/riotml/compiler.ml");
    let dir = temp_dir("riotml-compiler-lexer");
    let output = dir.join("compiler.mjs");
    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&output)
        .arg(&compiler_source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ lex }} from {:?}; import {{ readFileSync }} from 'node:fs'; let source = readFileSync({:?}, 'utf8'); let tokens = lex(source); let count = 0; while (tokens.type === 'Cons') {{ count += 1; tokens = tokens.value[1]; }} if (count < 100 || tokens.type !== 'Nil') process.exit(1)",
            output.to_str().unwrap(),
            compiler_source.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compile_js_writes_module_artifact() {
    let dir = temp_dir("compile-js");
    let file = dir.join("prelude.ml");
    fs::write(&file, "let answer = 42\n").unwrap();

    let output = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("js")
        .arg("-o")
        .arg(dir.join("prelude.mjs"))
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(dir.join("prelude.mjs").exists());
    assert!(!dir.join("prelude.tmi").exists());
}

#[test]
fn compile_js_resolves_dependency_source_modules() {
    let dir = temp_dir("compile-js-import");
    let prelude = dir.join("prelude.ml");
    let main = dir.join("Main.ml");
    fs::write(&prelude, "let answer = 41\nlet identity = fn x -> x\n").unwrap();
    fs::write(
        &main,
        "use Prelude.{answer, identity}\nlet result = identity (answer + 1)\n",
    )
    .unwrap();

    for (source, output) in [(&prelude, "Prelude.mjs"), (&main, "Main.mjs")] {
        let mut command = Command::new(tinyml());
        command
            .arg("compile")
            .arg("--target")
            .arg("js")
            .arg("-o")
            .arg(dir.join(output));
        if source == &main {
            command.arg("-I").arg(&dir);
        }
        let result = command.arg(source).output().unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
    }

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result !== 42n) process.exit(1)",
            dir.join("Main.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compiled_reserved_identifiers_import_with_collision_free_js_names() {
    let dir = temp_dir("reserved-js-imports");
    let prelude = dir.join("prelude.ml");
    let main = dir.join("Main.ml");
    fs::write(&prelude, "let default = 40\nlet _default = 2\n").unwrap();
    fs::write(
        &main,
        "use Prelude.{default, _default}\nlet result = default + _default\n",
    )
    .unwrap();

    for (source, output) in [(&prelude, "Prelude.mjs"), (&main, "Main.mjs")] {
        let mut command = Command::new(tinyml());
        command.arg("compile").arg("-o").arg(dir.join(output));
        if source == &main {
            command.arg("-I").arg(&dir);
        }
        let result = command.arg(source).output().unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
    }

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result !== 42n) process.exit(1)",
            dir.join("Main.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compile_resolves_source_modules_and_preserves_imported_types() {
    let dir = temp_dir("source-modules");
    let invocation_dir = temp_dir("source-modules-cwd");
    fs::write(
        dir.join("syntax.ml"),
        r#"
        type Tree<'a> =
          | Leaf('a)
          | Branch(Tree<'a>, Tree<'a>)

        let identity = fn value -> value
        "#,
    )
    .unwrap();
    fs::write(
        dir.join("types.ml"),
        "type User = User { name: string, age: i32 }\n",
    )
    .unwrap();
    let main = dir.join("main.ml");
    fs::write(
        &main,
        r#"
        use Syntax.{Tree, Leaf, Branch, identity}
        use Types.{User}

        let polymorphic = (identity 41, identity true)
        let leaf_count = fn (tree: Tree<i32>) -> match tree {
          | Leaf(_) -> 1
          | Branch(left, right) -> leaf_count left + leaf_count right
        }
        let get_name = fn (User { name, age: _ }) -> name
        "#,
    )
    .unwrap();

    let check = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("check")
        .arg(&main)
        .output()
        .unwrap();
    assert!(
        check.status.success(),
        "{}",
        String::from_utf8_lossy(&check.stderr)
    );

    let run_source = dir.join("run.ml");
    fs::write(
        &run_source,
        "use Syntax.{identity}\nlet polymorphic = (identity 41, identity true)\n",
    )
    .unwrap();
    let output = dir.join("run.mjs");
    let result = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("compile")
        .arg("-o")
        .arg(&output)
        .arg(&run_source)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(dir.join("Syntax.mjs").exists());

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ polymorphic }} from {:?}; if (polymorphic[0] !== 41n || polymorphic[1] !== true) process.exit(1)",
            output.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compiled_modules_run_with_adt_tuple_and_record_patterns() {
    let dir = temp_dir("module-runtime-patterns");
    let invocation_dir = temp_dir("module-runtime-patterns-cwd");
    fs::write(
        dir.join("syntax.ml"),
        r#"
        type Tree<'value> = | Leaf('value) | Fork(Tree<'value>, Tree<'value>)
        type Message =
          | Message { marker: i32 }
          | User { name: string, value: i32 }
          | Done
        let identity = fn value -> value
        "#,
    )
    .unwrap();
    let source = dir.join("main.ml");
    fs::write(
        &source,
        r#"
        use Syntax.{Tree, Leaf, Fork, identity}
        use Syntax.{Message, User, Done}
        type Person = Person { name: string, age: i32 }

        let depth = fn tree -> match tree {
          | Leaf(_) -> 1
          | Fork(left, right) -> depth left + depth right
        }
        let tuple_value = fn pair -> match pair {
          | (0, value) -> value
          | (_, value) -> value + 1
        }
        let classify_message = fn message -> match message {
          | Message { marker } -> marker
          | User { name: "Ada", value } -> value
          | Done -> 0
          | _ -> -1
        }
        let get_age = fn user -> match user {
          | Person { name: "Ada", age } -> age
          | _ -> 0
        }
        let result = (
          identity (depth (Fork(Leaf(1), Fork(Leaf(2), Leaf(3))))),
          tuple_value((0, 41)),
          get_age(Person { name: "Ada", age: 41 }),
          tuple_value((1, 41)),
          classify_message(User { name: "Ada", value: 43 }),
          classify_message(Message { marker: 44 })
        )
        "#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("compile")
        .arg("-o")
        .arg(dir.join("main.mjs"))
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ result }} from {:?}; if (result[0] !== 3n || result[1] !== 41n || result[2] !== 41n || result[3] !== 42n || result[4] !== 43n || result[5] !== 44n) process.exit(1)",
            dir.join("main.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
    let generated = fs::read_to_string(dir.join("main.mjs")).unwrap();
    assert!(generated.contains("import { identity } from \"./Syntax.mjs\";"));
    assert!(!generated.contains("import { Tree"));
}

#[test]
fn bundled_source_library_resolves_outside_the_repository() {
    let dir = temp_dir("bundled-library-source");
    let unrelated_cwd = temp_dir("bundled-library-cwd");
    let source = dir.join("library_client.ml");
    fs::write(
        &source,
        r#"
        use Prelude.{List, Nil, Cons, Option, Some, Result, Ok}
        use List.{length}
        use Option.{map}
        use Result.{unwrap_or}
        use String.{concat}

        let size = length (Cons(1, Cons(2, Nil)))
        let mapped = map (fn value -> value + 1) (Some(41))
        let unwrapped = unwrap_or 0 (Ok(42))
        let message = concat "hello" " world"
        "#,
    )
    .unwrap();

    let result = Command::new(tinyml())
        .current_dir(unrelated_cwd)
        .arg("check")
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&result.stdout), "ok\n");
}

#[test]
fn only_selected_imports_are_available_and_missing_members_fail() {
    let dir = temp_dir("selected-imports");
    fs::write(
        dir.join("values.ml"),
        "let visible = 1\nlet hidden = true\n",
    )
    .unwrap();
    let client = dir.join("client.ml");
    fs::write(
        &client,
        "use Values.{visible}\nlet result = (visible, hidden)\n",
    )
    .unwrap();

    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("unknown value `hidden`"));

    fs::write(&client, "use Values.{missing}\nlet result = 1\n").unwrap();
    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("does not export `missing`"));

    fs::write(dir.join("left.ml"), "let shared = 1\n").unwrap();
    fs::write(dir.join("right.ml"), "let shared = 2\n").unwrap();
    fs::write(
        &client,
        "use Left.{shared}\nuse Right.{shared}\nlet result = shared\n",
    )
    .unwrap();
    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("conflicts with another declaration"));
}

#[test]
fn imported_type_metadata_tracks_unselected_variant_constructors() {
    let dir = temp_dir("imported-type-metadata");
    fs::write(
        dir.join("shape.ml"),
        "type Shape = | Circle(i32) | Square(i32)\n",
    )
    .unwrap();
    let client = dir.join("client.ml");
    fs::write(
        &client,
        "use Shape.{Shape, Circle}\nlet classify = fn (shape: Shape) -> match shape { | Circle(_) -> 1 }\n",
    )
    .unwrap();

    let result = Command::new(tinyml())
        .arg("check")
        .arg(&client)
        .output()
        .unwrap();
    assert!(!result.status.success());
    assert!(
        String::from_utf8_lossy(&result.stderr).contains("match is missing constructor `Square`")
    );
}

#[test]
fn compile_rejects_type_errors_before_writing_artifacts() {
    let dir = temp_dir("compile-type-error");
    let file = dir.join("invalid.ml");
    let output_path = dir.join("invalid.mjs");
    fs::write(&file, "let invalid = (true: i32)\n").unwrap();

    let output = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&output_path)
        .arg(&file)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("typecheck failed"));
    assert!(
        !output_path.exists(),
        "type errors must be rejected before JS emission"
    );
}

#[test]
fn host_intrinsics_run_in_node_and_roundtrip_arguments_files_and_processes() {
    let dir = temp_dir("host-intrinsics");
    let invocation_dir = temp_dir("host-intrinsics-cwd");
    let input = dir.join("input.txt");
    let output = dir.join("output.txt");
    fs::write(&input, "hello from Host\\n").unwrap();
    let source = dir.join("host_client.ml");
    fs::write(
        &source,
        r#"
        use Host.{args, read_file, write_file, write_stdout, write_stderr, run_process}
        use Prelude.{List, Nil, Cons, Result, Ok, Err}

        let first = fn values -> match values { | Nil -> "" | Cons(value, _) -> value }
        let second = fn values -> match values { | Cons(_, Cons(value, _)) -> value | _ -> "" }
        let third = fn values -> match values { | Cons(_, Cons(_, Cons(value, _))) -> value | _ -> "" }
        let arguments = args ()
        let input_path = first arguments
        let output_path = second arguments
        let counter_path = third arguments
        let contents = match (read_file input_path) {
          | Ok(text) -> text
          | Err(_) -> "read failed"
        }
        let write_result = write_file output_path contents
        let child_result = run_process "node" (Cons("-e", Cons("process.exit(7)", Nil)))
        let child_status = match child_result { | Ok(status) -> status | Err(_) -> -1 }
        let counted = match (run_process "node" (Cons("-e", Cons("const fs = require('node:fs'); const path = process.argv[1]; if (fs.existsSync(path)) process.exit(2); fs.writeFileSync(path, '1'); process.exit(1)", Cons(counter_path, Nil))))) {
          | Ok(1) -> 1
          | Ok(2) -> 2
          | Ok(_) -> 3
          | Err(_) -> 4
        }
        let wrote_ok = match write_result { | Ok(_) -> true | Err(_) -> false }
        let child_report = match (child_status == 7) { | true -> "child-ok;" | false -> "child-bad;" }
        let count_report = match (counted == 1) { | true -> "once;" | false -> "multiple;" }
        let write_report = match wrote_ok { | true -> "write-ok" | false -> "write-bad" }
        let output = write_stdout (contents + child_report + count_report + write_report)
        let diagnostic = write_stderr "host diagnostic\\n"
        "#,
    )
    .unwrap();

    let compile = Command::new(tinyml())
        .current_dir(&invocation_dir)
        .arg("compile")
        .arg("-o")
        .arg(dir.join("host_client.mjs"))
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let execution = Command::new("node")
        .arg(dir.join("host_client.mjs"))
        .arg(&input)
        .arg(&output)
        .arg(dir.join("counter.txt"))
        .output()
        .unwrap();
    assert!(
        execution.status.success(),
        "{}",
        String::from_utf8_lossy(&execution.stderr)
    );
    assert_eq!(fs::read_to_string(output).unwrap(), "hello from Host\\n");
    assert_eq!(
        String::from_utf8_lossy(&execution.stdout),
        "hello from Host\\nchild-ok;once;write-ok"
    );
    assert_eq!(
        String::from_utf8_lossy(&execution.stderr),
        "host diagnostic\\n"
    );
    assert!(dir.join("Host.mjs").exists());
    assert_eq!(fs::read_to_string(dir.join("counter.txt")).unwrap(), "1");
}

#[test]
fn host_io_and_process_failures_return_errors() {
    let dir = temp_dir("host-errors");
    let source = dir.join("host_errors.ml");
    let invalid_utf8_path = dir.join("invalid-utf8.txt");
    fs::write(&invalid_utf8_path, [0xff, 0xfe]).unwrap();
    fs::write(
        &source,
        format!(
            r#"
        use Host.{{read_file, write_file, run_process}}
        use Prelude.{{List, Nil, Result, Ok, Err}}

        let missing = read_file "missing-tinyml-host-file"
        let invalid_utf8 = read_file {:?}
        let invalid_write = write_file "" "contents"
        let missing_process = run_process "tinyml-command-does-not-exist" Nil
        "#,
            invalid_utf8_path.to_str().unwrap()
        ),
    )
    .unwrap();
    let output = dir.join("host_errors.mjs");
    let compile = Command::new(tinyml())
        .arg("compile")
        .arg("-o")
        .arg(&output)
        .arg(&source)
        .output()
        .unwrap();
    assert!(
        compile.status.success(),
        "{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ missing, invalid_utf8, invalid_write, missing_process }} from {:?}; if (missing.type !== 'Err' || invalid_utf8.type !== 'Err' || invalid_write.type !== 'Err' || missing_process.type !== 'Err') process.exit(1)",
            output.to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(
        node.status.success(),
        "{}",
        String::from_utf8_lossy(&node.stderr)
    );
}

#[test]
fn compile_rejects_unsupported_target() {
    let dir = temp_dir("compile-target");
    let file = write_valid_source(&dir);

    let output = Command::new(tinyml())
        .arg("compile")
        .arg("--target")
        .arg("x86_64-unknown-linux-gnu")
        .arg(&file)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).contains("unsupported compile target"));
}
