use std::{fs, path::Path, process::Command};

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
            "import {{ add }} from {:?}; if (add(41) !== 42) process.exit(1)",
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
            "import {{ get, unwrap }} from {:?}; if (get({{type: 'Some', value: 41}}) !== 41 || get({{type: 'None'}}) !== 0 || unwrap({{type: 'Some', value: 9}}) !== 9) process.exit(1)",
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
            "import {{ fact }} from {:?}; if (fact(6) !== 720) process.exit(1)",
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
            "import {{ make_adder }} from {:?}; if (make_adder(2)(40) !== 42) process.exit(1)",
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
            "import {{ less, same }} from {:?}; if (!less(2) || less(3) || !same(3) || same(2)) process.exit(1)",
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
            "import {{ first, get_name }} from {:?}; if (first([7, 8]) !== 7 || get_name({{name: 'Ada', age: 42}}) !== 'Ada') process.exit(1)",
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
            "import {{ result }} from {:?}; if (result !== 42) process.exit(1)",
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
            "import {{ polymorphic }} from {:?}; if (polymorphic[0] !== 41 || polymorphic[1] !== true) process.exit(1)",
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
