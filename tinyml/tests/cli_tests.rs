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
        .arg("--stage")
        .arg("nir")
        .arg("--target")
        .arg("aarch64")
        .arg(&file)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    for extension in ["ast", "tir", "lambda", "nml", "S"] {
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
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));

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
    assert!(node.status.success(), "{}", String::from_utf8_lossy(&node.stderr));
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
    assert!(output.status.success(), "{}", String::from_utf8_lossy(&output.stderr));

    let node = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(format!(
            "import {{ fact }} from {:?}; if (fact(6) !== 720) process.exit(1)",
            dir.join("fact.mjs").to_str().unwrap()
        ))
        .output()
        .unwrap();
    assert!(node.status.success(), "{}", String::from_utf8_lossy(&node.stderr));
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
