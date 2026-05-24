use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::OnceLock;

use assert_cmd::cargo::cargo_bin;
use tempfile::TempDir;

type FixtureResult = Result<(), Box<dyn std::error::Error>>;

fn program_fixture_compiles(fixture: &Path) -> FixtureResult {
    ensure_release_runtime_built()?;
    let temp_dir = TempDir::new()?;
    let output_path = temp_dir.path().join(output_name(fixture));

    let compile = compile_fixture(fixture, &output_path)?;
    let mut transcript = Transcript::new(fixture);
    transcript.push_output("compile", &compile);

    if !compile.status.success() {
        assert_fixture_snapshot("program", fixture, &transcript.finish());
        return fail(format!(
            "expected program fixture to compile: {}",
            fixture.display()
        ));
    }

    let expected_stdout = fixture_path(fixture).with_extension("stdout");
    if expected_stdout.exists() {
        let run = Command::new(&output_path).output()?;
        transcript.push_output("run", &run);

        if !run.status.success() {
            assert_fixture_snapshot("program", fixture, &transcript.finish());
            return fail(format!(
                "expected generated fixture binary to run: {}",
                output_path.display()
            ));
        }

        let expected = std::fs::read(&expected_stdout)?;
        if run.stdout != expected {
            assert_fixture_snapshot("program", fixture, &transcript.finish());
            return fail(format!("stdout mismatch for {}", fixture.display()));
        }
    }

    assert_fixture_snapshot("program", fixture, &transcript.finish());
    Ok(())
}

fn diagnostic_fixture_fails(fixture: &Path) -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let output_path = temp_dir.path().join(output_name(fixture));

    let compile = compile_fixture(fixture, &output_path)?;
    let mut transcript = Transcript::new(fixture);
    transcript.push_output("compile", &compile);

    assert_fixture_snapshot("diagnostic", fixture, &transcript.finish());

    if compile.status.success() {
        return fail(format!(
            "expected diagnostics fixture to fail: {}",
            fixture.display()
        ));
    }

    Ok(())
}

#[test]
fn emit_typed_resolves_use_from_sig_dir() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let sig_dir = temp_dir.path().join("sigs");
    std::fs::create_dir(&sig_dir)?;
    let math = temp_dir.path().join("math.ml");
    let main = temp_dir.path().join("main.ml");
    let math_rsig = sig_dir.join("Math.rsig");

    std::fs::write(&math, "fn answer() { 42 }\n")?;
    std::fs::write(&main, "use Math\nfn main() { dbg(\"ok\") }\n")?;

    let emit_rsig = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("rsig")
        .arg(&math)
        .arg("-o")
        .arg(&math_rsig)
        .output()?;
    if !emit_rsig.status.success() {
        return fail(format!(
            "expected rsig emission to succeed:\n{}",
            String::from_utf8_lossy(&emit_rsig.stderr)
        ));
    }

    let emit_typed = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("typed")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&sig_dir)
        .output()?;
    if !emit_typed.status.success() {
        return fail(format!(
            "expected typed emit to resolve --sig-dir:\n{}",
            String::from_utf8_lossy(&emit_typed.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit_typed.stdout);
    if !stdout.contains("TypedUse") || !stdout.contains("Math") {
        return fail(format!(
            "typed output did not include resolved use:\n{stdout}"
        ));
    }

    Ok(())
}

#[test]
fn emit_typed_preserves_block_expression_boundary() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let source = temp_dir.path().join("main.ml");

    std::fs::write(
        &source,
        "fn main() { dbg({ let x = 40; let y = 2; x + y }) }\n",
    )?;

    let emit_typed = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("typed")
        .arg(&source)
        .output()?;
    if !emit_typed.status.success() {
        return fail(format!(
            "expected typed emit to preserve block expression:\n{}",
            String::from_utf8_lossy(&emit_typed.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit_typed.stdout);
    if !stdout.contains("Block(") || !stdout.contains("TypedBlock") {
        return fail(format!(
            "typed output did not include block expression boundary:\n{stdout}"
        ));
    }

    Ok(())
}

#[test]
fn emit_boundaries_show_compiler_like_fixture_shapes() -> FixtureResult {
    let typed = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("typed")
        .arg("tests/fixtures/programs/basic/compiler_like_token_classifier.ml")
        .output()?;
    if !typed.status.success() {
        return fail(format!(
            "expected typed emit for compiler-like fixture:\n{}",
            String::from_utf8_lossy(&typed.stderr)
        ));
    }
    let typed_stdout = String::from_utf8_lossy(&typed.stdout);
    for needle in [
        "TypedTypeDecl",
        "TypedVariantConstructor",
        "TypedMatchArm",
        "summary",
    ] {
        if !typed_stdout.contains(needle) {
            return fail(format!(
                "typed boundary missing `{needle}`:\n{typed_stdout}"
            ));
        }
    }

    let lambda = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("ir")
        .arg("tests/fixtures/programs/basic/lambda_variant_renderer.ml")
        .output()?;
    if !lambda.status.success() {
        return fail(format!(
            "expected lambda emit for closure/variant fixture:\n{}",
            String::from_utf8_lossy(&lambda.stderr)
        ));
    }
    let lambda_stdout = String::from_utf8_lossy(&lambda.stdout);
    for needle in ["Lambda", "Capture", "LambdaMatchArm", "Constructor"] {
        if !lambda_stdout.contains(needle) {
            return fail(format!(
                "lambda boundary missing `{needle}`:\n{lambda_stdout}"
            ));
        }
    }

    Ok(())
}

#[test]
fn mixed_case_file_name_is_rejected() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let source = temp_dir.path().join("helloWorld.ml");
    let output = temp_dir.path().join("helloWorld.rsig");
    std::fs::write(&source, "fn main() { dbg(\"bad\") }\n")?;

    let emit_rsig = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("rsig")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()?;
    if emit_rsig.status.success() {
        return fail("expected mixed-case file name to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&emit_rsig.stderr);
    if !stderr.contains("invalid module file name") {
        return fail(format!("unexpected diagnostic:\n{stderr}"));
    }

    Ok(())
}

#[test]
fn main_receives_argv_and_returns_i32_status() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let source = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &source,
        "fn main(args: List<String>) -> i32 { dbg(list_get(args, 1)); 7 }\n",
    )?;

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected argv main to compile:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).arg("hello").output()?;
    if run.status.code() != Some(7) {
        return fail(format!(
            "expected exit status 7, got {}",
            status_label(&run)
        ));
    }
    if run.stdout != b"hello\n" {
        return fail(format!(
            "unexpected argv main stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn main_result_err_controls_exit_status() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let source = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &source,
        "fn main(args: List<String>) -> Result<(), i32> { Err(5) }\n",
    )?;

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected result main to compile:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).arg("ignored").output()?;
    if run.status.code() != Some(5) {
        return fail(format!(
            "expected exit status 5, got {}",
            status_label(&run)
        ));
    }

    Ok(())
}

#[test]
fn std_arg_parser_compiles_and_reports_missing_values() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let arg_parser = manifest_dir().join("../std/arg_parser.ml");
    let source = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &source,
        r#"use ArgParser

fn run(matches: ArgParser.Matches) -> Result<(), i32> {
  if ArgParser.get_flag(matches, "help") {
    dbg("help");
    Ok(())
  } else {
    match ArgParser.get_one(matches, "output") {
      Some(output) -> {
        dbg(output);
        Ok(())
      },
      None -> {
        dbg("none");
        Ok(())
      },
      _ -> Err(3)
    }
  }
}

fn main(args: List<String>) -> Result<(), i32> {
  let cmd = ArgParser.command("riotc", [
    ArgParser.flag("help", "--help"),
    ArgParser.option("output", "--output")
  ]);
  match ArgParser.parse(cmd, args) {
    Ok(matches) -> run(matches),
    Err(error) -> {
      dbg(ArgParser.error_message(error));
      Err(2)
    },
    _ -> Err(4)
  }
}
"#,
    )?;

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&arg_parser)
        .arg(&source)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected std arg parser program to compile:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let ok = Command::new(&output)
        .args(["--output", "out.txt"])
        .output()?;
    if ok.status.code() != Some(0) || ok.stdout != b"out.txt\n" {
        return fail(format!(
            "unexpected arg parser success run: status={}, stdout={}",
            status_label(&ok),
            String::from_utf8_lossy(&ok.stdout)
        ));
    }

    let missing = Command::new(&output).arg("--output").output()?;
    if missing.status.code() != Some(2) || missing.stdout != b"missing value for --output\n" {
        return fail(format!(
            "unexpected arg parser missing-value run: status={}, stdout={}",
            status_label(&missing),
            String::from_utf8_lossy(&missing.stdout)
        ));
    }

    Ok(())
}

#[test]
fn emit_object_links_imported_function_from_object_dir() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let sig_dir = temp_dir.path().join("sigs");
    let object_dir = temp_dir.path().join("objects");
    std::fs::create_dir(&sig_dir)?;
    std::fs::create_dir(&object_dir)?;

    let math = temp_dir.path().join("math.ml");
    let main = temp_dir.path().join("main.ml");
    let math_rsig = sig_dir.join("Math.rsig");
    let math_object = object_dir.join("Math.o");
    let output = temp_dir.path().join("main");

    std::fs::write(&math, "fn answer() { 42 }\n")?;
    std::fs::write(&main, "use Math\nfn main() { dbg(Math.answer()) }\n")?;

    let emit_rsig = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("rsig")
        .arg(&math)
        .arg("-o")
        .arg(&math_rsig)
        .output()?;
    if !emit_rsig.status.success() {
        return fail(format!(
            "expected rsig emission to succeed:\n{}",
            String::from_utf8_lossy(&emit_rsig.stderr)
        ));
    }

    let emit_object = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("object")
        .arg(&math)
        .arg("-o")
        .arg(&math_object)
        .output()?;
    if !emit_object.status.success() {
        return fail(format!(
            "expected object emission to succeed:\n{}",
            String::from_utf8_lossy(&emit_object.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&sig_dir)
        .arg("--object-dir")
        .arg(&object_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported run stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn explicit_object_mapping_links_imported_function() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let sig_dir = temp_dir.path().join("sigs");
    std::fs::create_dir(&sig_dir)?;

    let math = temp_dir.path().join("math.ml");
    let main = temp_dir.path().join("main.ml");
    let math_rsig = sig_dir.join("Math.rsig");
    let math_object = temp_dir.path().join("custom_math.o");
    let output = temp_dir.path().join("main");

    std::fs::write(&math, "fn answer() { 42 }\n")?;
    std::fs::write(&main, "use Math\nfn main() { dbg(Math.answer()) }\n")?;

    let emit_rsig = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("rsig")
        .arg(&math)
        .arg("-o")
        .arg(&math_rsig)
        .output()?;
    if !emit_rsig.status.success() {
        return fail(format!(
            "expected rsig emission to succeed:\n{}",
            String::from_utf8_lossy(&emit_rsig.stderr)
        ));
    }

    let emit_object = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("object")
        .arg(&math)
        .arg("-o")
        .arg(&math_object)
        .output()?;
    if !emit_object.status.success() {
        return fail(format!(
            "expected object emission to succeed:\n{}",
            String::from_utf8_lossy(&emit_object.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&sig_dir)
        .arg("--object")
        .arg(format!("Math={}", math_object.display()))
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected explicit object compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected explicit object run stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_emits_signature_and_object_artifacts() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let math = temp_dir.path().join("math.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&math, "fn answer() -> i64 { 42 }\n")?;
    std::fs::write(&main, "use Math\nfn main() { dbg(Math.answer()) }\n")?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&math)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let math_rsig = out_dir.join("Math.rsig");
    let math_object = out_dir.join("Math.o");
    if !math_rsig.exists() || !math_object.exists() {
        return fail(format!(
            "compile-lib did not emit expected artifacts:\n  {}\n  {}",
            math_rsig.display(),
            math_object.display()
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected compile against compile-lib artifacts to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }
    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected compile-lib linked stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_multiple_sources_processes_in_dependency_order() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let math = temp_dir.path().join("math.ml");
    let constants = temp_dir.path().join("constants.ml");

    std::fs::write(
        &math,
        "use Constants\nfn answer() { Constants.base() + 1 }\n",
    )?;
    std::fs::write(&constants, "fn base() { 41 }\n")?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&math)
        .arg(&constants)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected multi-source compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    for artifact in [
        out_dir.join("Math.rsig"),
        out_dir.join("Math.o"),
        out_dir.join("Constants.rsig"),
        out_dir.join("Constants.o"),
    ] {
        if !artifact.exists() {
            return fail(format!(
                "missing multi-source artifact: {}",
                artifact.display()
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_multiple_sources_links_dependency_objects() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let main = temp_dir.path().join("main.ml");
    let math = temp_dir.path().join("math.ml");
    let constants = temp_dir.path().join("constants.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&main, "use Math\nfn main() { dbg(Math.answer()) }\n")?;
    std::fs::write(
        &math,
        "use Constants\nfn answer() { Constants.base() + 1 }\n",
    )?;
    std::fs::write(&constants, "fn base() { 41 }\n")?;

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg(&math)
        .arg(&constants)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected multi-source compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected multi-source stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_three_module_interfaces_link_through_object_dir() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let analyze = temp_dir.path().join("analyze.ml");
    let render = temp_dir.path().join("render.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | Number(i64)\nfn ident(name: String) -> token { Ident(name) }\nfn number(value: i64) -> token { Number(value) }\n",
    )?;
    std::fs::write(
        &analyze,
        "use Syntax\nfn weight(token: Syntax.token) -> i64 { match token { Syntax.Ident(_) -> 1, Syntax.Number(value) -> value } }\n",
    )?;
    std::fs::write(
        &render,
        "use Syntax\nuse Analyze\nfn describe(token: Syntax.token) -> String { if Analyze.weight(token) == 1 { \"ident\" } else { \"number\" } }\nfn describe_ident() -> String { describe(Syntax.ident(\"name\")) }\nfn number_weight() -> i64 { Analyze.weight(Syntax.number(41)) }\n",
    )?;
    std::fs::write(
        &main,
        "use Render\nfn main() { dbg(Render.number_weight()); println(Render.describe_ident()) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&render)
        .arg(&analyze)
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected three-module compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    for artifact in [
        out_dir.join("Syntax.rsig"),
        out_dir.join("Syntax.o"),
        out_dir.join("Analyze.rsig"),
        out_dir.join("Analyze.o"),
        out_dir.join("Render.rsig"),
        out_dir.join("Render.o"),
    ] {
        if !artifact.exists() {
            return fail(format!("missing three-module artifact: {}", artifact.display()));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected three-module executable compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"41\nident\n" {
        return fail(format!(
            "unexpected three-module stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_compiler_shaped_actor_smoke() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let analyze = temp_dir.path().join("analyze.ml");
    let worker = temp_dir.path().join("worker.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | Number(i64) | Plus\nfn ident(name: String) -> token { Ident(name) }\nfn number(value: i64) -> token { Number(value) }\nfn plus() -> token { Plus }\n",
    )?;
    std::fs::write(
        &analyze,
        "use Syntax\ntype classification = Word(String) | Int(i64) | Symbol(String)\nfn classify(token: Syntax.token) -> classification { match token { Syntax.Ident(name) -> Word(name), Syntax.Number(value) -> Int(value), Syntax.Plus -> Symbol(\"plus\") } }\n",
    )?;
    std::fs::write(
        &worker,
        "use Syntax\nuse Analyze\nfn print_classification(token: Syntax.token) { match Analyze.classify(token) { Analyze.Word(name) -> println(name), Analyze.Int(value) -> dbg(value), Analyze.Symbol(name) -> println(name) } }\nfn start() -> actor_id<Syntax.token> { spawn { receive { token -> print_classification(token) }; receive { token -> print_classification(token) }; receive { token -> print_classification(token) } } }\n",
    )?;
    std::fs::write(
        &main,
        "use Syntax\nuse Worker\nfn main() { let worker = Worker.start(); send(worker, Syntax.ident(\"ident\")); send(worker, Syntax.number(42)); send(worker, Syntax.plus()) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&worker)
        .arg(&analyze)
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected compiler-shaped smoke compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected compiler-shaped smoke executable compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"ident\n42\nplus\n" {
        return fail(format!(
            "unexpected compiler-shaped smoke stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compiler_shaped_actor_smoke_emit_boundaries() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let analyze = temp_dir.path().join("analyze.ml");
    let worker = temp_dir.path().join("worker.ml");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | Number(i64) | Plus\nfn ident(name: String) -> token { Ident(name) }\nfn number(value: i64) -> token { Number(value) }\nfn plus() -> token { Plus }\n",
    )?;
    std::fs::write(
        &analyze,
        "use Syntax\ntype classification = Word(String) | Int(i64) | Symbol(String)\nfn classify(token: Syntax.token) -> classification { match token { Syntax.Ident(name) -> Word(name), Syntax.Number(value) -> Int(value), Syntax.Plus -> Symbol(\"plus\") } }\n",
    )?;
    std::fs::write(
        &worker,
        "use Syntax\nuse Analyze\nfn print_classification(token: Syntax.token) { match Analyze.classify(token) { Analyze.Word(name) -> println(name), Analyze.Int(value) -> dbg(value), Analyze.Symbol(name) -> println(name) } }\nfn start() -> actor_id<Syntax.token> { spawn { receive { token -> print_classification(token) }; receive { token -> print_classification(token) }; receive { token -> print_classification(token) } } }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&analyze)
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected smoke dependency compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&worker)
        .arg("--sig-dir")
        .arg(&out_dir)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected compiler-shaped smoke emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }

    let stdout = String::from_utf8_lossy(&emit.stdout);
    let typed = emit_all_section(&stdout, "== typed ==", "== rsig ==")?;
    let rsig = emit_all_section(&stdout, "== rsig ==", "== ir ==")?;
    let lambda = emit_all_section(&stdout, "== ir ==", "== actor-ir ==")?;
    let actor_ir = emit_all_section(&stdout, "== actor-ir ==", "== llvm ==")?;
    for (section_name, section, expected) in [
        ("typed", typed.as_str(), "Analyze.classification"),
        ("rsig", rsig.as_str(), "fn start() -> actor_id<Syntax.token>"),
        ("lambda", lambda.as_str(), "\"classify\""),
        ("actor-ir", actor_ir.as_str(), "ActorFrameLayout"),
    ] {
        if !section.contains(expected) {
            return fail(format!(
                "expected compiler-shaped smoke {section_name} emit section to contain `{expected}`:\n{section}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_exports_annotated_mutual_recursion() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let logic = temp_dir.path().join("logic.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &logic,
        "fn is_even(n: i64) -> bool { if n < 1 { true } else { is_odd(n - 1) } }\nfn is_odd(n: i64) -> bool { if n < 1 { false } else { is_even(n - 1) } }\n",
    )?;
    std::fs::write(
        &main,
        "use Logic\nfn main() { dbg(Logic.is_even(8)); dbg(Logic.is_odd(9)) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&logic)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected mutually recursive compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported mutual recursion compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"true\ntrue\n" {
        return fail(format!(
            "unexpected imported mutual recursion stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_higher_order_function_uses_arrow_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let apply = temp_dir.path().join("apply.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&apply, "fn apply_i64(f, x) { f(x + 0) + 1 }\n")?;
    std::fs::write(
        &main,
        "use Apply\nfn main() { dbg(Apply.apply_i64(fn(n) { n + 1 }, 40)) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&apply)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected higher-order compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected higher-order imported compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected higher-order imported stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_polymorphic_closure_values_have_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"fn make_identity() {
  fn(value) { value }
}

fn make_int_const() {
  fn(_) { 42 }
}

fn make_string_const() {
  fn(_) { "imported" }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let int_id = Funcs.make_identity();
  dbg(int_id(41) + 1);
  let string_id = Funcs.make_identity();
  dbg(string_concat(string_id("poly"), " closure"));
  let answer = Funcs.make_int_const();
  dbg(answer("ignored"));
  let label = Funcs.make_string_const();
  dbg(string_concat(label(()), " const"));
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected polymorphic closure compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected polymorphic closure emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "fn make_identity() -> ('",
        "fn make_int_const() -> ('",
        "-> i64",
        "fn make_string_const() -> ('",
        "-> String",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "polymorphic closure rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported polymorphic closure compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\npoly closure\n42\nimported const\n" {
        return fail(format!(
            "unexpected imported polymorphic closure stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_polymorphic_closure_params_have_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"fn keep_identity(id: ('a -> 'a)) -> ('a -> 'a) {
  id
}

fn accept_callback(callback: ('a -> 'a)) {
  ()
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let id = Funcs.keep_identity(fn(value) { value });
  Funcs.accept_callback(fn(value) { value });
  dbg(id(41) + 1);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected polymorphic closure-param compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected polymorphic closure-param emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "fn keep_identity(('",
        "-> '",
        "fn accept_callback(('",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "polymorphic closure-param rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported polymorphic closure-param compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported polymorphic closure-param stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_container_closure_params_have_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"type bundle<'id, 'answer> = { id: 'id, answer: 'answer }

fn keep_pair(pair: (('a -> 'a), ('b -> i64))) -> (('a -> 'a), ('b -> i64)) {
  pair
}

fn keep_bundle(bundle: bundle<('a -> 'a), ('b -> i64)>) -> bundle<('a -> 'a), ('b -> i64)> {
  bundle
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let pair = Funcs.keep_pair((fn(value) { value }, fn(_) { 20 }));
  let pair_id = pair.0;
  let pair_answer = pair.1;
  dbg(pair_id(21) + pair_answer("ignored"));

  let bundle = Funcs.keep_bundle(Funcs.bundle {
    id: fn(value) { value },
    answer: fn(_) { 10 },
  });
  dbg(bundle.id(31) + bundle.answer("ignored"));
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected container closure-param compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected container closure-param emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "fn keep_pair(((",
        "fn keep_bundle(bundle<('",
        "-> i64",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "container closure-param rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported container closure-param compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"41\n41\n" {
        return fail(format!(
            "unexpected imported container closure-param stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_variant_and_list_closure_params_have_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let bad_main = temp_dir.path().join("bad_main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"type cell<'f> = One('f)

fn keep_cell(cell: cell<('a -> i64)>) -> cell<('a -> i64)> {
  cell
}

fn keep_list(callbacks: List<('a -> i64)>) -> List<('a -> i64)> {
  callbacks
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let callbacks = Funcs.keep_list([fn(_value: String) { 20 }]);
  let list_value = match callbacks {
    [id, .._] -> id("ignored") + 1,
    [] -> 0,
  };
  dbg(list_value);

  let cell = Funcs.keep_cell(Funcs.One(fn(_value: String) { 40 }));
  let cell_value = match cell {
    Funcs.One(id) -> id("ignored") + 1,
  };
  dbg(cell_value);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected variant/list closure-param compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected variant/list closure-param emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["fn keep_cell(cell<('", "fn keep_list(List<('"] {
        if !stdout.contains(expected) {
            return fail(format!(
                "variant/list closure-param rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported variant/list closure-param compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"21\n41\n" {
        return fail(format!(
            "unexpected imported variant/list closure-param stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    std::fs::write(
        &bad_main,
        r#"use Funcs

fn main() {
  let _callbacks = Funcs.keep_list([fn(_value: String) { "not an int" }]);
  dbg("done")
}
"#,
    )?;
    let bad_compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad_main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad_main"))
        .output()?;
    if bad_compile.status.success() {
        return fail("expected bad imported variant/list closure-param compile to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&bad_compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `i64`, but the argument provides `String`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "bad variant/list closure-param diagnostic missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_nested_closure_params_match_arrows() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let bad_main = temp_dir.path().join("bad_main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"fn run_with_i64(callback: (('a -> i64) -> i64)) -> i64 {
  callback(fn(_value) { 40 })
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let result = Funcs.run_with_i64(fn(callback: (String -> i64)) {
    callback("ignored") + 1
  });
  dbg(result + 1);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected nested closure-param compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected nested closure-param emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["fn run_with_i64(((", "-> i64) -> i64)"] {
        if !stdout.contains(expected) {
            return fail(format!(
                "nested closure-param rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported nested closure-param compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported nested closure-param stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    std::fs::write(
        &bad_main,
        r#"use Funcs

fn main() {
  let _result = Funcs.run_with_i64(fn(callback: (String -> String)) {
    string_concat(callback("ignored"), "!");
    0
  });
  dbg("done")
}
"#,
    )?;
    let bad_compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad_main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad_main"))
        .output()?;
    if bad_compile.status.success() {
        return fail("expected bad imported nested closure-param compile to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&bad_compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `i64`, but the argument provides `String`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "bad nested closure-param diagnostic missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_record_nested_closure_params_match_arrows() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let bad_main = temp_dir.path().join("bad_main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"type runner<'callback> = { run: 'callback }

fn use_runner(runner: runner<(('a -> i64) -> i64)>) -> i64 {
  let run = runner.run;
  run(fn(_value) { 40 })
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let result = Funcs.use_runner(Funcs.runner {
    run: fn(callback: (String -> i64)) {
      callback("ignored") + 1
    },
  });
  dbg(result + 1);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected record nested closure-param compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected record nested closure-param emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["fn use_runner(runner<((", "-> i64) -> i64)>"] {
        if !stdout.contains(expected) {
            return fail(format!(
                "record nested closure-param rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported record nested closure-param compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported record nested closure-param stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    std::fs::write(
        &bad_main,
        r#"use Funcs

fn main() {
  let _result = Funcs.use_runner(Funcs.runner {
    run: fn(callback: (String -> String)) {
      string_concat(callback("ignored"), "!");
      0
    },
  });
  dbg("done")
}
"#,
    )?;
    let bad_compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad_main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad_main"))
        .output()?;
    if bad_compile.status.success() {
        return fail("expected bad imported record nested closure-param compile to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&bad_compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `i64`, but the argument provides `String`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "bad record nested closure-param diagnostic missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_variant_nested_closure_params_match_arrows() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let bad_main = temp_dir.path().join("bad_main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"type task<'callback> = Run('callback)

fn use_task(task: task<(('a -> i64) -> i64)>) -> i64 {
  match task {
    Run(run) -> run(fn(_value) { 40 }),
  }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let result = Funcs.use_task(Funcs.Run(fn(callback: (String -> i64)) {
    callback("ignored") + 1
  }));
  dbg(result + 1);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected variant nested closure-param compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected variant nested closure-param emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["fn use_task(task<((", "-> i64) -> i64)>"] {
        if !stdout.contains(expected) {
            return fail(format!(
                "variant nested closure-param rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported variant nested closure-param compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported variant nested closure-param stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    std::fs::write(
        &bad_main,
        r#"use Funcs

fn main() {
  let _result = Funcs.use_task(Funcs.Run(fn(callback: (String -> String)) {
    string_concat(callback("ignored"), "!");
    0
  }));
  dbg("done")
}
"#,
    )?;
    let bad_compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad_main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad_main"))
        .output()?;
    if bad_compile.status.success() {
        return fail("expected bad imported variant nested closure-param compile to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&bad_compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `i64`, but the argument provides `String`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "bad variant nested closure-param diagnostic missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_actor_id_closure_params_match_nested_messages() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let bad_main = temp_dir.path().join("bad_main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"fn observe(worker: actor_id<'msg>, callback: actor_id<'msg> -> i64) -> i64 {
  callback(worker)
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

type monitor_down = Down(actor_id<_>)

fn make_worker() {
  spawn {
    receive {
      "go" -> dbg("worker:go"),
    }
  }
}

fn main() {
  spawn {
    let worker = make_worker();
    monitor(worker);
    let result = Funcs.observe(worker, fn(target: actor_id<String>) {
      send(target, "go");
      41
    });
    receive {
      Down(_) -> dbg(result + 1),
    }
  };
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected actor-id closure-param compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected actor-id closure-param emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["fn observe(actor_id<'", "actor_id<'", "-> i64"] {
        if !stdout.contains(expected) {
            return fail(format!(
                "actor-id closure-param rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported actor-id closure-param compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"worker:go\n42\n" {
        return fail(format!(
            "unexpected imported actor-id closure-param stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    std::fs::write(
        &bad_main,
        r#"use Funcs

fn make_string_worker() {
  spawn {
    receive {
      "go" -> (),
    }
  }
}

fn main() {
  let _result = Funcs.observe(make_string_worker(), fn(target: actor_id<i64>) {
    send(target, 1);
    0
  });
  dbg("done")
}
"#,
    )?;
    let bad_compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad_main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad_main"))
        .output()?;
    if bad_compile.status.success() {
        return fail("expected bad imported actor-id closure-param compile to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&bad_compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `String`, but the argument provides `i64`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "bad actor-id closure-param diagnostic missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_tuple_of_polymorphic_closures_has_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"fn make_pair() {
  (fn(value) { value }, fn(_) { 42 })
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let pair = Funcs.make_pair();
  let id = pair.0;
  let answer = pair.1;
  dbg(id(41) + 1);
  dbg(answer("ignored"));
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected tuple closure compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected tuple closure emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["fn make_pair() -> ((", "-> '", "-> i64)"] {
        if !stdout.contains(expected) {
            return fail(format!("tuple closure rsig missed `{expected}`:\n{stdout}"));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported tuple closure compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n42\n" {
        return fail(format!(
            "unexpected imported tuple closure stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_record_of_polymorphic_closures_has_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"type bundle<'id, 'answer> = { id: 'id, answer: 'answer }

fn make_bundle() {
  bundle { id: fn(value) { value }, answer: fn(_) { 42 } }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let bundle = Funcs.make_bundle();
  let id = bundle.id;
  let answer = bundle.answer;
  dbg(id(41) + 1);
  dbg(answer("ignored"));
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected record closure compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected record closure emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["type bundle<", "fn make_bundle() -> bundle<(", "-> i64)>"] {
        if !stdout.contains(expected) {
            return fail(format!("record closure rsig missed `{expected}`:\n{stdout}"));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported record closure compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n42\n" {
        return fail(format!(
            "unexpected imported record closure stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_variant_of_polymorphic_closures_has_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"type bundle<'id, 'answer> = Bundle('id, 'answer)

fn make_bundle() {
  Bundle(fn(value) { value }, fn(_) { 42 })
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let bundle = Funcs.make_bundle();
  let total = match bundle {
    Funcs.Bundle(id, answer) -> id(41) + answer("ignored")
  };
  dbg(total + 1)
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected variant closure compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected variant closure emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["type bundle<", "fn make_bundle() -> bundle<(", "-> i64)>"] {
        if !stdout.contains(expected) {
            return fail(format!("variant closure rsig missed `{expected}`:\n{stdout}"));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported variant closure compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"84\n" {
        return fail(format!(
            "unexpected imported variant closure stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_list_of_polymorphic_closures_has_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &funcs,
        r#"fn make_ids() {
  [fn(value) { value }]
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let ids = Funcs.make_ids();
  let result = match ids {
    [id, .._] -> id(41) + 1,
    [] -> 0
  };
  dbg(result)
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected list closure compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected list closure emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in ["fn make_ids() -> List<(", "-> '"] {
        if !stdout.contains(expected) {
            return fail(format!("list closure rsig missed `{expected}`:\n{stdout}"));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported list closure compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported list closure stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_empty_list_keeps_unknown_abi_diagnostic() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&funcs, "fn make_empty() { [] }\n")?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let items = Funcs.make_empty();
  dbg(0)
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected empty list compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected empty list emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    if !stdout.contains("fn make_empty() -> List<'") {
        return fail(format!("empty list rsig missed unknown element type:\n{stdout}"));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if compile.status.success() {
        return fail("expected imported empty list compile to fail unknown ABI".to_string());
    }
    let stderr = String::from_utf8_lossy(&compile.stderr);
    for expected in ["imported value has unknown ABI", "make_empty"] {
        if !stderr.contains(expected) {
            return fail(format!(
                "empty list unknown ABI diagnostic missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_tuple_containing_unknown_keeps_unknown_abi_diagnostic() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let funcs = temp_dir.path().join("funcs.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&funcs, "fn make_pair() { ([], 1) }\n")?;
    std::fs::write(
        &main,
        r#"use Funcs

fn main() {
  let pair = Funcs.make_pair();
  dbg(0)
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&funcs)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected tuple unknown compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&funcs)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected tuple unknown emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    if !stdout.contains("fn make_pair() -> (List<'") {
        return fail(format!("tuple rsig missed unknown nested element type:\n{stdout}"));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if compile.status.success() {
        return fail(
            "expected imported tuple containing unknown compile to fail unknown ABI".to_string(),
        );
    }
    let stderr = String::from_utf8_lossy(&compile.stderr);
    for expected in ["imported value has unknown ABI", "make_pair"] {
        if !stderr.contains(expected) {
            return fail(format!(
                "tuple unknown ABI diagnostic missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn imported_function_values_use_arrow_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let math = temp_dir.path().join("math.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&math, "fn add(n, x) { x + n }\n")?;
    std::fs::write(
        &main,
        "use Math\nfn main() { let f = Math.add; dbg(f(2)(40)) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&math)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected function-value compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported function-value compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported function-value stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_variant_constructors_use_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let palette = temp_dir.path().join("palette.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&palette, "type color = Red | Green | Blue\n")?;
    std::fs::write(&main, "use Palette\nfn main() { dbg(Palette.Blue) }\n")?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&palette)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected variant compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported variant constructor compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"Blue\n" {
        return fail(format!(
            "unexpected imported variant constructor stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_variant_annotations_use_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let palette = temp_dir.path().join("palette.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &palette,
        "type color = Red | Green | Blue\nfn blue() -> color { Blue }\n",
    )?;
    std::fs::write(
        &main,
        "use Palette\nfn main() { let c: Palette.color = Palette.blue(); dbg(c) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&palette)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected annotated variant compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported variant annotation compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"Blue\n" {
        return fail(format!(
            "unexpected imported variant annotation stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_generic_variant_pattern_uses_rsig_type_args() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let options = temp_dir.path().join("options.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");
    let bad_output = temp_dir.path().join("bad");

    std::fs::write(
        &options,
        "type option<'a> = Some('a) | None\nfn make_i64(value: i64) -> option<i64> { Some(value) }\n",
    )?;
    std::fs::write(
        &main,
        "use Options\nfn main() { let label = match Options.make_i64(1) { Options.Some(value) -> if value == 1 { \"one\" } else { \"other\" }, Options.None -> \"none\" }; dbg(label) }\n",
    )?;
    std::fs::write(
        &bad,
        "use Options\nfn main() { match Options.make_i64(1) { Options.Some(value) -> string_concat(value, \"\"), Options.None -> \"none\" } }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&options)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected generic variant compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported generic variant pattern compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"one\n" {
        return fail(format!(
            "unexpected imported generic variant stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let compile_bad = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&bad_output)
        .output()?;
    if compile_bad.status.success() {
        return fail("expected imported generic variant binding mismatch to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&compile_bad.stderr);
    if !stderr.contains("i64") {
        return fail(format!(
            "expected imported generic variant binder to carry i64 into string_concat diagnostic:\n{stderr}"
        ));
    }

    Ok(())
}

#[test]
fn imported_generic_variant_payload_tests_nested_imported_record_pattern() -> FixtureResult {
    ensure_release_runtime_built()?;
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let boxes = temp_dir.path().join("boxes.ml");
    let options = temp_dir.path().join("options.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");
    let bad_output = temp_dir.path().join("bad");

    std::fs::write(
        &boxes,
        "type box<'a> = { value: 'a }\nfn make_i64(value: i64) -> box<i64> { box { value: value } }\n",
    )?;
    std::fs::write(&options, "type option<'a> = Some('a) | None\n")?;
    std::fs::write(
        &main,
        "use Boxes\nuse Options\nfn main() { let wrapped = Options.Some(Boxes.make_i64(1)); let label = match wrapped { Options.Some(Boxes.box { value: value }) -> if value == 1 { \"one\" } else { \"other\" }, Options.None -> \"none\" }; dbg(label) }\n",
    )?;
    std::fs::write(
        &bad,
        "use Boxes\nuse Options\nfn main() { let wrapped = Options.Some(Boxes.make_i64(1)); let out = match wrapped { Options.Some(Boxes.box { value: value }) -> string_concat(value, \"\"), Options.None -> \"none\" }; dbg(out) }\n",
    )?;

    for source in [&boxes, &options] {
        let compile_lib = Command::new(cargo_bin("stage0"))
            .current_dir(manifest_dir())
            .arg("compile-lib")
            .arg(source)
            .arg("--out-dir")
            .arg(&out_dir)
            .output()?;
        if !compile_lib.status.success() {
            return fail(format!(
                "expected imported nested payload dependency compile-lib to succeed for {}:\n{}",
                source.display(),
                String::from_utf8_lossy(&compile_lib.stderr)
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported generic variant nested imported record pattern compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"one\n" {
        return fail(format!(
            "unexpected imported generic variant nested imported record stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let compile_bad = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&bad_output)
        .output()?;
    if compile_bad.status.success() {
        return fail("expected nested imported generic pattern mismatch to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&compile_bad.stderr);
    if !stderr.contains("i64") {
        return fail(format!(
            "expected nested imported generic pattern binder to carry i64 into string_concat diagnostic:\n{stderr}"
        ));
    }

    Ok(())
}

#[test]
fn imported_variant_non_exhaustive_names_missing_constructor() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let options = temp_dir.path().join("options.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &options,
        "type option<'a> = Some('a) | None\nfn make_i64(value: i64) -> option<i64> { Some(value) }\n",
    )?;
    std::fs::write(
        &main,
        "use Options\nfn main() { let label = match Options.make_i64(1) { Options.Some(_) -> \"some\" }; dbg(label) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&options)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected generic variant compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if compile.status.success() {
        return fail("expected imported non-exhaustive variant match to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&compile.stderr);
    if !stderr.contains("Options.None") {
        return fail(format!(
            "expected imported missing constructor diagnostic to name Options.None:\n{stderr}"
        ));
    }

    Ok(())
}

#[test]
fn imported_compiler_data_model_flows_through_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | IntLit(i64)\ntype span = { start: i64, end: i64 }\nfn name(token) { match token { Ident(text) -> text, IntLit(_) -> \"int\" } }\nfn zero_span() { span { start: 0, end: 0 } }\n",
    )?;
    std::fs::write(
        &main,
        "use Syntax\nfn main() { let tok = Syntax.Ident(\"answer\"); let span = Syntax.zero_span(); println(Syntax.name(tok)); dbg(span.start); dbg(span.end) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected compiler-data compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported compiler-data compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"answer\n0\n0\n" {
        return fail(format!(
            "unexpected imported compiler-data stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_record_construction_uses_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let geometry = temp_dir.path().join("geometry.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(&geometry, "type point = { x: i64, y: i64 }\n")?;
    std::fs::write(
        &main,
        "use Geometry\nfn main() { let p: Geometry.point = Geometry.point { x: 10, y: 20 }; dbg(p.x); dbg(p.y) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&geometry)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected record compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported record construction compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"10\n20\n" {
        return fail(format!(
            "unexpected imported record construction stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_generic_record_pattern_uses_rsig_type_args() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let boxes = temp_dir.path().join("boxes.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &boxes,
        "type box<'a> = { value: 'a }\nfn make_i64(value: i64) -> box<i64> { box { value: value } }\n",
    )?;
    std::fs::write(
        &main,
        "use Boxes\nfn main() { let out = match Boxes.make_i64(1) { Boxes.box { value: _ } -> \"ok\" }; dbg(out) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&boxes)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected generic record compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported generic record pattern compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"ok\n" {
        return fail(format!(
            "unexpected imported generic record stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_generic_record_literal_infers_rsig_type_args() -> FixtureResult {
    ensure_release_runtime_built()?;
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let boxes = temp_dir.path().join("boxes.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");
    let bad_output = temp_dir.path().join("bad");

    std::fs::write(&boxes, "type box<'a> = { value: 'a }\n")?;
    std::fs::write(
        &main,
        "use Boxes\nfn main() { let made = Boxes.box { value: 1 }; let label = match made { Boxes.box { value: value } -> if value == 1 { \"one\" } else { \"other\" } }; dbg(label) }\n",
    )?;
    std::fs::write(
        &bad,
        "use Boxes\nfn main() { let made = Boxes.box { value: 1 }; dbg(match made { Boxes.box { value: value } -> string_concat(value, \"\") }) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&boxes)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected generic record provider to compile:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported generic record literal inference to compile:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"one\n" {
        return fail(format!(
            "unexpected imported generic record literal stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let compile_bad = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&bad_output)
        .output()?;
    if compile_bad.status.success() {
        return fail("expected imported generic record literal binder mismatch to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&compile_bad.stderr);
    if !stderr.contains("i64") {
        return fail(format!(
            "expected imported generic record literal binder to carry i64 into string_concat diagnostic:\n{stderr}"
        ));
    }

    Ok(())
}

#[test]
fn imported_generic_record_field_access_infers_rsig_type_args() -> FixtureResult {
    ensure_release_runtime_built()?;
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let boxes = temp_dir.path().join("boxes.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");
    let bad_output = temp_dir.path().join("bad");

    std::fs::write(&boxes, "type box<'a> = { value: 'a }\n")?;
    std::fs::write(
        &main,
        "use Boxes\nfn main() { let item = Boxes.box { value: 41 }; dbg(item.value + 1) }\n",
    )?;
    std::fs::write(
        &bad,
        "use Boxes\nfn main() { let item = Boxes.box { value: 1 }; dbg(string_concat(item.value, \"\")) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&boxes)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected generic record provider to compile:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported generic record field access to compile:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"42\n" {
        return fail(format!(
            "unexpected imported generic record field stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let compile_bad = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&bad_output)
        .output()?;
    if compile_bad.status.success() {
        return fail("expected imported generic record field mismatch to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&compile_bad.stderr);
    if !stderr.contains("function argument type does not match") || !stderr.contains("i64") {
        return fail(format!(
            "expected imported generic record field access to report i64 argument mismatch:\n{stderr}"
        ));
    }

    Ok(())
}

#[test]
fn imported_generic_record_pattern_binds_rsig_type_args() -> FixtureResult {
    ensure_release_runtime_built()?;
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let boxes = temp_dir.path().join("boxes.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");
    let bad_output = temp_dir.path().join("bad");

    std::fs::write(
        &boxes,
        "type box<'a> = { value: 'a }\nfn make_i64(value: i64) -> box<i64> { box { value: value } }\n",
    )?;
    std::fs::write(
        &main,
        "use Boxes\nfn main() { let label = match Boxes.make_i64(1) { Boxes.box { value: value } -> if value == 1 { \"one\" } else { \"other\" } }; dbg(label) }\n",
    )?;
    std::fs::write(
        &bad,
        "use Boxes\nfn main() { match Boxes.make_i64(1) { Boxes.box { value: value } -> string_concat(value, \"\") } }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&boxes)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected generic record compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported generic record binding compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"one\n" {
        return fail(format!(
            "unexpected imported generic record binding stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let compile_bad = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&bad_output)
        .output()?;
    if compile_bad.status.success() {
        return fail("expected imported generic record binding mismatch to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&compile_bad.stderr);
    if !stderr.contains("i64") {
        return fail(format!(
            "expected imported generic record binder to carry i64 into string_concat diagnostic:\n{stderr}"
        ));
    }

    Ok(())
}

#[test]
fn imported_tuple_pattern_tests_nested_items_through_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | Number(i64)\nfn good() -> (token, token) { (Ident(\"ok\"), Number(7)) }\nfn bad() -> (token, token) { (Ident(\"bad\"), Ident(\"not-number\")) }\n",
    )?;
    std::fs::write(
        &main,
        "use Syntax\nfn classify(pair: (Syntax.token, Syntax.token)) { match pair { (Syntax.Ident(\"ok\"), Syntax.Number(7)) -> \"ok\", _ -> \"fallback\" } }\nfn main() { dbg(classify(Syntax.good())); dbg(classify(Syntax.bad())) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected imported tuple-pattern provider to compile:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported nested tuple-pattern compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"ok\nfallback\n" {
        return fail(format!(
            "unexpected imported tuple-pattern stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_record_pattern_tests_nested_fields_through_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | Number(i64)\ntype entry = { head: token, tail: token }\nfn good() -> entry { entry { head: Ident(\"ok\"), tail: Number(7) } }\nfn bad() -> entry { entry { head: Ident(\"bad\"), tail: Ident(\"not-number\") } }\n",
    )?;
    std::fs::write(
        &main,
        "use Syntax\nfn classify(entry: Syntax.entry) { match entry { Syntax.entry { head: Syntax.Ident(\"ok\"), tail: Syntax.Number(7) } -> \"ok\", _ -> \"fallback\" } }\nfn main() { dbg(classify(Syntax.good())); dbg(classify(Syntax.bad())) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected imported record-pattern provider to compile:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported nested record-pattern compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"ok\nfallback\n" {
        return fail(format!(
            "unexpected imported record-pattern stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_list_pattern_tests_nested_items_through_rsig() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | Number(i64)\nfn good() -> List<token> { [Ident(\"ok\"), Number(7)] }\nfn bad() -> List<token> { [Ident(\"bad\"), Ident(\"not-number\")] }\n",
    )?;
    std::fs::write(
        &main,
        "use Syntax\nfn classify(tokens: List<Syntax.token>) { match tokens { [Syntax.Ident(\"ok\"), Syntax.Number(7), ..rest] -> \"ok\", _ -> \"fallback\" } }\nfn main() { dbg(classify(Syntax.good())); dbg(classify(Syntax.bad())) }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected imported list-pattern provider to compile:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported nested list-pattern compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"ok\nfallback\n" {
        return fail(format!(
            "unexpected imported list-pattern stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn imported_unknown_abi_call_is_rejected() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let sig_dir = temp_dir.path().join("sigs");
    std::fs::create_dir(&sig_dir)?;

    let math = temp_dir.path().join("math.ml");
    let main = temp_dir.path().join("main.ml");
    let math_rsig = sig_dir.join("Math.rsig");
    let output = temp_dir.path().join("main");

    std::fs::write(&math, "fn id(x) { x }\n")?;
    std::fs::write(&main, "use Math\nfn main() { dbg(Math.id(1)) }\n")?;

    let emit_rsig = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("rsig")
        .arg(&math)
        .arg("-o")
        .arg(&math_rsig)
        .output()?;
    if !emit_rsig.status.success() {
        return fail(format!(
            "expected rsig emission to succeed:\n{}",
            String::from_utf8_lossy(&emit_rsig.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&sig_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if compile.status.success() {
        return fail("expected unknown ABI import to fail".to_owned());
    }
    let stderr = String::from_utf8_lossy(&compile.stderr);
    if !stderr.contains("unknown ABI") {
        return fail(format!("unexpected unknown ABI diagnostic:\n{stderr}"));
    }

    Ok(())
}

#[test]
fn emit_llvm_contains_real_function_and_actor_symbols() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/actors/fibonacci_worker.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("llvm")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected llvm emission to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for needle in [
        "riot_mod_FibonacciWorker_fib",
        "riot_rt_alloc_frame_v2",
        "riot_rt_spawn_actor_v2",
        "riot_actor_FibonacciWorker_",
    ] {
        if !stdout.contains(needle) {
            return fail(format!("llvm output did not contain `{needle}`:\n{stdout}"));
        }
    }

    Ok(())
}

#[test]
fn emit_llvm_uses_std_send_value_external() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/actors/send_bool_payload.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("llvm")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected llvm emission to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    if !stdout.contains("riot_rt_send_value") {
        return fail(format!(
            "llvm output did not contain std send external:\n{stdout}"
        ));
    }

    Ok(())
}

#[test]
fn emit_llvm_contains_recursive_self_call() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/recursive_factorial.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("llvm")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected llvm emission to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    if !stdout.contains("call i64 @riot_mod_RecursiveFactorial_fact") {
        return fail(format!(
            "llvm output did not contain recursive fact call:\n{stdout}"
        ));
    }

    Ok(())
}

#[test]
fn emit_llvm_contains_while_loop_blocks() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/while_false_skips_body.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("llvm")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected llvm emission to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "br label %while.cond",
        "while.cond:",
        "br i1 false, label %while.body, label %while.cont",
        "while.body:",
        "while.cont:",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "llvm output did not contain while loop boundary `{expected}`:\n{stdout}"
            ));
        }
    }

    Ok(())
}

#[test]
fn emit_llvm_uniques_nested_while_loop_blocks() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/while_nested_false.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("llvm")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected llvm emission to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "while.cond:",
        "while.body:",
        "while.cont:",
        "while.cond1:",
        "while.body2:",
        "while.cont3:",
        "br label %while.cond1",
        "br label %while.cond",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "llvm output did not contain nested while boundary `{expected}`:\n{stdout}"
            ));
        }
    }

    Ok(())
}

#[test]
fn emit_all_contains_while_across_lowering_phases() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/while_false_skips_body.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "== cst ==",
        "Expr(\n                            While",
        "== typed ==",
        "kind: While",
        "== ir ==",
        "While {",
        "== llvm ==",
        "while.cond:",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "emit all output did not contain while lowering marker `{expected}`:\n{stdout}"
            ));
        }
    }

    Ok(())
}

#[test]
fn emit_all_preserves_pipeline_phase_order() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/actors/fibonacci_worker.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    let mut previous = 0;
    for marker in [
        "== cst ==",
        "== typed ==",
        "== rsig ==",
        "== ir ==",
        "== actor-ir ==",
        "== llvm ==",
    ] {
        let Some(index) = stdout.find(marker) else {
            return fail(format!(
                "emit all output did not contain `{marker}`:\n{stdout}"
            ));
        };
        if index < previous {
            return fail(format!(
                "emit all marker `{marker}` appeared out of order:\n{stdout}"
            ));
        }
        previous = index;
    }
    if !stdout.contains("ActorFrameLayout") || !stdout.contains("riot_rt_spawn_actor_v2") {
        return fail(format!(
            "emit all output missed boundary details:\n{stdout}"
        ));
    }

    Ok(())
}

#[test]
fn emit_all_includes_stable_interface_text() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/actor_factory_signature.ml");
    let emit_once = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&fixture)
        .output()?;
    if !emit_once.status.success() {
        return fail(format!(
            "expected first emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit_once.stderr)
        ));
    }
    let emit_twice = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&fixture)
        .output()?;
    if !emit_twice.status.success() {
        return fail(format!(
            "expected second emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit_twice.stderr)
        ));
    }

    let first_stdout = String::from_utf8_lossy(&emit_once.stdout);
    let second_stdout = String::from_utf8_lossy(&emit_twice.stdout);
    let first_rsig = emit_all_section(&first_stdout, "== rsig ==", "== ir ==")?;
    let second_rsig = emit_all_section(&second_stdout, "== rsig ==", "== ir ==")?;

    if first_rsig != second_rsig {
        return fail(format!(
            "emit all rsig text was not deterministic:\nfirst:\n{first_rsig}\nsecond:\n{second_rsig}"
        ));
    }
    for expected in [
        "module ActorFactorySignature",
        "fingerprint ",
        "fn make_worker() -> actor_id<String>",
        "riot_mod_ActorFactorySignature_make_worker",
    ] {
        if !first_rsig.contains(expected) {
            return fail(format!(
                "emit all rsig text missed `{expected}`:\n{first_rsig}"
            ));
        }
    }

    Ok(())
}

#[test]
fn emit_interface_outputs_canonical_interface_text() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/actor_factory_signature.ml");
    let emit_interface = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("interface")
        .arg(&fixture)
        .output()?;
    if !emit_interface.status.success() {
        return fail(format!(
            "expected emit interface to succeed:\n{}",
            String::from_utf8_lossy(&emit_interface.stderr)
        ));
    }
    let emit_all = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&fixture)
        .output()?;
    if !emit_all.status.success() {
        return fail(format!(
            "expected emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit_all.stderr)
        ));
    }

    let interface_stdout = String::from_utf8_lossy(&emit_interface.stdout);
    let all_stdout = String::from_utf8_lossy(&emit_all.stdout);
    let all_rsig = emit_all_section(&all_stdout, "== rsig ==", "== ir ==")?;
    let interface_text = interface_stdout.trim();

    if interface_text != all_rsig {
        return fail(format!(
            "emit interface text differed from emit all rsig section:\ninterface:\n{interface_text}\nall:\n{all_rsig}"
        ));
    }
    assert_interface_text_shape(interface_text)?;

    Ok(())
}

#[test]
fn emit_interface_writes_review_artifact() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/actor_factory_signature.ml");
    let temp_dir = TempDir::new()?;
    let interface_path = temp_dir.path().join("ActorFactorySignature.interface.txt");
    let emit_interface = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("interface")
        .arg(&fixture)
        .arg("--output")
        .arg(&interface_path)
        .output()?;
    if !emit_interface.status.success() {
        return fail(format!(
            "expected emit interface --output to succeed:\n{}",
            String::from_utf8_lossy(&emit_interface.stderr)
        ));
    }
    if !emit_interface.stdout.is_empty() {
        return fail(format!(
            "expected emit interface --output to keep stdout empty, got:\n{}",
            String::from_utf8_lossy(&emit_interface.stdout)
        ));
    }

    let interface_text = std::fs::read_to_string(&interface_path)?;
    assert_interface_text_shape(interface_text.trim())?;

    Ok(())
}

#[test]
fn emit_interface_records_imported_dependencies() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let sig_dir = temp_dir.path().join("build");
    let syntax = temp_dir.path().join("syntax.ml");
    let analyze = temp_dir.path().join("analyze.ml");

    std::fs::write(
        &syntax,
        "type token = Ident(String) | Number(i64)\nfn ident(name: String) -> token { Ident(name) }\n",
    )?;
    std::fs::write(
        &analyze,
        "use Syntax\nfn weight(token: Syntax.token) -> i64 { match token { Syntax.Ident(_) -> 1, Syntax.Number(value) -> value } }\n",
    )?;

    let compile_syntax = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&syntax)
        .arg("--out-dir")
        .arg(&sig_dir)
        .output()?;
    if !compile_syntax.status.success() {
        return fail(format!(
            "expected Syntax compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_syntax.stderr)
        ));
    }

    let emit_interface = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("interface")
        .arg(&analyze)
        .arg("--sig-dir")
        .arg(&sig_dir)
        .output()?;
    if !emit_interface.status.success() {
        return fail(format!(
            "expected imported emit interface to succeed:\n{}",
            String::from_utf8_lossy(&emit_interface.stderr)
        ));
    }

    let interface_text = String::from_utf8_lossy(&emit_interface.stdout);
    for expected in [
        "module Analyze",
        "depends Syntax ",
        "fn weight(Syntax.token) -> i64",
        "riot_mod_Analyze_weight",
    ] {
        if !interface_text.contains(expected) {
            return fail(format!(
                "imported emit interface missed `{expected}`:\n{interface_text}"
            ));
        }
    }
    for forbidden in ["Analyze.Syntax.token", "== cst ==", "== typed =="] {
        if interface_text.contains(forbidden) {
            return fail(format!(
                "imported emit interface included forbidden `{forbidden}`:\n{interface_text}"
            ));
        }
    }

    Ok(())
}

fn assert_interface_text_shape(interface_text: &str) -> FixtureResult {
    for forbidden in ["== cst ==", "== typed ==", "== ir ==", "== llvm =="] {
        if interface_text.contains(forbidden) {
            return fail(format!(
                "emit interface included pipeline marker `{forbidden}`:\n{interface_text}"
            ));
        }
    }
    for expected in [
        "module ActorFactorySignature",
        "fingerprint ",
        "fn make_worker() -> actor_id<String>",
        "riot_mod_ActorFactorySignature_make_worker",
    ] {
        if !interface_text.contains(expected) {
            return fail(format!(
                "emit interface text missed `{expected}`:\n{interface_text}"
            ));
        }
    }

    Ok(())
}

fn emit_all_section(
    output: &str,
    start: &str,
    end: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let Some(start_index) = output.find(start) else {
        return Err(format!("emit all output did not contain `{start}`:\n{output}").into());
    };
    let after_start = start_index + start.len();
    let Some(relative_end_index) = output[after_start..].find(end) else {
        return Err(
            format!("emit all output did not contain `{end}` after `{start}`:\n{output}").into(),
        );
    };
    Ok(output[after_start..after_start + relative_end_index]
        .trim()
        .to_owned())
}

#[test]
fn emit_all_exposes_actor_message_types_in_rsig() -> FixtureResult {
    let fixture = manifest_dir().join("tests/fixtures/programs/basic/actor_factory_signature.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    if !stdout.contains("fn make_worker() -> actor_id<String>") {
        return fail(format!(
            "rsig output did not expose actor message type:\n{stdout}"
        ));
    }

    Ok(())
}

#[test]
fn emit_all_distinguishes_concrete_and_unknown_actor_message_types() -> FixtureResult {
    let fixture =
        manifest_dir().join("tests/fixtures/programs/basic/actor_message_signature_shapes.ml");
    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&fixture)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "fn string_worker() -> actor_id<String>",
        "fn boxed_worker() -> actor_id<box<i64>>",
        "fn mixed_worker() -> actor_id<'a>",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "rsig output missed actor message signature `{expected}`:\n{stdout}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_higher_order_actor_factories_preserve_message_types() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let actors = temp_dir.path().join("actors.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &actors,
        r#"fn make_sender_factory() {
  fn(_) {
    spawn { receive { "go" -> dbg("import higher sent") } }
  }
}

fn make_any_factory() {
  fn(_) {
    spawn {
      receive { "go" -> dbg("import higher any string") };
      receive { 1 -> dbg("import higher any int") }
    }
  }
}

fn make_idle_closure() {
  let idle = spawn { receive { () -> () } };
  fn(_) { idle }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Actors

type monitor_down = Down(actor_id<_>)

fn main() {
  let sender_factory = Actors.make_sender_factory();
  send(sender_factory(()), "go");
  let any_factory = Actors.make_any_factory();
  let any = any_factory(());
  send(any, "go");
  send(any, 1);
  let idle_factory = Actors.make_idle_closure();
  spawn {
    monitor(idle_factory(()));
    dbg("import higher monitored");
    receive { Down(_) -> () }
  };
  spawn {
    link(idle_factory(()));
    dbg("import higher linked");
    receive { () -> () }
  };
  ()
}
"#,
    )?;
    std::fs::write(
        &bad,
        r#"use Actors
fn main() {
  let sender_factory = Actors.make_sender_factory();
  send(sender_factory(()), 1)
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&actors)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected higher-order actor factory compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&actors)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected higher-order actor factory emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "fn make_sender_factory() -> ('",
        "-> actor_id<String>",
        "fn make_any_factory() -> ('",
        "-> actor_id<'",
        "fn make_idle_closure() -> ('",
        "-> actor_id<unit>",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "higher-order actor factory rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported higher-order actor factory compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"import higher sent\nimport higher any string\nimport higher any int\nimport higher monitored\nimport higher linked\n" {
        return fail(format!(
            "unexpected imported higher-order actor factory stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let bad_compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad"))
        .output()?;
    if bad_compile.status.success() {
        return fail("expected imported higher-order actor message mismatch to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&bad_compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `String`, but the argument provides `i64`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "imported higher-order actor mismatch missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_actor_factory_targets_work() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let actors = temp_dir.path().join("actors.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &actors,
        r#"fn make_string_worker() {
  spawn { receive { "go" -> dbg("import sent") } }
}

fn make_idle_worker() {
  spawn { receive { () -> () } }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Actors

type monitor_down = Down(actor_id<_>)

fn main() {
  send(Actors.make_string_worker(), "go");
  spawn {
    monitor(Actors.make_idle_worker());
    dbg("import monitored");
    receive { Down(_) -> () }
  };
  spawn {
    link(Actors.make_idle_worker());
    dbg("import linked");
    receive { () -> () }
  };
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&actors)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected actor factory compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&actors)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected actor factory emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "fn make_string_worker() -> actor_id<String>",
        "fn make_idle_worker() -> actor_id<unit>",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "actor factory rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported actor factory compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"import sent\nimport monitored\nimport linked\n" {
        return fail(format!(
            "unexpected imported actor factory stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_actor_factory_message_mismatch() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let actors = temp_dir.path().join("actors.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &actors,
        r#"fn make_string_worker() {
  spawn { receive { "go" -> () } }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Actors
fn main() { send(Actors.make_string_worker(), 1) }
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&actors)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected actor factory compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if compile.status.success() {
        return fail("expected imported actor message mismatch to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `String`, but the argument provides `i64`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "imported actor message mismatch missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_heterogeneous_actor_factory_keeps_unknown_message_signature() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let actors = temp_dir.path().join("actors.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &actors,
        r#"fn make_any_worker() {
  spawn {
    receive { "go" -> dbg("import any string") };
    receive { 1 -> dbg("import any int") }
  }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Actors

fn main() {
  let worker = Actors.make_any_worker();
  send(worker, "go");
  send(worker, 1);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&actors)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected heterogeneous actor factory compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&actors)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected heterogeneous actor factory emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    if !stdout.contains("fn make_any_worker() -> actor_id<'") {
        return fail(format!(
            "heterogeneous actor factory rsig should keep an unknown message type:\n{stdout}"
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported heterogeneous actor factory compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout != b"import any string\nimport any int\n" {
        return fail(format!(
            "unexpected imported heterogeneous actor factory stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_actor_record_fields_preserve_message_types() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let actors = temp_dir.path().join("actor_boxes.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &actors,
        r#"type actor_box<'msg> = { worker: actor_id<'msg> }

fn make_sender() {
  spawn { receive { "go" -> dbg("imported record concrete") } }
}

fn make_any() {
  spawn {
    receive { "go" -> dbg("imported record unknown string") };
    receive { 1 -> dbg("imported record unknown i64") }
  }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use ActorBoxes

fn main() {
  let concrete = ActorBoxes.actor_box { worker: ActorBoxes.make_sender() };
  send(concrete.worker, "go");
  let any = ActorBoxes.actor_box { worker: ActorBoxes.make_any() };
  send(any.worker, "go");
  send(any.worker, 1);
  ()
}
"#,
    )?;
    std::fs::write(
        &bad,
        r#"use ActorBoxes

fn main() {
  let concrete = ActorBoxes.actor_box { worker: ActorBoxes.make_sender() };
  send(concrete.worker, 1);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&actors)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected imported actor record compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&actors)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected imported actor record emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "type actor_box<'msg> = { worker: actor_id<'msg> }",
        "fn make_sender() -> actor_id<String>",
        "fn make_any() -> actor_id<'",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "imported actor record rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported actor record compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout
        != b"imported record concrete\nimported record unknown string\nimported record unknown i64\n"
    {
        return fail(format!(
            "unexpected imported actor record stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let compile_bad = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad"))
        .output()?;
    if compile_bad.status.success() {
        return fail("expected imported actor record mismatch to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&compile_bad.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `String`, but the argument provides `i64`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "imported actor record mismatch missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_actor_variant_payloads_preserve_message_types() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let actors = temp_dir.path().join("actor_boxes.ml");
    let main = temp_dir.path().join("main.ml");
    let bad = temp_dir.path().join("bad.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &actors,
        r#"type actor_envelope<'msg> = Worker(actor_id<'msg>)

fn make_sender() {
  spawn { receive { "go" -> dbg("imported variant concrete") } }
}

fn make_any() {
  spawn {
    receive { "go" -> dbg("imported variant unknown string") };
    receive { 1 -> dbg("imported variant unknown i64") }
  }
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use ActorBoxes

fn main() {
  let concrete = ActorBoxes.Worker(ActorBoxes.make_sender());
  match concrete { ActorBoxes.Worker(worker) -> send(worker, "go") };
  let any = ActorBoxes.Worker(ActorBoxes.make_any());
  match any { ActorBoxes.Worker(worker) -> send(worker, "go") };
  match any { ActorBoxes.Worker(worker) -> send(worker, 1) };
  dbg("done")
}
"#,
    )?;
    std::fs::write(
        &bad,
        r#"use ActorBoxes

fn main() {
  let concrete = ActorBoxes.Worker(ActorBoxes.make_sender());
  match concrete { ActorBoxes.Worker(worker) -> send(worker, 1) };
  dbg("done")
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&actors)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected imported actor variant compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&actors)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected imported actor variant emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    for expected in [
        "type actor_envelope<'msg> = Worker(actor_id<'msg>)",
        "fn make_sender() -> actor_id<String>",
        "fn make_any() -> actor_id<'",
    ] {
        if !stdout.contains(expected) {
            return fail(format!(
                "imported actor variant rsig missed `{expected}`:\n{stdout}"
            ));
        }
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported actor variant compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&output).output()?;
    if run.stdout
        != b"done\nimported variant concrete\nimported variant unknown string\nimported variant unknown i64\n"
    {
        return fail(format!(
            "unexpected imported actor variant stdout:\n{}",
            String::from_utf8_lossy(&run.stdout)
        ));
    }

    let compile_bad = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&bad)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(temp_dir.path().join("bad"))
        .output()?;
    if compile_bad.status.success() {
        return fail("expected imported actor variant mismatch to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&compile_bad.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `String`, but the argument provides `i64`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "imported actor variant mismatch missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_actor_id_unknown_params_have_concrete_abi() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let helpers = temp_dir.path().join("helpers.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &helpers,
        r#"fn observe(worker: actor_id<_>) {
  monitor(worker);
  link(worker);
  ()
}
"#,
    )?;
    std::fs::write(
        &main,
        r#"use Helpers

fn main() {
  let worker = spawn {
    receive { "go" -> dbg("helper string") };
    receive { 1 -> dbg("helper int") }
  };
  Helpers.observe(worker);
  send(worker, "go");
  send(worker, 1);
  ()
}
"#,
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&helpers)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected actor-id helper compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let emit = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("emit")
        .arg("all")
        .arg(&helpers)
        .output()?;
    if !emit.status.success() {
        return fail(format!(
            "expected actor-id helper emit all to succeed:\n{}",
            String::from_utf8_lossy(&emit.stderr)
        ));
    }
    let stdout = String::from_utf8_lossy(&emit.stdout);
    if !stdout.contains("fn observe(actor_id<'") {
        return fail(format!(
            "actor-id helper rsig should expose an unknown actor message parameter:\n{stdout}"
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if !compile.status.success() {
        return fail(format!(
            "expected imported actor-id helper compile to succeed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    Ok(())
}

#[test]
fn compile_lib_imported_actor_id_unknown_param_rejects_non_actor() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let helpers = temp_dir.path().join("helpers.ml");
    let main = temp_dir.path().join("main.ml");
    let output = temp_dir.path().join("main");

    std::fs::write(
        &helpers,
        r#"fn observe(worker: actor_id<_>) {
  monitor(worker);
  ()
}
"#,
    )?;
    std::fs::write(
        &main,
        "use Helpers\nfn main() { let _ignored = Helpers.observe(42); dbg(\"done\") }\n",
    )?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&helpers)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected actor-id helper compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    let compile = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(&main)
        .arg("--sig-dir")
        .arg(&out_dir)
        .arg("--object-dir")
        .arg(&out_dir)
        .arg("-o")
        .arg(&output)
        .output()?;
    if compile.status.success() {
        return fail("expected imported actor-id helper argument mismatch to fail".to_string());
    }
    let stderr = String::from_utf8_lossy(&compile.stderr);
    for expected in [
        "function argument type does not match",
        "this call requires `actor_id<'",
        "but the argument provides `i64`",
    ] {
        if !stderr.contains(expected) {
            return fail(format!(
                "imported actor-id helper argument mismatch missed `{expected}`:\n{stderr}"
            ));
        }
    }

    Ok(())
}

#[test]
fn compile_lib_imported_actor_operation_rejects_non_actor_factories() -> FixtureResult {
    let temp_dir = TempDir::new()?;
    let out_dir = temp_dir.path().join("build");
    let values = temp_dir.path().join("values.ml");

    std::fs::write(&values, "fn answer() { 42 }\n")?;

    let compile_lib = Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile-lib")
        .arg(&values)
        .arg("--out-dir")
        .arg(&out_dir)
        .output()?;
    if !compile_lib.status.success() {
        return fail(format!(
            "expected value factory compile-lib to succeed:\n{}",
            String::from_utf8_lossy(&compile_lib.stderr)
        ));
    }

    for operation in ["monitor", "link"] {
        let main = temp_dir.path().join(format!("{operation}.ml"));
        let output = temp_dir.path().join(operation);
        std::fs::write(
            &main,
            format!("use Values\nfn main() {{ {operation}(Values.answer()) }}\n"),
        )?;

        let compile = Command::new(cargo_bin("stage0"))
            .current_dir(manifest_dir())
            .arg("compile")
            .arg(&main)
            .arg("--sig-dir")
            .arg(&out_dir)
            .arg("--object-dir")
            .arg(&out_dir)
            .arg("-o")
            .arg(&output)
            .output()?;
        if compile.status.success() {
            return fail(format!("expected imported {operation} target mismatch to fail"));
        }
        let stderr = String::from_utf8_lossy(&compile.stderr);
        for expected in [
            "function argument type does not match",
            "this call requires `actor_id<'",
            "but the argument provides `i64`",
        ] {
            if !stderr.contains(expected) {
                return fail(format!(
                    "imported {operation} target mismatch missed `{expected}`:\n{stderr}"
                ));
            }
        }
    }

    Ok(())
}

#[test]
fn emit_actor_ir_snapshots_frame_contracts() -> FixtureResult {
    for (name, fixture) in [
        (
            "actor_ir__fibonacci_worker",
            "tests/fixtures/programs/actors/fibonacci_worker.ml",
        ),
        (
            "actor_ir__spawn_chain_two_hops",
            "tests/fixtures/programs/actors/spawn_chain_two_hops.ml",
        ),
    ] {
        let fixture = manifest_dir().join(fixture);
        let emit = Command::new(cargo_bin("stage0"))
            .current_dir(manifest_dir())
            .arg("emit")
            .arg("actor-ir")
            .arg(&fixture)
            .output()?;
        if !emit.status.success() {
            return fail(format!(
                "expected actor-ir emission to succeed for {}:\n{}",
                fixture.display(),
                String::from_utf8_lossy(&emit.stderr)
            ));
        }
        let stdout = String::from_utf8_lossy(&emit.stdout);
        for needle in [
            "ActorIrActor",
            "ActorFrameLayout",
            "captures",
            "states",
            "Receive",
        ] {
            if !stdout.contains(needle) {
                return fail(format!(
                    "actor-ir output for {} did not contain `{needle}`:\n{stdout}",
                    fixture.display()
                ));
            }
        }
        assert_named_snapshot(name, &stdout);
    }

    Ok(())
}

fn ensure_release_runtime_built() -> std::io::Result<()> {
    static RELEASE_RUNTIME: OnceLock<Result<(), String>> = OnceLock::new();
    match RELEASE_RUNTIME.get_or_init(|| {
        let output = Command::new("cargo")
            .current_dir(manifest_dir())
            .arg("build")
            .arg("--release")
            .arg("--manifest-path")
            .arg("../rt/Cargo.toml")
            .output()
            .map_err(|error| format!("failed to start release runtime build: {error}"))?;
        if output.status.success() {
            Ok(())
        } else {
            Err(format!(
                "failed to build release runtime before fixture compile:\n{}",
                String::from_utf8_lossy(&output.stderr)
            ))
        }
    }) {
        Ok(()) => Ok(()),
        Err(message) => Err(std::io::Error::other(message.clone())),
    }
}

fn compile_fixture(fixture: &Path, output_path: &Path) -> std::io::Result<Output> {
    Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
        .arg("compile")
        .arg(fixture)
        .arg("-o")
        .arg(output_path)
        .output()
}

struct Transcript {
    contents: String,
}

impl Transcript {
    fn new(fixture: &Path) -> Self {
        let mut contents = String::new();
        contents.push_str("fixture: ");
        contents.push_str(&relative_fixture(fixture));
        contents.push('\n');
        Self { contents }
    }

    fn push_output(&mut self, label: &str, output: &Output) {
        self.contents.push('\n');
        self.contents.push_str(label);
        self.contents.push_str(":\n");
        self.contents.push_str("status: ");
        self.contents.push_str(&status_label(output));
        self.contents.push('\n');
        self.push_bytes("stdout", &output.stdout);
        self.push_bytes("stderr", &output.stderr);
    }

    fn push_bytes(&mut self, label: &str, bytes: &[u8]) {
        self.contents.push_str(label);
        self.contents.push_str(":\n");

        if bytes.is_empty() {
            self.contents.push_str("<empty>\n");
        } else {
            self.contents.push_str(&String::from_utf8_lossy(bytes));
            if !bytes.ends_with(b"\n") {
                self.contents.push('\n');
            }
        }
    }

    fn finish(self) -> String {
        self.contents
    }
}

fn assert_fixture_snapshot(kind: &str, fixture: &Path, transcript: &str) {
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(manifest_dir().join("tests/snapshots"));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        insta::assert_snapshot!(snapshot_name(kind, fixture), transcript);
    });
}

fn assert_named_snapshot(name: &str, contents: &str) {
    let mut settings = insta::Settings::clone_current();
    settings.set_snapshot_path(manifest_dir().join("tests/snapshots"));
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| {
        insta::assert_snapshot!(name, contents);
    });
}

fn status_label(output: &Output) -> String {
    if output.status.success() {
        "success".to_owned()
    } else {
        match output.status.code() {
            Some(code) => format!("failure({code})"),
            None => "failure".to_owned(),
        }
    }
}

fn relative_fixture(fixture: &Path) -> String {
    fixture.to_string_lossy().replace('\\', "/")
}

fn snapshot_name(kind: &str, fixture: &Path) -> String {
    let path = relative_fixture(fixture);
    let stem = path
        .strip_suffix(".ml")
        .unwrap_or(&path)
        .chars()
        .map(|ch| match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' => ch,
            _ => '_',
        })
        .collect::<String>();
    format!("{kind}__{stem}")
}

fn manifest_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf()
}

fn fixture_path(fixture: &Path) -> PathBuf {
    if fixture.is_absolute() {
        fixture.to_path_buf()
    } else {
        manifest_dir().join(fixture)
    }
}

fn output_name(fixture: &Path) -> String {
    fixture
        .file_stem()
        .and_then(OsStr::to_str)
        .unwrap_or("fixture")
        .to_owned()
}

fn fail(message: String) -> FixtureResult {
    Err(std::io::Error::other(message).into())
}

include!(concat!(env!("OUT_DIR"), "/fixture_tests.rs"));
