use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use assert_cmd::cargo::cargo_bin;
use tempfile::TempDir;

type FixtureResult = Result<(), Box<dyn std::error::Error>>;

fn program_fixture_compiles(fixture: &Path) -> FixtureResult {
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
fn emit_llvm_uses_typed_actor_message_senders() -> FixtureResult {
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
    if !stdout.contains("riot_rt_send_bool") {
        return fail(format!(
            "llvm output did not contain typed bool sender:\n{stdout}"
        ));
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
