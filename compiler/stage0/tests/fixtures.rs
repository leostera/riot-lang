use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;

use assert_cmd::cargo::cargo_bin;
use tempfile::TempDir;
use walkdir::WalkDir;

#[test]
fn compiler_program_fixtures_compile() {
    let fixtures = ml_fixtures(&compiler_dir().join("fixtures/programs"));
    assert!(
        !fixtures.is_empty(),
        "expected at least one program fixture"
    );

    for fixture in fixtures {
        let temp_dir = TempDir::new().expect("create temp dir");
        let output = temp_dir.path().join(output_name(&fixture));

        let compile = Command::new(cargo_bin("stage0"))
            .arg(&fixture)
            .arg("-o")
            .arg(&output)
            .output()
            .expect("run stage0");

        assert!(
            compile.status.success(),
            "expected program fixture to compile: {}\nstdout:\n{}\nstderr:\n{}",
            fixture.display(),
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr),
        );

        let expected_stdout = fixture.with_extension("stdout");
        if expected_stdout.exists() {
            let run = Command::new(&output)
                .output()
                .expect("run generated fixture binary");
            assert!(
                run.status.success(),
                "expected generated fixture binary to run: {}\nstdout:\n{}\nstderr:\n{}",
                output.display(),
                String::from_utf8_lossy(&run.stdout),
                String::from_utf8_lossy(&run.stderr),
            );

            let expected = std::fs::read(&expected_stdout).expect("read expected stdout sidecar");
            assert_eq!(
                run.stdout,
                expected,
                "stdout mismatch for {}",
                fixture.display()
            );
        }
    }
}

#[test]
fn compiler_diagnostic_fixtures_fail() {
    let fixtures = ml_fixtures(&compiler_dir().join("fixtures/diagnostics"));
    assert!(
        !fixtures.is_empty(),
        "expected at least one diagnostics fixture"
    );

    for fixture in fixtures {
        let temp_dir = TempDir::new().expect("create temp dir");
        let output = temp_dir.path().join(output_name(&fixture));

        let compile = Command::new(cargo_bin("stage0"))
            .arg(&fixture)
            .arg("-o")
            .arg(&output)
            .output()
            .expect("run stage0");

        assert!(
            !compile.status.success(),
            "expected diagnostics fixture to fail: {}\nstdout:\n{}\nstderr:\n{}",
            fixture.display(),
            String::from_utf8_lossy(&compile.stdout),
            String::from_utf8_lossy(&compile.stderr),
        );
    }
}

fn ml_fixtures(root: &Path) -> Vec<PathBuf> {
    let mut fixtures = WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .map(|entry| entry.into_path())
        .filter(|path| path.extension() == Some(OsStr::new("ml")))
        .collect::<Vec<_>>();
    fixtures.sort();
    fixtures
}

fn compiler_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("stage0 lives under compiler/")
        .to_path_buf()
}

fn output_name(fixture: &Path) -> String {
    fixture
        .file_stem()
        .and_then(OsStr::to_str)
        .unwrap_or("fixture")
        .to_owned()
}
