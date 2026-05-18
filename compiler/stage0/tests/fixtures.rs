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

fn compile_fixture(fixture: &Path, output_path: &Path) -> std::io::Result<Output> {
    Command::new(cargo_bin("stage0"))
        .current_dir(manifest_dir())
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
